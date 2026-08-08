from pyspark.sql import functions as F
from transforms.api import transform_df, Input, Output

# Users: replace 'dummy_directory' references with appropriate directory paths

@transform_df(
    # specify the directory of existing data frame that was created in foundry
    output=Output(
        "/dummy_directory/copd_patient_deaths"
    ),
    concept_set_members=Input(
        "/dummy_directory/concept_set_members"
    ),
    death=Input(
        "/dummy_directory/COPD Cohort OMOP Tables/death"
    ),
    manifest=Input("/dummy_directory/manifest"),
    visit_occurrence=Input(
        "/dummy_directory/visit_occurrence"
    ),
    person=Input(
        "/dummy_directory/person"
    ),
)

# Description - This node gathers the COPD cohort person data (developed from based on 4/13/2026 LDS data),
# manifest and microvisit_to_macrovisit_lds LDS data as of 4/13/2026, filters the visits table for rows that
# have a discharge_to_concept_id that corresponds with the DECEASED or HOSPICE concept sets and combines these
# records with the patients in the deaths table. Death dates are taken from the deaths table and from the visits
# table if the patient has a discharge_to_concept_id that corresponds with the DECEASED concept set. Death dates
# are prioritized such that we filter to only plausible death dates from the OMOP death table and take a min prior
# to adding in any additional plausible death dates from the OMOP visits table with concept id belonging to the
# DECEASED concept set and take a max for patients who do not already have a death date from the OMOP death table.
# No date is retained for patients who were discharged to hospice.


def copd_patient_deaths(manifest, person, visit_occurrence, death, concept_set_members):

    df = person.select("person_id", "data_partner_id")

    manifest_df = manifest.select("data_partner_id", "run_date").withColumnRenamed(
        "run_date", "data_extraction_date"
    )

    # join in manifest_df information
    df = df.join(manifest_df, "data_partner_id", "inner")

    persons = df.select("person_id", "data_extraction_date")
    death_df = (
        death.select("person_id", "death_date")
        .distinct()
        .join(persons, on="person_id", how="inner")
    )
    visits_df = (
        visit_occurrence.select(
            "person_id", "visit_start_date", "visit_end_date", "discharge_to_concept_id"
        )
        .distinct()
        .join(persons, on="person_id", how="inner")
    )
    concepts_df = concept_set_members.select(
        "concept_set_name", "is_most_recent_version", "concept_id"
    ).where(F.col("is_most_recent_version") == "true")

    # create lists of concept ids to look for in the discharge_to_concept_id column of the visits_df
    death_from_visits_ids = list(
        concepts_df.where(F.col("concept_set_name") == "DECEASED")
        .select("concept_id")
        .toPandas()["concept_id"]
    )
    hospice_from_visits_ids = list(
        concepts_df.where(F.col("concept_set_name") == "HOSPICE")
        .select("concept_id")
        .toPandas()["concept_id"]
    )

    # filter visits table to patient and date rows that have DECEASED that matches list of concept_ids
    death_from_visits_df = (
        visits_df.where(F.col("discharge_to_concept_id").isin(death_from_visits_ids))
        .drop("discharge_to_concept_id")
        .distinct()
    )
    # filter visits table to patient rows that have HOSPICE that matches list of concept_ids
    hospice_from_visits_df = (
        visits_df.drop("visit_start_date", "visit_end_date", "data_extraction_date")
        .where(F.col("discharge_to_concept_id").isin(hospice_from_visits_ids))
        .drop("discharge_to_concept_id")
        .distinct()
    )

    #####################################################################
    ###combine relevant visits sourced deaths with deaths table deaths###
    #####################################################################

    # keep rows where death_date is plausible
    # then take earliest recorded date per person
    death_w_dates_df = (
        death_df.where(
            (F.col("death_date") >= "2017-05-01")
            & (F.col("death_date") < F.col("data_extraction_date"))
        )
        .groupby("person_id")
        .agg(F.min("death_date").alias("death_date"))
        .drop("data_extraction_date")
    )

    # from rows of visit table that have concept_id belonging to DECEASED concept set,
    # create new column visit_death_date that is plauisble visit_end_date when available,
    # and plausible visit_start_date when visit_end_date is Null or not plausible
    # then take latest recorded date per person
    death_from_visits_w_dates_df = (
        death_from_visits_df.withColumn(
            "visit_death_date",
            F.when(
                (F.col("visit_end_date") >= "2017-05-01")
                & (
                    F.col("visit_end_date")
                    < (F.col("data_extraction_date") + (365 * 2))
                ),
                F.col("visit_end_date"),
            )
            .when(
                (F.col("visit_start_date") >= "2017-05-01")
                & (
                    F.col("visit_start_date")
                    < (F.col("data_extraction_date") + (365 * 2))
                ),
                F.col("visit_start_date"),
            )
            .otherwise(None),
        )
        .groupby("person_id")
        .agg(F.max("visit_death_date").alias("visit_death_date"))
        .drop("data_extraction_date")
    )

    # join deaths with dates from both domains
    df = death_w_dates_df.join(
        death_from_visits_w_dates_df, on="person_id", how="outer"
    )
    # prioritize death_dates from the deaths table over from visits table
    df = df.withColumn(
        "date",
        F.when(F.col("death_date").isNotNull(), F.col("death_date")).otherwise(
            F.col("visit_death_date")
        ),
    ).drop("death_date", "visit_death_date")

    # join in patients, without any date, from deaths table
    # join in patients, without any date, from visits table
    # join in patients, without any date, for HOSPICE from visits table
    # inner join with cohort node patients to keep only confirmed covid patients
    df = (
        df.join(death_df.select("person_id"), on="person_id", how="outer")
        .join(death_from_visits_df.select("person_id"), on="person_id", how="outer")
        .join(hospice_from_visits_df, on="person_id", how="outer")
        .join(persons, on="person_id", how="inner")
        .dropDuplicates()
    )
    # flag all patients as having died regardless of date
    df = df.withColumn("patient_death", F.lit(1))

    return df
    # output.write_dataframe(df)
