from pyspark.sql import functions as F
from transforms.api import transform_df, Input, Output

# Users: replace 'dummy_directory' references with appropriate directory paths

@transform_df(
    # specify the directory of existing data frame that was created in foundry
    output=Output(
        "/dummy_directory/copd_patient_deaths_s"
    ),
    concept_set_members=Input(
        "/dummy_directory/concept_set_members"
    ),
    death=Input(
        "/dummy_directory/copd_omop_death_ds"
    ),
    visit_occurrence=Input(
        "/dummy_directory/copd_omop_visit_occurrence_ds"
    ),
    person=Input(
        "/dummy_directory/copd_omop_person_ds"
    ),
)

# Description - This node gathers the COPD cohort person data (developed from based on 4/13/2026 LDS data),
# manifest is an N3C-specific metadata table that tracks information about each data partner site contributing
# data to the enclave. he run_date (which your script renames to data_extraction_date) is used as a
# plausibility cutoff — you filter out death dates or visit dates that fall after the data extraction date,
# since those would be impossible or erroneous records.
# Since we are working with synthetic data (not multi-site real LDS data), a single hardcoded date is perfectly reasonable — there are no per-site differences to account for.


def copd_patient_deaths_s(person, visit_occurrence, death, concept_set_members):

    persons = person.select("person_id")

    # manifest_df = manifest \
    #    .select('data_partner_id','run_date') \
    #    .withColumnRenamed("run_date", "data_extraction_date")

    # join in manifest_df information
    # df = df.join(manifest_df, 'data_partner_id','inner')

    death_df = (
        death.select("person_id", "death_date")
        .distinct()
        .join(persons, on="person_id", how="inner")
    )
    visits_df = (
        visit_occurrence.select(
            "person_id",
            "visit_start_date",
            "visit_end_date",
            "discharged_to_concept_id",
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
        visits_df.where(F.col("discharged_to_concept_id").isin(death_from_visits_ids))
        .drop("discharged_to_concept_id")
        .distinct()
    )
    # filter visits table to patient rows that have HOSPICE that matches list of concept_ids
    hospice_from_visits_df = (
        visits_df.drop("visit_start_date", "visit_end_date")
        .where(F.col("discharged_to_concept_id").isin(hospice_from_visits_ids))
        .drop("discharged_to_concept_id")
        .distinct()
    )

    #####################################################################
    ###combine relevant visits sourced deaths with deaths table deaths###
    #####################################################################

    # keep rows where death_date is plausible
    # then take earliest recorded date per person
    death_w_dates_df = (
        death_df.where(
            (F.col("death_date") >= "2017-05-01") & (F.col("death_date") < "2026-06-30")
        )
        .groupby("person_id")
        .agg(F.min("death_date").alias("death_date"))
    )
    # .drop('data_extraction_date')

    # from rows of visit table that have concept_id belonging to DECEASED concept set,
    # create new column visit_death_date that is plauisble visit_end_date when available,
    # and plausible visit_start_date when visit_end_date is Null or not plausible
    # then take latest recorded date per person
    death_from_visits_w_dates_df = (
        death_from_visits_df.withColumn(
            "visit_death_date",
            F.when(
                (F.col("visit_end_date") >= "2017-05-01")
                & (F.col("visit_end_date") < "2026-06-30"),
                F.col("visit_end_date"),
            )
            .when(
                (F.col("visit_start_date") >= "2017-05-01")
                & (F.col("visit_start_date") < "2026-06-30"),
                F.col("visit_start_date"),
            )
            .otherwise(None),
        )
        .groupby("person_id")
        .agg(F.max("visit_death_date").alias("visit_death_date"))
    )
    # .drop('data_extraction_date')

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
