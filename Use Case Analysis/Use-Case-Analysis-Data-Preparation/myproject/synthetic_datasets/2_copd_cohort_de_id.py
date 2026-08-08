from transforms.api import transform_df, Input, Output
from pyspark.sql import functions as F
from pyspark.sql.types import IntegerType

output_path = "/dummy_directory"
output_path += "/Utility-Assessment/Use-Case-Data/Synthetic-Use-Case-Data/copd_cohort_de_id"


@transform_df(
    Output(output_path),
    concept_set_members=Input("ri.foundry.main.dataset.dummyid"),
    person=Input("ri.foundry.main.dataset.dummyid"),
    visit_occurrence=Input("ri.foundry.main.dataset.dummyid"),
)

# Purpose - The purpose of this pipeline is to produce a day level and a persons level
# fact table for all patients in the COPD cohort.
# This pipeline is based on the N3C logic liaison template pipeline "2_everyone_cohort_de_id.py"
# whose README can be found at [REDACTED].
# Description - This node gathers some commonly used facts about these patients
# from the "person" and "location" tables, as well as some facts about the patient's
# institution (from the "manifest" table).
# Available age, race, ethnicity, and locations data is gathered at this node.
# The patient’s total number of visits as well as
# the number of days in their observation period is calculated
# from the “microvisits_to_macrovisits” table in this node
# after filtering for plausible visit dates based on a site's latest data_extraction_date.
# These facts will eventually be joined with the final patient-level table
# in the final node.


def copd_cohort_de_id(concept_set_members, person, visit_occurrence):
    """
    Select proportion of enclave patients to use:
    A value of 1.0 indicates the pipeline will use all patients in the persons table.
    A value less than 1.0 takes a random sample of the patients with a value of 0.001
    (for example) representing a 0.1% sample of the persons table will be used.
    """
    use_sampling = False
    proportion_of_patients_to_use = 1.0

    person_sample = (
        person.select(
            "person_id",
            "year_of_birth",
            "month_of_birth",
            "day_of_birth",
            "ethnicity_concept_name",
            "race_concept_name",
            "gender_concept_name",
        )
        .withColumnRenamed("gender_concept_name", "sex")
        .distinct()
    )

    if use_sampling:
        person_sample = person.sample(withReplacement=False, fraction=proportion_of_patients_to_use, seed=111)

    visits_df = visit_occurrence

    # join in manifest_df information
    df = person_sample
    df = df.withColumn(
        "max_num_shift_days",
        F.lit("0"),
    )

    # calculate date of birth for all patients
    df = df.withColumn(
        "new_year_of_birth",
        F.when(F.col("year_of_birth").isNull(), 1).otherwise(F.col("year_of_birth")),
    )
    df = df.withColumn(
        "new_month_of_birth",
        F.when(F.col("month_of_birth").isNull(), 7)
        .when(F.col("month_of_birth") == 0, 7)
        .otherwise(F.col("month_of_birth")),
    )
    df = df.withColumn(
        "new_day_of_birth",
        F.when(F.col("day_of_birth").isNull(), 1).when(F.col("day_of_birth") == 0, 1).otherwise(F.col("day_of_birth")),
    )

    df = df.withColumn(
        "date_of_birth",
        F.concat_ws(
            "-",
            F.col("new_year_of_birth"),
            F.col("new_month_of_birth"),
            F.col("new_day_of_birth"),
        ),
    )
    df = df.withColumn("date_of_birth", F.to_date("date_of_birth", format=None))

    # convert date of birth string to date and apply min and max reasonable birthdate filter parameters, inclusive
    max_shift_as_int = (
        df.withColumn("shift_days_as_int", F.col("max_num_shift_days").cast(IntegerType()))
        .select(F.max("shift_days_as_int"))
        .head()[0]
    )
    min_reasonable_dob = "1902-01-01"
    max_reasonable_dob = F.date_add(F.current_date(), 0)
    df = df.withColumn(
        "date_of_birth",
        F.when(
            F.col("date_of_birth").between(min_reasonable_dob, max_reasonable_dob),
            F.col("date_of_birth"),
        ).otherwise(None),
    )

    df = df.withColumn(
        "age",
        F.floor(F.months_between(max_reasonable_dob, "date_of_birth", roundOff=False) / 12),
    )

    H = ["Hispanic"]
    A = [
        "Asian",
        "Asian Indian",
        "Bangladeshi",
        "Bhutanese",
        "Burmese",
        "Cambodian",
        "Chinese",
        "Filipino",
        "Hmong",
        "Indonesian",
        "Japanese",
        "Korean",
        "Laotian",
        "Malaysian",
        "Maldivian",
        "Nepalese",
        "Okinawan",
        "Pakistani",
        "Singaporean",
        "Sri Lankan",
        "Taiwanese",
        "Thai",
        "Vietnamese",
    ]
    B_AA = [
        "African",
        "African American",
        "Barbadian",
        "Black",
        "Black or African American",
        "Dominica Islander",
        "Haitian",
        "Jamaican",
        "Madagascar",
        "Trinidadian",
        "West Indian",
    ]
    W = ["White"]
    NH_PI = [
        "Melanesian",
        "Micronesian",
        "Native Hawaiian or Other Pacific Islander",
        "Other Pacific Islander",
        "Polynesian",
    ]
    AI_AN = ["American Indian or Alaska Native"]
    O = ["More than one race", "Multiple race", "Multiple races", "Other", "Other Race"]
    U = [
        "Asian or Pacific islander",
        "No Information",
        "No matching concept",
        "Refuse to Answer",
        "Unknown",
        "Unknown racial group",
    ]

    df = df.withColumn(
        "race",
        F.when(F.col("race_concept_name").isin(H), "Hispanic or Latino")
        .when(F.col("race_concept_name").isin(A), "Asian")
        .when(F.col("race_concept_name").isin(B_AA), "Black or African American")
        .when(F.col("race_concept_name").isin(W), "White")
        .when(
            F.col("race_concept_name").isin(NH_PI),
            "Native Hawaiian or Other Pacific Islander",
        )
        .when(F.col("race_concept_name").isin(AI_AN), "American Indian or Alaska Native")
        .when(F.col("race_concept_name").isin(O), "Other")
        .when(F.col("race_concept_name").isin(U), "Unknown")
        .otherwise("Unknown"),
    )

    df = df.withColumn(
        "race_ethnicity",
        F.when(
            F.col("ethnicity_concept_name") == "Hispanic or Latino",
            "Hispanic or Latino Any Race",
        )
        .when(F.col("race_concept_name").isin(H), "Hispanic or Latino Any Race")
        .when(F.col("race_concept_name").isin(A), "Asian Non-Hispanic")
        .when(
            F.col("race_concept_name").isin(B_AA),
            "Black or African American Non-Hispanic",
        )
        .when(F.col("race_concept_name").isin(W), "White Non-Hispanic")
        .when(
            F.col("race_concept_name").isin(NH_PI),
            "Native Hawaiian or Other Pacific Islander Non-Hispanic",
        )
        .when(
            F.col("race_concept_name").isin(AI_AN),
            "American Indian or Alaska Native Non-Hispanic",
        )
        .when(F.col("race_concept_name").isin(O), "Other Non-Hispanic")
        .when(F.col("race_concept_name").isin(U), "Unknown")
        .otherwise("Unknown"),
    )

    # create visit counts/obs period dataframes
    # hosp_visits = visits_df.where(F.col("macrovisit_start_date").isNotNull()) \
    #    .orderBy("visit_start_date") \
    #    .coalesce(1) \
    #    .dropDuplicates(["person_id", "macrovisit_start_date"]) #hospital
    # non_hosp_visits = visits_df.where(F.col("macrovisit_start_date").isNull()) \
    #    .dropDuplicates(["person_id", "visit_start_date"]) #non-hospital
    # visits_df = hosp_visits.union(non_hosp_visits) #join the two

    # total number of visits
    visits_count = visits_df.groupBy("person_id").count().select("person_id", F.col("count").alias("total_visits"))

    # obs period in days
    observation_period = (
        visits_df.groupby("person_id")
        .agg(
            F.max("visit_start_date").alias("pt_max_visit_date"),
            F.min("visit_start_date").alias("pt_min_visit_date"),
        )
        .withColumn("observation_period", F.datediff("pt_max_visit_date", "pt_min_visit_date"))
        .select("person_id", "observation_period")
    )

    # join visit counts/obs periods dataframes with main dataframe
    df = df.join(visits_count, "person_id", "left")
    df = df.join(observation_period, "person_id", "left")

    # LEVEL 2 ONLY
    df = df.withColumn("max_num_shift_days", F.concat(F.col("max_num_shift_days"), F.lit(" + 180"))).withColumn(
        "shift_date_yn", F.lit("Y")
    )

    df = df.select(
        "person_id",
        "total_visits",
        "observation_period",
        "sex",
        "age",
        "race",
        "race_ethnicity",
    )

    return df
