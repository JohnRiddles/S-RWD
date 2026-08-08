from pyspark.sql import functions as F
from transforms.api import transform_df, Input, Output

# Users: replace 'dummy_directory' references with appropriate directory paths

@transform_df(
    # specify the directory of existing data frame that was created in foundry
    output=Output(
        "/dummy_directory/copd_person_obesity_s"
    ),
    person=Input(
        "/dummy_directory/copd_omop_person_ds"
    ),
    measurement=Input(
        "/dummy_directory/copd_omop_measurement_ds"
    ),
    concept_set_members=Input(
        "/dummy_directory/concept_set_members"
    ),
)

# Description - This node filters the measurements table for rows that have a measurement_concept_id associated with one of the concept sets described in the data dictionary in the README.  Indicator names for a positive COVID PCR or AG test, negative COVID PCR or AG test, positive COVID antibody test, and negative COVID antibody test are assigned, and the indicators are collapsed to unique instances on the basis of patient and date. It also finds the harmonized value as a number for BMI measurements and collapses these values to unique instances on the basis of patient and date.  Measurement BMI cutoffs included are intended for adults. Analyses focused on pediatric measurements should use different bounds for BMI measurements.


def copd_person_obesity(measurement, concept_set_members, person):

    # bring in only cohort patient ids
    persons = person.select("person_id")

    # filter procedure occurrence table to only cohort patients
    df = (
        measurement.select(
            "person_id",
            "measurement_date",
            "measurement_concept_id",
            "value_as_number",
            "value_as_concept_id",
        )
        .where(F.col("measurement_date").isNotNull())
        .withColumnRenamed("measurement_date", "date")
        .join(persons, "person_id", "inner")
    )

    concepts_df = concept_set_members.select(
        "concept_set_name", "is_most_recent_version", "concept_id"
    ).where(F.col("is_most_recent_version") == "true")

    # Find BMI closest to COVID using both reported/observed BMI and calculated BMI using height and weight.  Cutoffs for reasonable height, weight, and BMI are provided and can be changed by the template user.
    lowest_acceptable_BMI = 10
    highest_acceptable_BMI = 100
    lowest_acceptable_weight = 5  # in kgs
    highest_acceptable_weight = 300  # in kgs
    lowest_acceptable_height = 0.6  # in meters
    highest_acceptable_height = 2.43  # in meters

    bmi_codeset_ids = list(
        concepts_df.where(
            (concepts_df.concept_set_name == "body mass index")
            & (concepts_df.is_most_recent_version == "true")
        )
        .select("concept_id")
        .toPandas()["concept_id"]
    )
    weight_codeset_ids = list(
        concepts_df.where(
            (concepts_df.concept_set_name == "Body weight (LG34372-9 and SNOMED)")
            & (concepts_df.is_most_recent_version == "true")
        )
        .select("concept_id")
        .toPandas()["concept_id"]
    )
    height_codeset_ids = list(
        concepts_df.where(
            (concepts_df.concept_set_name == "Height (LG34373-7 + SNOMED)")
            & (concepts_df.is_most_recent_version == "true")
        )
        .select("concept_id")
        .toPandas()["concept_id"]
    )

    pcr_ag_test_ids = list(
        concepts_df.where(
            (concepts_df.concept_set_name == "ATLAS SARS-CoV-2 rt-PCR and AG")
            & (concepts_df.is_most_recent_version == "true")
        )
        .select("concept_id")
        .toPandas()["concept_id"]
    )
    antibody_test_ids = list(
        concepts_df.where(
            (concepts_df.concept_set_name == "Atlas #818 [N3C] CovidAntibody retry")
            & (concepts_df.is_most_recent_version == "true")
        )
        .select("concept_id")
        .toPandas()["concept_id"]
    )
    covid_positive_measurement_ids = list(
        concepts_df.where(
            (concepts_df.concept_set_name == "ResultPos")
            & (concepts_df.is_most_recent_version == "true")
        )
        .select("concept_id")
        .toPandas()["concept_id"]
    )
    covid_negative_measurement_ids = list(
        concepts_df.where(
            (concepts_df.concept_set_name == "ResultNeg")
            & (concepts_df.is_most_recent_version == "true")
        )
        .select("concept_id")
        .toPandas()["concept_id"]
    )

    # add value columns for rows associated with the above concept sets, but only include BMI or height or weight when in reasonable range
    BMI_df = (
        df.where(F.col("value_as_number").isNotNull())
        .withColumn(
            "Recorded_BMI",
            F.when(
                df.measurement_concept_id.isin(bmi_codeset_ids)
                & df.value_as_number.between(
                    lowest_acceptable_BMI, highest_acceptable_BMI
                ),
                df.value_as_number,
            ).otherwise(0),
        )
        .withColumn(
            "height",
            F.when(
                df.measurement_concept_id.isin(height_codeset_ids)
                & df.value_as_number.between(
                    lowest_acceptable_height, highest_acceptable_height
                ),
                df.value_as_number,
            ).otherwise(0),
        )
        .withColumn(
            "weight",
            F.when(
                df.measurement_concept_id.isin(weight_codeset_ids)
                & df.value_as_number.between(
                    lowest_acceptable_weight, highest_acceptable_weight
                ),
                df.value_as_number,
            ).otherwise(0),
        )
    )

    labs_df = (
        df.withColumn(
            "PCR_AG_Pos",
            F.when(
                df.measurement_concept_id.isin(pcr_ag_test_ids)
                & df.value_as_concept_id.isin(covid_positive_measurement_ids),
                1,
            ).otherwise(0),
        )
        .withColumn(
            "PCR_AG_Neg",
            F.when(
                df.measurement_concept_id.isin(pcr_ag_test_ids)
                & df.value_as_concept_id.isin(covid_negative_measurement_ids),
                1,
            ).otherwise(0),
        )
        .withColumn(
            "Antibody_Pos",
            F.when(
                df.measurement_concept_id.isin(antibody_test_ids)
                & df.value_as_concept_id.isin(covid_positive_measurement_ids),
                1,
            ).otherwise(0),
        )
        .withColumn(
            "Antibody_Neg",
            F.when(
                df.measurement_concept_id.isin(antibody_test_ids)
                & df.value_as_concept_id.isin(covid_negative_measurement_ids),
                1,
            ).otherwise(0),
        )
    )

    # collapse all reasonable values to unique person and visit rows
    BMI_df = BMI_df.groupby("person_id", "date").agg(
        F.max("Recorded_BMI").alias("Recorded_BMI"),
        F.max("height").alias("height"),
        F.max("weight").alias("weight"),
    )
    labs_df = labs_df.groupby("person_id", "date").agg(
        F.max("PCR_AG_Pos").alias("PCR_AG_Pos"),
        F.max("PCR_AG_Neg").alias("PCR_AG_Neg"),
        F.max("Antibody_Pos").alias("Antibody_Pos"),
        F.max("Antibody_Neg").alias("Antibody_Neg"),
    )

    # add a calculated BMI for each visit date when height and weight available.  Note that if only one is available, it will result in zero
    # subsequent filter out rows that would have resulted from unreasonable calculated_BMI being used as best_BMI for the visit
    BMI_df = BMI_df.withColumn(
        "calculated_BMI", (BMI_df.weight / (BMI_df.height * BMI_df.height))
    )
    BMI_df = BMI_df.withColumn(
        "BMI",
        F.when(BMI_df.Recorded_BMI > 0, BMI_df.Recorded_BMI).otherwise(
            BMI_df.calculated_BMI
        ),
    ).select("person_id", "date", "BMI")
    BMI_df = (
        BMI_df.filter(
            (BMI_df.BMI <= highest_acceptable_BMI)
            & (BMI_df.BMI >= lowest_acceptable_BMI)
        )
        .withColumn("BMI_rounded", F.round(BMI_df.BMI))
        .drop("BMI")
    )
    BMI_df = BMI_df.withColumn(
        "OBESITY", F.when(BMI_df.BMI_rounded >= 30, 1).otherwise(0)
    )

    # join BMI_df with labs_df to retain all lab results with only reasonable BMI_rounded and OBESITY flags
    df = labs_df.join(BMI_df, on=["person_id", "date"], how="left")

    df = (
        df.select("person_id", "date", "OBESITY")
        .groupby("person_id")
        .agg(F.max("OBESITY").alias("OBESITY"))
    )

    return df
