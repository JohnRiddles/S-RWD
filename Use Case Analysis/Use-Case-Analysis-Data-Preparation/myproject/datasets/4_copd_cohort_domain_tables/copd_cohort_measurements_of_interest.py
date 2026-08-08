from pyspark.sql import functions as F
from transforms.api import transform_df, Input, Output

output_path = "/dummy_directory"
output_path += "/Utility-Assessment/Use-Case-Data/copd_cohort_measurements_of_interest"


@transform_df(
    Output(output_path),
    measurement=Input("ri.foundry.main.dataset.dummyid"),
    copd_cohort_de_id=Input("ri.foundry.main.dataset.dummyid"),
    concept_set_members=Input("ri.foundry.main.dataset.dummyid"),
)

# copd_cohort_measurements_of_interest
# Purpose - The purpose of this pipeline is to produce a day level and a persons level fact table for all patients in the N3C enclave.
# Creator/Owner/contact - Andrea Zhou
# Last Update - 12/7/22
# Description - This node filters the measurements table for rows
# that have a measurement_concept_id associated with one of the concept sets
# described in the data dictionary in the README.
# Indicator names for a positive COVID PCR or AG test, negative COVID PCR or AG test,
# positive COVID antibody test, and negative COVID antibody test are assigned,
# and the indicators are collapsed to unique instances on the basis of patient and date.
# It also finds the harmonized value as a number for BMI measurements and collapses
# these values to unique instances on the basis of patient and date.
# Measurement BMI cutoffs included are intended for adults.
# Analyses focused on pediatric measurements should use
# different bounds for BMI measurements.

def copd_cohort_measurements_of_interest(measurement, concept_set_members, copd_cohort_de_id):
    # bring in only cohort patient ids
    persons = copd_cohort_de_id.select("person_id")
    # filter measurement table to only cohort patients
    df = (
        measurement.select(
            "person_id", "measurement_date", "measurement_concept_id", "value_as_number", "value_as_concept_id"
        )
        .where(F.col("measurement_date").isNotNull())
        .withColumnRenamed("measurement_date", "date")
        .join(persons, "person_id", "inner")
    )

    concepts_df = concept_set_members.select("concept_set_name", "is_most_recent_version", "concept_id").where(
        F.col("is_most_recent_version") == "true"
    )

    # Find BMI closest to COVID using both reported/observed BMI and calculated BMI using height and weight.  Cutoffs for reasonable height, weight, and BMI are provided and can be changed by the template user.
    lowest_acceptable_BMI = 10
    highest_acceptable_BMI = 100
    lowest_acceptable_weight = 5  # in kgs
    highest_acceptable_weight = 300  # in kgs
    lowest_acceptable_height = 0.6  # in meters
    highest_acceptable_height = 2.43  # in meters

    bmi_codeset_ids = list(
        concepts_df.where(
            (concepts_df.concept_set_name == "body mass index") & (concepts_df.is_most_recent_version == "true")
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

    # Get concept ids for Covid tests (PCR/AG and Antibody)
    covid_pcr_ag_test_ids = list(
        concepts_df.where(
            (concepts_df.concept_set_name == "ATLAS SARS-CoV-2 rt-PCR and AG")
            & (concepts_df.is_most_recent_version == "true")
        )
        .select("concept_id")
        .toPandas()["concept_id"]
    )
    covid_antibody_test_ids = list(
        concepts_df.where(
            (concepts_df.concept_set_name == "Atlas #818 [N3C] CovidAntibody retry")
            & (concepts_df.is_most_recent_version == "true")
        )
        .select("concept_id")
        .toPandas()["concept_id"]
    )

    # Get concept ids for influenza tests
    influenza_test_ids = list(
        concepts_df.where(
            (concepts_df.concept_set_name == "Influenza Diagnostic Testing")
            & (concepts_df.is_most_recent_version == "true")
        )
        .select("concept_id")
        .toPandas()["concept_id"]
    )

    # Get concept ids for positive and negative test results
    positive_test_measurement_ids = list(
        concepts_df.where(
            (concepts_df.concept_set_name == "ResultPos") & (concepts_df.is_most_recent_version == "true")
        )
        .select("concept_id")
        .toPandas()["concept_id"]
    )
    negative_test_measurement_ids = list(
        concepts_df.where(
            (concepts_df.concept_set_name == "ResultNeg") & (concepts_df.is_most_recent_version == "true")
        )
        .select("concept_id")
        .toPandas()["concept_id"]
    )
    # Ensure list includes a value concept ID specific to one of the flu tests
    negative_test_measurement_ids.append(1261264)
    negative_test_measurement_ids = list(set(negative_test_measurement_ids))

    # add value columns for rows associated with the above concept sets, but only include BMI or height or weight when in reasonable range
    BMI_df = (
        df.where(F.col("value_as_number").isNotNull())
        .withColumn(
            "Recorded_BMI",
            F.when(
                df.measurement_concept_id.isin(bmi_codeset_ids)
                & df.value_as_number.between(lowest_acceptable_BMI, highest_acceptable_BMI),
                df.value_as_number,
            ).otherwise(0),
        )
        .withColumn(
            "height",
            F.when(
                df.measurement_concept_id.isin(height_codeset_ids)
                & df.value_as_number.between(lowest_acceptable_height, highest_acceptable_height),
                df.value_as_number,
            ).otherwise(0),
        )
        .withColumn(
            "weight",
            F.when(
                df.measurement_concept_id.isin(weight_codeset_ids)
                & df.value_as_number.between(lowest_acceptable_weight, highest_acceptable_weight),
                df.value_as_number,
            ).otherwise(0),
        )
    )

    covid_labs_df = (
        df.withColumn(
            "PCR_AG_Pos",
            F.when(
                df.measurement_concept_id.isin(covid_pcr_ag_test_ids)
                & df.value_as_concept_id.isin(positive_test_measurement_ids),
                1,
            ).otherwise(0),
        )
        .withColumn(
            "PCR_AG_Neg",
            F.when(
                df.measurement_concept_id.isin(covid_pcr_ag_test_ids)
                & df.value_as_concept_id.isin(negative_test_measurement_ids),
                1,
            ).otherwise(0),
        )
        .withColumn(
            "Antibody_Pos",
            F.when(
                df.measurement_concept_id.isin(covid_antibody_test_ids)
                & df.value_as_concept_id.isin(positive_test_measurement_ids),
                1,
            ).otherwise(0),
        )
        .withColumn(
            "Antibody_Neg",
            F.when(
                df.measurement_concept_id.isin(covid_antibody_test_ids)
                & df.value_as_concept_id.isin(negative_test_measurement_ids),
                1,
            ).otherwise(0),
        )
    )

    influenza_labs_df = df.withColumn(
        "INFLUENZA_NEG",
        F.when(
            df.measurement_concept_id.isin(influenza_test_ids)
            & df.value_as_concept_id.isin(positive_test_measurement_ids),
            1,
        ).otherwise(0),
    ).withColumn(
        "INFLUENZA_POS",
        F.when(
            df.measurement_concept_id.isin(influenza_test_ids)
            & df.value_as_concept_id.isin(negative_test_measurement_ids),
            1,
        ).otherwise(0),
    )

    # collapse all reasonable values to unique person and visit rows
    BMI_df = BMI_df.groupby("person_id", "date").agg(
        F.max("Recorded_BMI").alias("Recorded_BMI"), F.max("height").alias("height"), F.max("weight").alias("weight")
    )
    # Create unique person-by-date Covid test result indicators
    covid_labs_df = covid_labs_df.groupby("person_id", "date").agg(
        F.max("PCR_AG_Pos").alias("PCR_AG_Pos"),
        F.max("PCR_AG_Neg").alias("PCR_AG_Neg"),
        F.max("Antibody_Pos").alias("Antibody_Pos"),
        F.max("Antibody_Neg").alias("Antibody_Neg"),
    )
    # Create unique person-by-date influenza test result indicators
    # and an indicator for whether the person has a definitive influenza test result
    influenza_labs_df = (
        influenza_labs_df.groupby("person_id", "date")
        .agg(
            F.max("INFLUENZA_POS").alias("INFLUENZA_POS"),
            F.max("INFLUENZA_NEG").alias("INFLUENZA_NEG"),
        )
        .withColumn("HAS_INFLUENZA_TEST_RESULT", F.greatest(F.col("INFLUENZA_POS"), F.col("INFLUENZA_NEG")))
    )

    # add a calculated BMI for each visit date when height and weight available.  Note that if only one is available, it will result in zero
    # subsequent filter out rows that would have resulted from unreasonable calculated_BMI being used as best_BMI for the visit
    BMI_df = BMI_df.withColumn("calculated_BMI", (BMI_df.weight / (BMI_df.height * BMI_df.height)))
    BMI_df = BMI_df.withColumn(
        "BMI", F.when(BMI_df.Recorded_BMI > 0, BMI_df.Recorded_BMI).otherwise(BMI_df.calculated_BMI)
    ).select("person_id", "date", "BMI")
    BMI_df = (
        BMI_df.filter((BMI_df.BMI <= highest_acceptable_BMI) & (BMI_df.BMI >= lowest_acceptable_BMI))
        .withColumn("BMI_rounded", F.round(BMI_df.BMI))
        .drop("BMI")
    )
    BMI_df = BMI_df.withColumn("OBESITY", F.when(BMI_df.BMI_rounded >= 30, 1).otherwise(0))

    # join BMI_df with Covid and influenza labs dataframes to retain all lab results with only reasonable BMI_rounded and OBESITY flags
    output_df = influenza_labs_df.join(covid_labs_df, on=["person_id", "date"], how="left")
    output_df = output_df.join(BMI_df, on=["person_id", "date"], how="left")

    return output_df
