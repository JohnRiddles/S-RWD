from pyspark.sql import functions as F
from transforms.api import transform_df, Input, Output

output_dir_path = "/dummy_directory"
output_dir_path += "/Utility-Assessment/Use-Case-Data/Synthetic-Use-Case-Data/"
output_path = output_dir_path + "all_patients_summary_fact_table_de_id"


@transform_df(
    Output(output_path),
    all_patients_fact_day_table_de_id=Input(output_dir_path + "all_patients_fact_day_table_de_id"),
    copd_cohort_de_id=Input(output_dir_path + "copd_cohort_de_id"),
    copd_cohort_patient_deaths=Input(output_dir_path + "copd_cohort_patient_deaths"),
)
# all_patients_summary_fact_table_de_id (0698a3f9-ba93-4f37-bc79-51fe962cd8fd): v12
# Purpose - The purpose of this pipeline is to produce a day level and a persons level fact table for all patients in the N3C enclave.
# Creator/Owner/contact - Andrea Zhou
# Last Update - 12/7/22
# Description - The final step is to aggregate information to create a data frame that contains a single row of data for each patient in the cohort.  This node aggregates all information from the cohort_all_facts_table and summarizes each patient's facts in a single row.

def all_patients_summary_fact_table_de_id(
    all_patients_fact_day_table_de_id, copd_cohort_de_id, copd_cohort_patient_deaths
):
    deaths_df = copd_cohort_patient_deaths.select("person_id", "patient_death")
    df = all_patients_fact_day_table_de_id.drop(
        "patient_death_at_visit", "during_macrovisit_hospitalization", "macrovisit_start_date", "macrovisit_end_date"
    )

    df = df.groupby("person_id").agg(
        F.max("BMI_rounded").alias("BMI_max_observed_or_calculated"),
        *[
            F.max(col).alias(col + "_indicator")
            for col in df.columns
            if col not in ("person_id", "BMI_rounded", "date")
        ],
    )

    # columns to indicate whether a patient belongs in confirmed or possible subcohorts
    df = df.withColumn(
        "confirmed_covid_patient",
        F.when((F.col("LL_COVID_diagnosis_indicator") == 1) | (F.col("PCR_AG_Pos_indicator") == 1), 1).otherwise(0),
    )

    df = df.withColumn(
        "possible_covid_patient",
        F.when(F.col("confirmed_covid_patient") == 1, 0)
        .when(F.col("Antibody_Pos_indicator") == 1, 1)
        # .when(F.col('LL_Long_COVID_diagnosis_indicator') == 1, 1)
        # .when(F.col('LL_Long_COVID_clinic_visit_indicator') == 1, 1)
        # .when(F.col('LL_PNEUMONIADUETOCOVID_indicator') == 1, 1)
        # .when(F.col('LL_MISC_indicator') == 1, 1)
        # .when(F.col('LL_SUSPECTEDCOVID19_indicator') == 1, 1)
        .otherwise(0),
    )

    # join above tables on patient ID
    df = df.join(deaths_df, "person_id", "left").withColumnRenamed("patient_death", "patient_death_indicator")
    df = copd_cohort_de_id.join(df, "person_id", "left")

    # final fill of null in non-continuous variables with 0
    df = df.na.fill(
        value=0,
        subset=[col for col in df.columns if col not in ("BMI_max_observed_or_calculated", "postal_code", "age")],
    )

    return df
