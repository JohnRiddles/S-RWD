from pyspark.sql import functions as F
from transforms.api import transform_df, Input, Output

output_dir_path = "/dummy_directory"
output_dir_path += "/Utility-Assessment/Use-Case-Data/"
output_path = output_dir_path + "all_patients_fact_day_table_de_id"


# Must run preceding step before building this list of inputs
@transform_df(
    Output(output_path),
    copd_cohort_conditions_of_interest=Input("ri.foundry.main.dataset.dummyid"),
    copd_cohort_measurements_of_interest=Input("ri.foundry.main.dataset.dummyid"),
    copd_cohort_procedures_of_interest=Input("ri.foundry.main.dataset.dummyid"),
    copd_cohort_observations_of_interest=Input("ri.foundry.main.dataset.dummyid"),
    copd_cohort_drugs_of_interest=Input("ri.foundry.main.dataset.dummyid"),
    copd_cohort_patient_deaths=Input("ri.foundry.main.dataset.dummyid"),
    copd_cohort_devices_of_interest=Input("ri.foundry.main.dataset.dummyid"),
    copd_cohort_hospitalization_macrovisits=Input("ri.foundry.main.dataset.dummyid"),
    visit_occurrence=Input("ri.foundry.main.dataset.dummyid"),
)

# all_patients_fact_day_table_de_id
# Purpose - The purpose of this pipeline is to produce a day level and a persons level fact table for all patients in the N3C enclave.
# Creator/Owner/contact - Andrea Zhou
# Last Update - 12/6/23
# Description - All facts collected in the previous steps are combined in this cohort_all_facts_table on the basis of unique days for each patient. Indicators are created for the presence or absence of events, medications, conditions, measurements, device exposures, observations, procedures, and outcomes.  It also creates an indicator for whether the date where a fact was noted occurred during any hospitalization. This table is useful if the analyst needs to use actual dates of events as it provides more detail than the final patient-level table.  Use the max and min functions to find the first and last occurrences of any events.

def all_patients_fact_day_table_de_id(
    copd_cohort_conditions_of_interest,
    copd_cohort_measurements_of_interest,
    copd_cohort_procedures_of_interest,
    copd_cohort_observations_of_interest,
    copd_cohort_drugs_of_interest,
    copd_cohort_patient_deaths,
    copd_cohort_devices_of_interest,
    copd_cohort_hospitalization_macrovisits,
    visit_occurrence,
):
    visits_df = visit_occurrence
    macrovisits_df = copd_cohort_hospitalization_macrovisits
    # vaccines_df = copd_cohort_vaccines_of_interest
    procedures_df = copd_cohort_procedures_of_interest
    devices_df = copd_cohort_devices_of_interest
    observations_df = copd_cohort_observations_of_interest
    conditions_df = copd_cohort_conditions_of_interest
    drugs_df = copd_cohort_drugs_of_interest
    measurements_df = copd_cohort_measurements_of_interest
    deaths_df = (
        copd_cohort_patient_deaths.where(
            (copd_cohort_patient_deaths.date.isNotNull())
            & (copd_cohort_patient_deaths.date >= "2017-05-01")
            & (copd_cohort_patient_deaths.date < (F.col("data_extraction_date") + (365 * 2)))
        )
        .withColumnRenamed("patient_death", "patient_death_at_visit")
        .drop("data_extraction_date")
    )

    df = visits_df.select("person_id", "visit_start_date").withColumnRenamed("visit_start_date", "date")
    # df = df.join(vaccines_df, on=list(set(df.columns)&set(vaccines_df.columns)), how='outer')
    df = df.join(procedures_df, on=list(set(df.columns) & set(procedures_df.columns)), how="outer")
    df = df.join(devices_df, on=list(set(df.columns) & set(devices_df.columns)), how="outer")
    df = df.join(observations_df, on=list(set(df.columns) & set(observations_df.columns)), how="outer")
    df = df.join(conditions_df, on=list(set(df.columns) & set(conditions_df.columns)), how="outer")
    df = df.join(drugs_df, on=list(set(df.columns) & set(drugs_df.columns)), how="outer")
    df = df.join(measurements_df, on=list(set(df.columns) & set(measurements_df.columns)), how="outer")
    df = df.join(deaths_df, on=list(set(df.columns) & set(deaths_df.columns)), how="outer")

    df = df.na.fill(value=0, subset=[col for col in df.columns if col not in ("BMI_rounded")])

    # add F.max of all indicator columns to collapse all cross-domain flags to unique person and visit rows
    # each date represents the date of the event or fact being noted in the patient's medical record
    df = df.groupby("person_id", "date").agg(
        *[F.max(col).alias(col) for col in df.columns if col not in ("person_id", "date")]
    )

    # final fill of null non-continuous variables with 0
    df = df.na.fill(value=0, subset=[col for col in df.columns if col not in ("BMI_rounded")])

    # Prepare distinct combined macrovisit intervals
    macrovisits_df = copd_cohort_hospitalization_macrovisits.select(
        F.col("person_id").alias("mv_person_id"),
        "combined_macrovisit_id",
        "combined_macrovisit_start_date",
        "combined_macrovisit_end_date",
    ).distinct()

    # Range join: attach combined_macrovisit_id where df.date falls within the interval
    df = df.join(
        macrovisits_df,
        on=(
            (df["person_id"] == macrovisits_df["mv_person_id"])
            & (df["date"] >= macrovisits_df["combined_macrovisit_start_date"])
            & (df["date"] <= macrovisits_df["combined_macrovisit_end_date"])
        ),
        how="left",
    ).drop("mv_person_id")

    return df
