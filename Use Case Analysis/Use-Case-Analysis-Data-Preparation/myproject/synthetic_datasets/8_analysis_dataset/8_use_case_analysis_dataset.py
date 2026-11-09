from pyspark.sql import functions as F
from transforms.api import transform_df, Input, Output
from copy import deepcopy

output_dir_path = "/dummy"
output_dir_path += "/Utility-Assessment/Use-Case-Data/Synthetic-Use-Case-Data/"
input_dir_path = deepcopy(output_dir_path)

output_path = output_dir_path + "use_case_analysis_dataset"

@transform_df(
    Output(output_path),
    all_patients_fact_day_table_de_id=Input(input_dir_path + "all_patients_fact_day_table_de_id"),
    all_patients_summary_fact_table_de_id=Input(input_dir_path + "all_patients_summary_fact_table_de_id"),
    copd_cohort_hospitalization_macrovisits=Input(input_dir_path + "copd_cohort_hospitalization_macrovisits"),
)

def use_case_analysis_dataset(
    all_patients_fact_day_table_de_id, 
    all_patients_summary_fact_table_de_id, 
    copd_cohort_hospitalization_macrovisits
):

     # ── Load data ──────────────────────────────────────────────────────────────
    all_patients_fact_day_df = all_patients_fact_day_table_de_id
    all_patients_summary_fact_df = all_patients_summary_fact_table_de_id
    copd_hospitalizations_df = copd_cohort_hospitalization_macrovisits

    # ── Patients with non-missing demographic data ────────────────────────────
    patients_with_nonmissing_demographics = (
        all_patients_summary_fact_df
        .filter(F.upper(F.col("sex")).isin(["MALE", "FEMALE"]))
        .filter(F.lower(F.col("race_ethnicity")) != "unknown")
    )

    # ── Hospitalizations lasting at least one day ─────────────────────────────
    sufficiently_long_hospitalizations = (
        copd_hospitalizations_df
        .select(
            "person_id",
            "combined_macrovisit_id",
            "combined_macrovisit_start_date",
            "combined_macrovisit_end_date",
        )
        .dropDuplicates()
        .filter(
            F.col("combined_macrovisit_start_date") < F.col("combined_macrovisit_end_date")
        )
    )

    # ── Identify hospitalizations with ARI diagnoses ──────────────────────────
    relevant_patient_day_facts = all_patients_fact_day_df.join(
        patients_with_nonmissing_demographics.select("person_id").dropDuplicates(),
        on="person_id",
        how="left_semi",
    )

    ari_diagnosis_fact_days = relevant_patient_day_facts.filter(
        (F.col("INFLUENZAINFECTION") == 1)
        | (F.col("LL_COVID_diagnosis") == 1)
        | (F.col("ARIOTHER") == 1)
        | (F.col("ARIUNSPECIFIED") == 1)
    )

    ari_diagnosis_hospitalizations = sufficiently_long_hospitalizations.join(
        ari_diagnosis_fact_days.select("person_id", "date"),
        on=(
            (sufficiently_long_hospitalizations["person_id"] == ari_diagnosis_fact_days["person_id"])
            & (sufficiently_long_hospitalizations["combined_macrovisit_start_date"] <= ari_diagnosis_fact_days["date"])
            & (sufficiently_long_hospitalizations["combined_macrovisit_end_date"] >= ari_diagnosis_fact_days["date"])
        ),
        how="left_semi",
    )

    # ── Subset ARI hospitalizations to those with required tests ──────────────
    ari_diag_hosp_test_windows = ari_diagnosis_hospitalizations.withColumn(
        "test_window_start_date",
        F.date_sub(F.col("combined_macrovisit_start_date"), 10),
    ).withColumn(
        "test_window_end_date",
        F.date_add(F.col("combined_macrovisit_end_date"), 3),
    )

    relevant_tests_raw = relevant_patient_day_facts.filter(
        ((F.col("INFLUENZA_POS") == 1) | (F.col("INFLUENZA_NEG") == 1))
        | ((F.col("PCR_AG_Pos") == 1) | (F.col("PCR_AG_Neg") == 1))
    )

    test_window_keys = ari_diag_hosp_test_windows.select(
        F.col("person_id").alias("_hosp_person_id"),
        F.col("test_window_start_date"),
        F.col("test_window_end_date"),
        F.col("combined_macrovisit_id").alias("associated_combined_macrovisit_id"),
    )

    relevant_tests = relevant_tests_raw.join(
        test_window_keys,
        on=(
            (relevant_tests_raw["person_id"] == test_window_keys["_hosp_person_id"])
            & (relevant_tests_raw["date"] >= test_window_keys["test_window_start_date"])
            & (relevant_tests_raw["date"] <= test_window_keys["test_window_end_date"])
        ),
        how="inner",
    ).drop("_hosp_person_id", "test_window_start_date", "test_window_end_date")

    hospitalization_test_indicators = relevant_tests.groupBy(
        "person_id", "associated_combined_macrovisit_id"
    ).agg(
        F.when(
            F.max(
                F.when(
                    (F.col("INFLUENZA_POS") == 1) | (F.col("INFLUENZA_NEG") == 1), 1
                ).otherwise(0)
            ) >= 1,
            F.lit(1),
        ).otherwise(F.lit(0)).alias("HAS_INFLUENZA_TEST"),
        F.when(
            F.max(
                F.when(
                    (F.col("PCR_AG_Pos") == 1) | (F.col("PCR_AG_Neg") == 1), 1
                ).otherwise(0)
            ) >= 1,
            F.lit(1),
        ).otherwise(F.lit(0)).alias("HAS_COVID_TEST"),
        F.when(
            F.max(F.when(F.col("INFLUENZA_POS") == 1, 1).otherwise(0)) >= 1,
            F.lit(1),
        ).otherwise(F.lit(0)).alias("POSITIVE_INFLUENZA_TEST"),
    ).withColumnRenamed("associated_combined_macrovisit_id", "combined_macrovisit_id")

    ari_hospitalizations_with_required_tests = hospitalization_test_indicators.filter(
        (F.col("HAS_INFLUENZA_TEST") == 1) & (F.col("HAS_COVID_TEST") == 1)
    )

    # ── Exclude hospitalizations based on vaccination date ────────────────────
    vaccine_exclusion_dates = ari_diagnosis_hospitalizations.join(
        ari_hospitalizations_with_required_tests.select("combined_macrovisit_id"),
        on="combined_macrovisit_id",
        how="left_semi",
    ).withColumn(
        "vaccine_exclusion_start_date",
        F.date_sub(F.col("combined_macrovisit_start_date"), 14),
    ).withColumn(
        "vaccine_exclusion_end_date",
        F.col("combined_macrovisit_start_date"),
    )

    vaccine_exclusion_keys = vaccine_exclusion_dates.select(
        F.col("person_id").alias("_excl_person_id"),
        F.col("vaccine_exclusion_start_date"),
        F.col("vaccine_exclusion_end_date"),
        F.col("combined_macrovisit_id").alias("associated_combined_macrovisit_id"),
    )

    vaccine_fact_days = all_patients_fact_day_df.filter(
        F.col("INFLUENZAVACCINE") == 1
    ).select("person_id", "date")

    hospitalizations_to_exclude = (
        vaccine_fact_days.join(
            vaccine_exclusion_keys,
            on=(
                (vaccine_fact_days["person_id"] == vaccine_exclusion_keys["_excl_person_id"])
                & (vaccine_fact_days["date"] >= vaccine_exclusion_keys["vaccine_exclusion_start_date"])
                & (vaccine_fact_days["date"] <= vaccine_exclusion_keys["vaccine_exclusion_end_date"])
            ),
            how="inner",
        )
        .withColumnRenamed("associated_combined_macrovisit_id", "combined_macrovisit_id")
        .select("combined_macrovisit_id")
        .dropDuplicates()
    )

    # ── Final set of hospitalizations for analysis ────────────────────────────
    hospitalizations_for_analysis = (
        ari_hospitalizations_with_required_tests.join(
            hospitalizations_to_exclude,
            on="combined_macrovisit_id",
            how="left_anti",
        )
        .select("person_id", "combined_macrovisit_id", "POSITIVE_INFLUENZA_TEST")
    )

    # ── Vaccination status (within 90 days before hospitalization) ────────────
    hosp_with_start = hospitalizations_for_analysis.join(
        copd_hospitalizations_df.select(
            "combined_macrovisit_id", "combined_macrovisit_start_date"
        ).dropDuplicates(),
        on="combined_macrovisit_id",
        how="left",
    ).withColumn(
        "three_months_before_start",
        F.date_sub(F.col("combined_macrovisit_start_date"), 90),
    )

    vaccine_dates_df = (
        all_patients_fact_day_df.filter(F.col("INFLUENZAVACCINE") == 1)
        .select(
            F.col("person_id").alias("_vax_person_id"),
            F.col("date").alias("vaccine_date"),
        )
        .dropDuplicates()
    )

    vaccination_status = (
        hosp_with_start.join(
            vaccine_dates_df,
            on=(
                (hosp_with_start["person_id"] == vaccine_dates_df["_vax_person_id"])
                & (hosp_with_start["combined_macrovisit_start_date"] > vaccine_dates_df["vaccine_date"])
                & (hosp_with_start["three_months_before_start"] <= vaccine_dates_df["vaccine_date"])
            ),
            how="left",
        )
        .groupBy("combined_macrovisit_id")
        .agg(
            F.when(
                F.max(F.col("vaccine_date").isNotNull().cast("int")) >= 1,
                F.lit(1),
            ).otherwise(F.lit(0)).alias("VACCINATED")
        )
    )

    # ── Prior vaccination status (more than 12 months before hospitalization) ─
    hosp_with_start_prior = hospitalizations_for_analysis.join(
        copd_hospitalizations_df.select(
            "combined_macrovisit_id", "combined_macrovisit_start_date"
        ).dropDuplicates(),
        on="combined_macrovisit_id",
        how="left",
    ).withColumn(
        "twelve_months_before_start",
        F.add_months(F.col("combined_macrovisit_start_date"), -12),
    )

    prior_vaccination_status = (
        hosp_with_start_prior.join(
            vaccine_dates_df,
            on=(
                (hosp_with_start_prior["person_id"] == vaccine_dates_df["_vax_person_id"])
                & (hosp_with_start_prior["twelve_months_before_start"] >= vaccine_dates_df["vaccine_date"])
            ),
            how="left",
        )
        .groupBy("combined_macrovisit_id")
        .agg(
            F.when(
                F.max(F.col("vaccine_date").isNotNull().cast("int")) >= 1,
                F.lit(1),
            ).otherwise(F.lit(0)).alias("PRIOR_VACCINATED")
        )
    )

    # ── Assemble analysis dataset ─────────────────────────────────────────────
    analysis_data = (
        hospitalizations_for_analysis
        .join(
            copd_hospitalizations_df.select(
                "combined_macrovisit_id",
                "combined_macrovisit_start_date",
                "combined_macrovisit_end_date",
            ).dropDuplicates(),
            on="combined_macrovisit_id",
            how="left",
        )
        .select(
            "person_id",
            "combined_macrovisit_id",
            "combined_macrovisit_start_date",
            "combined_macrovisit_end_date",
            "POSITIVE_INFLUENZA_TEST",
        )
        .join(
            vaccination_status.select("combined_macrovisit_id", "VACCINATED"),
            on="combined_macrovisit_id",
            how="left",
        )
        .join(
            prior_vaccination_status.select("combined_macrovisit_id", "PRIOR_VACCINATED"),
            on="combined_macrovisit_id",
            how="left",
        )
        .join(
            patients_with_nonmissing_demographics.select(
                "person_id", "age", "sex", "race_ethnicity"
            ),
            on="person_id",
            how="inner",
        )
    )

    # Rename all columns to lowercase
    for col_name in analysis_data.columns:
        if col_name != col_name.lower():
            analysis_data = analysis_data.withColumnRenamed(col_name, col_name.lower())

    return analysis_data