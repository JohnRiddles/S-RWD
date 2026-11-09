library(tidyverse)
library(duckplyr)
source("palantir-foundry-helper-functions.R")

# Define a function that prepares the analysis data ----

prepare_data <- function(version) {
  
  ## Load datasets of interest ----
  ## Primarily datasets created based on the Logic Liaison Template (LLT),
  ## including patient and patient-day fact tables.
  ## Also some underlying tables used in the construction of fact tables
  stopifnot(is.character(version) && length(version) == 1)
  version <- tolower(version)
  stopifnot(!is.na(version) && (version %in% c("original", "synthetic")))
  
  message("Loading data")
  
  if (version == "original") {
    all_patients_fact_day_table_de_id <- connect_to_table("all_patients_fact_day_table_de_id")
    all_patients_summary_fact_table_de_id <- connect_to_table("all_patients_summary_fact_table_de_id")
    copd_cohort_hospitalization_macrovisits <- connect_to_table("copd_cohort_hospitalization_macrovisits")
  }
  if (version == "synthetic") {
    all_patients_fact_day_table_de_id <- connect_to_table("synthetic_all_patients_fact_day_table_de_id")
    all_patients_summary_fact_table_de_id <- connect_to_table("synthetic_all_patients_summary_fact_table_de_id")
    copd_cohort_hospitalization_macrovisits <- connect_to_table("synthetic_copd_cohort_hospitalization_macrovisits")
  }
  
  ## Initialize counts of patients at each step of data processing ----
  
  count_n_patients <- function(tbl) {
    tbl |>
      summarize(n = n_distinct(person_id)) |>
      collect() |>
      pull("n")
  }
  
  n_patients_by_step <- c(
    'Initial' = all_patients_summary_fact_table_de_id |> 
      count_n_patients()
  )
  
  ## Patients with non-missing data for basic demographic information ----
  
  message("Subsetting to patients with non-missing demographic data")
  
  patient_covariates <- c("sex", "age", "race_ethnicity")
  
  patients_with_nonmissing_demographics <- all_patients_summary_fact_table_de_id |> 
    filter(toupper(sex) %in% c("MALE", "FEMALE")) |>
    filter(tolower(race_ethnicity) != "unknown")
  
  n_patients_by_step['Non-missing demographics'] <- patients_with_nonmissing_demographics |>
    count_n_patients()
  
  ## Hospitalizations lasting at least one day ----
  
  message("Subsetting to hospitalizations lasting at least one day")
  
  sufficiently_long_hospitalizations <- copd_cohort_hospitalization_macrovisits |>
    distinct(
      person_id, combined_macrovisit_id,
      combined_macrovisit_start_date, combined_macrovisit_end_date
    ) |>
    filter(
      combined_macrovisit_start_date < combined_macrovisit_end_date
    )
  
  n_patients_by_step['Sufficiently long hospitalizations'] <- semi_join(
    patients_with_nonmissing_demographics,
    sufficiently_long_hospitalizations |> distinct(person_id),
    by = "person_id"
  ) |> count_n_patients()
  
  ## Identify hospitalizations with ARI diagnoses ----
  
  message("Subsetting to hospitalizations with an ARI diagnosis")
  
  relevant_patient_day_facts <- semi_join(
    x = all_patients_fact_day_table_de_id,
    y = patients_with_nonmissing_demographics,
    by = c("person_id")
  )
  
  has_ari_diagnosis <- rlang::expr(
    (!!sym("INFLUENZAINFECTION") == 1) |
      (!!sym("LL_COVID_diagnosis") == 1) |
      (!!sym("ARIOTHER") == 1) | (!!sym("ARIUNSPECIFIED") == 1)
  )
  ari_diagnosis_fact_days <- relevant_patient_day_facts |>
    filter(has_ari_diagnosis)
  
  ari_diagnosis_hospitalizations <- sufficiently_long_hospitalizations |>
    semi_join(
      y = ari_diagnosis_fact_days,
      by = join_by(
        person_id == person_id,
        combined_macrovisit_start_date <= date,
        combined_macrovisit_end_date >= date
      )
    )
  
  n_patients_by_step['Has ARI diagnosis hospitalization'] <- semi_join(
    x = patients_with_nonmissing_demographics,
    y = ari_diagnosis_hospitalizations |>
      distinct(person_id),
    by = "person_id"
  ) |> count_n_patients()
  
  ## Subset ARI hospitalizations to those with required tests in test window  -----
  
  message("Subsetting to ARI hospitalizations with required tests in test window")
  
  ari_diag_hosp_test_windows <- ari_diagnosis_hospitalizations |> mutate(
    test_window_start_date = combined_macrovisit_start_date - days(10),
    test_window_end_date   = combined_macrovisit_end_date   + days(3)
  )
  
  relevant_tests <- relevant_patient_day_facts |>
    filter(
      ((INFLUENZA_POS == 1) | (INFLUENZA_NEG == 1)) |
        ((PCR_AG_Pos == 1) | (PCR_AG_Neg == 1))
    ) |>
    inner_join(
      y = ari_diag_hosp_test_windows |>
        select(
          person_id, test_window_start_date, test_window_end_date,
          associated_combined_macrovisit_id = combined_macrovisit_id
        ),
      by = join_by(
        person_id == person_id,
        date >= test_window_start_date,
        date <= test_window_end_date
      )
    )
  
  hospitalization_test_indicators <- relevant_tests |>
    summarize(
      HAS_INFLUENZA_TEST = ifelse(
        any((INFLUENZA_POS == 1) | (INFLUENZA_NEG == 1)),
        1L, 0L
      ),
      HAS_COVID_TEST = ifelse(
        any(
          ((PCR_AG_Pos == 1) | (PCR_AG_Neg == 1))        ),
        1L, 0L
      ),
      POSITIVE_INFLUENZA_TEST = ifelse(any(INFLUENZA_POS == 1), 1L, 0L),
      .by = c(person_id, associated_combined_macrovisit_id)
    ) |>
    rename(combined_macrovisit_id = associated_combined_macrovisit_id)
  
  ari_hospitalizations_with_required_tests <- hospitalization_test_indicators |>
    filter(HAS_INFLUENZA_TEST == 1, HAS_COVID_TEST == 1)
  
  n_patients_by_step['ARI hospitalization has required tests'] <- semi_join(
    x = patients_with_nonmissing_demographics,
    y = ari_hospitalizations_with_required_tests |>
      distinct(person_id),
    by = "person_id"
  ) |> count_n_patients()
  
  ## Identify ARI hospitalizations to exclude based on vaccination date ----
  
  message("Excluding ARI hospitalizations based on vaccination date")
  
  vaccine_exclusion_dates <- ari_diagnosis_hospitalizations |>
    semi_join(
      y = ari_hospitalizations_with_required_tests,
      by = "combined_macrovisit_id"
    ) |>
    mutate(
      vaccine_exclusion_start_date = combined_macrovisit_start_date - days(14),
      vaccine_exclusion_end_date   = combined_macrovisit_start_date
    )
  
  hospitalizations_to_exclude_based_on_vaccine_date <- all_patients_fact_day_table_de_id |>
    filter(INFLUENZAVACCINE == 1) |>
    select(person_id, date) |>
    inner_join(
      y = vaccine_exclusion_dates |>
        select(
          person_id, 
          vaccine_exclusion_start_date, vaccine_exclusion_end_date,
          associated_combined_macrovisit_id = combined_macrovisit_id
        ),
      by = join_by(
        person_id == person_id,
        date >= vaccine_exclusion_start_date,
        date <= vaccine_exclusion_end_date
      )
    ) |>
    rename(combined_macrovisit_id = associated_combined_macrovisit_id) |>
    distinct(combined_macrovisit_id)
  
  ## Identify final set of hospitalizations for analysis ----
  
  hospitalizations_for_analysis <- ari_hospitalizations_with_required_tests |>
    anti_join(
      y = hospitalizations_to_exclude_based_on_vaccine_date,
      by = "combined_macrovisit_id"
    ) |>
    select(person_id, combined_macrovisit_id, POSITIVE_INFLUENZA_TEST)
  
  n_patients_by_step['Not excluded based on vaccine date'] <- semi_join(
    x = patients_with_nonmissing_demographics,
    y = hospitalizations_for_analysis |>
      distinct(person_id),
    by = "person_id"
  ) |> count_n_patients()
  
  ## Create indicator for whether person had vaccination prior to hospitalization ----
  
  message("Creating indicator for whether person had vaccination prior to hospitalization")
  
  vaccination_status <- hospitalizations_for_analysis |>
    left_join(
      y = copd_cohort_hospitalization_macrovisits |>
        distinct(combined_macrovisit_id, combined_macrovisit_start_date),
      by = "combined_macrovisit_id"
    ) |>
    mutate(
      three_months_before_combined_macrovisit_start_date = (
        combined_macrovisit_start_date - days(90)
      )
    ) |>
    left_join(
      y = all_patients_fact_day_table_de_id |>
        filter(INFLUENZAVACCINE == 1) |>
        distinct(person_id, date) |>
        rename(vaccine_date = date),
      by = join_by(
        person_id == person_id,
        combined_macrovisit_start_date > vaccine_date,
        three_months_before_combined_macrovisit_start_date <= vaccine_date
      )
    ) |>
    summarize(
      VACCINATED = ifelse(any(!is.na(vaccine_date)), 1L, 0L),
      .by = combined_macrovisit_id
    )
  
  prior_vaccination_status <- hospitalizations_for_analysis |>
    left_join(
      y = copd_cohort_hospitalization_macrovisits |>
        distinct(combined_macrovisit_id, combined_macrovisit_start_date),
      by = "combined_macrovisit_id"
    ) |>
    mutate(
      tweleve_months_before_combined_macrovisit_start_date = (
        combined_macrovisit_start_date - months(12)
      )
    ) |>
    left_join(
      y = all_patients_fact_day_table_de_id |>
        filter(INFLUENZAVACCINE == 1) |>
        distinct(person_id, date) |>
        rename(vaccine_date = date),
      by = join_by(
        person_id == person_id,
        tweleve_months_before_combined_macrovisit_start_date >= vaccine_date
      )
    ) |>
    summarize(
      PRIOR_VACCINATED = ifelse(any(!is.na(vaccine_date)), 1L, 0L),
      .by = combined_macrovisit_id
    )
  
  ## Assemble analysis dataset ----
  
  message("Combining information into analysis dataset")
  
  analysis_data <- hospitalizations_for_analysis |>
    left_join(
      y = copd_cohort_hospitalization_macrovisits |>
        distinct(combined_macrovisit_id, combined_macrovisit_start_date, combined_macrovisit_end_date),
      by = "combined_macrovisit_id"
    ) |>
    select(
      person_id, combined_macrovisit_id, 
      combined_macrovisit_start_date, combined_macrovisit_end_date,
      POSITIVE_INFLUENZA_TEST
    ) |>
    left_join(
      y = vaccination_status |> 
        select(combined_macrovisit_id, VACCINATED),
      by = "combined_macrovisit_id"
    ) |>
    left_join(
      y = prior_vaccination_status |> 
        select(combined_macrovisit_id, PRIOR_VACCINATED),
      by = "combined_macrovisit_id"
    ) |>
    inner_join(
      y = patients_with_nonmissing_demographics |>
        select(person_id, age, sex, race_ethnicity),
      by = "person_id"
    ) |>
    collect() |>
    rename_with(tolower)
  
  has_expected_n_patients <- (
    count_n_patients(analysis_data) == tail(n_patients_by_step, 1)
  )
  if (!has_expected_n_patients) {
    stop("`analysis_data` does not have the expected counts.")
  }
  
  result <- list(
    'analysis_data' = analysis_data,
    'n_patients_by_step' = n_patients_by_step
  )
  return(result)
}

# Use the function to create original and synthetic analysis data ----

  synthetic_use_case_analysis_data <- prepare_data("synthetic")
  original_use_case_analysis_data <- prepare_data("original")
  
# Extract counts of persons at each processing step ----
  
  data_processing_counts <- data.frame(
    "Step" = names(original_use_case_analysis_data[['n_patients_by_step']]),
    "Original" = original_use_case_analysis_data[['n_patients_by_step']],
    "Synthetic" = synthetic_use_case_analysis_data[['n_patients_by_step']]
  )
  
# Save output datasets ----
  
  original_use_case_analysis_data[['analysis_data']] |>
    datasets.write_table("use_case_analysis_data")

  datasets.write_table(
    synthetic_use_case_analysis_data[['analysis_data']], 
    "synthetic_use_case_analysis_data"
  )
  
  data_processing_counts |>
    datasets.write_table("data_processing_counts")
  