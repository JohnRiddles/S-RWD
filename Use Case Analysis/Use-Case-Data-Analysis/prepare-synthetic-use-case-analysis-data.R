library(tidyverse)
library(duckplyr)
source("palantir-foundry-helper-functions.R")

# Load datasets of interest ----

  ## Load datasets created based on the Logic Liaison Template (LLT),
  ## including patient and patient-day fact tables,
  ## and some underlying tables used in the construction of fact tables
  all_patients_fact_day_table_de_id <- connect_to_table("synthetic_all_patients_fact_day_table_de_id")
  all_patients_summary_fact_table_de_id <- connect_to_table("synthetic_all_patients_summary_fact_table_de_id")
  copd_cohort_hospitalization_macrovisits <- connect_to_table("synthetic_copd_cohort_hospitalization_macrovisits")
  visit_occurrence <- connect_to_table("synthetic_copd_cohort_visit_occurrence")

# Patients with non-missing data for basic demographic information ----

  patient_covariates <- c("sex", "age", "race_ethnicity")
  
  patients_with_nonmissing_demographics <- all_patients_summary_fact_table_de_id |> 
    filter(toupper(sex) %in% c("MALE", "FEMALE")) |>
    filter(tolower(race_ethnicity) != "unknown")
  
# Hospitalizations lasting at least one day ----
  
  sufficiently_long_hospitalizations <- copd_cohort_hospitalization_macrovisits |>
    distinct(
      person_id, combined_macrovisit_id,
      combined_macrovisit_start_date, combined_macrovisit_end_date
    ) |>
    filter(
      combined_macrovisit_start_date < combined_macrovisit_end_date
    )
  
# Identify hospitalizations with ARI diagnoses ----
  
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
  
# Subset ARI hospitalizations to those with required tests in test window  -----
  
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
  
# Idnetify ARI hospitalizations to exclude based on vaccination date ----
  
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
  
# Identify final set of hospitalizations for analysis ----
  
  hospitalizations_for_analysis <- ari_hospitalizations_with_required_tests |>
    anti_join(
      y = hospitalizations_to_exclude_based_on_vaccine_date,
      by = "combined_macrovisit_id"
    ) |>
    select(person_id, combined_macrovisit_id, POSITIVE_INFLUENZA_TEST)
  
# Create indicator for whether person had vaccination prior to hospitalization ----
    
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
  
# Assemble analysis dataset ----
  
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
      y = patients_with_nonmissing_demographics |>
        select(person_id, age, sex, race_ethnicity),
      by = "person_id"
    ) |>
    collect() |>
    rename_with(tolower)

# Save the analysis data ----
  
  datasets.write_table(analysis_data, "synthetic_use_case_analysis_data")
  