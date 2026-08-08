library(tidyverse)
library(broom)

# Load use case analysis data ----

  use_case_analysis_data <- datasets.read_table("use_case_analysis_data")
  synthetic_use_case_analysis_data <- datasets.read_table("synthetic_use_case_analysis_data")

# Define a function that conducts the analysis for a specified dataset ----
  
  conduct_analysis <- function(analysis_dataset) {
    
    ## Counts of patients and hospitalizations
    sample_size_counts <- analysis_dataset |>
      summarize(
        `Included Patients` = n_distinct(person_id),
        `Included Hospitalizations` = n()
      )
    
    ## Contingency table
    contingency_table <- analysis_dataset |>
      summarize(
        `Vaccinated Cases`      = sum((positive_influenza_test == 1) & (vaccinated == 1)),
        `Vaccinated Controls`   = sum((positive_influenza_test == 0) & (vaccinated == 1)),
        `Unvaccinated Cases`    = sum((positive_influenza_test == 1) & (vaccinated == 0)),
        `Unvaccinated Controls` = sum((positive_influenza_test == 0) & (vaccinated == 0))
      )
    
    ## Unadjusted odds ratio and effectiveness estimates
    
    unadjusted_model_fit <- glm(
      data = analysis_dataset,
      formula = positive_influenza_test ~ vaccinated,
      family = binomial(link = 'logit')
    )
    
    unadj_log_odds <- coef(unadjusted_model_fit)[['vaccinated']]
    unadj_log_odds_se <- sqrt(unadjusted_model_fit |> vcov() |> diag())['vaccinated']
    unadj_log_odds_ci <- unadjusted_model_fit |>
      confint(parm = "vaccinated", level = 0.95)
    
    unadj_odds_ratio <- exp(unadj_log_odds)
    unadj_odds_ratio_se <- unadj_odds_ratio * unadj_log_odds_se
    unadj_odds_ratio_ci <- exp(unadj_log_odds_ci)
    
    unadj_vaccine_effectiveness <- 100*(1-unadj_odds_ratio)
    
    ## Adjusted odds ratio and effectiveness estimates
    
    adjusted_model_fit <- glm(
      data = analysis_dataset,
      formula = positive_influenza_test ~ vaccinated + age + sex + race_ethnicity,
      family = binomial(link = 'logit')
    )
    
    adj_log_odds <- coef(adjusted_model_fit)[['vaccinated']]
    adj_log_odds_se <- sqrt(adjusted_model_fit |> vcov() |> diag())['vaccinated']
    adj_log_odds_ci <- adjusted_model_fit |>
      confint(parm = "vaccinated", level = 0.95)
    
    adj_odds_ratio <- exp(adj_log_odds)
    adj_odds_ratio_se <- adj_odds_ratio * adj_log_odds_se
    adj_odds_ratio_ci <- exp(adj_log_odds_ci)
    
    ## Vaccine effectiveness statistic
    
    vaccine_eff <- 100*(1-adj_odds_ratio)
    vaccine_eff_se <- 100 * adj_odds_ratio_se
    vaccine_eff_ci <- 100 * sort(1-adj_odds_ratio_ci)
    
    ## Collect estimates
    
    collected_estimates <- bind_rows(
      tibble(
        'Statistic'      = "Unadjusted Odds Ratio",
        'Point Estimate' = unadj_odds_ratio,
        'Standard Error' = unadj_odds_ratio_se,
        'CI 95 - Lower'  = unadj_odds_ratio_ci[1],
        'CI 95 - Upper'  = unadj_odds_ratio_ci[2]
      ),
      tibble(
        'Statistic'      = "Adjusted Odds Ratio",
        'Point Estimate' = adj_odds_ratio,
        'Standard Error' = adj_odds_ratio_se,
        'CI 95 - Lower'  = adj_odds_ratio_ci[1],
        'CI 95 - Upper'  = adj_odds_ratio_ci[2]
      ),
      tibble(
        'Statistic'      = "Vaccine Effectiveness",
        'Point Estimate' = vaccine_eff,
        'Standard Error' = vaccine_eff_se,
        'CI 95 - Lower'  = vaccine_eff_ci[1],
        'CI 95 - Upper'  = vaccine_eff_ci[2]
      )
    )
    
    output <- list(
      'Sample Size' = sample_size_counts,
      'Contingency Table' = contingency_table,
      'Estimates' = collected_estimates
    )
    
  }
  
# Conduct the analysis for the original and for the synthetic dataset ----
  
  analysis_results <- list()
  analysis_results[['Original']] <- conduct_analysis(use_case_analysis_data)
  if (nrow(synthetic_use_case_analysis_data) > 0) {
    analysis_results[['Synthetic']] <- conduct_analysis(synthetic_use_case_analysis_data)  
  }
  