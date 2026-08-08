library(haven)    # For data manipulation and import/export
library(dplyr)
library(sparklyr) 
library(survey)   # For complex survey data analysis
library(svrep)
library(synthpop) # For synthetic data analysis

# Create an example dataset with weights and SDRM replicates ----

  set.seed(2025)
  
  example_orig_data <- svrep::library_stsys_sample |>
    svydesign(
      data   = _, 
      ids    = ~ 1,
      strata = ~ SAMPLING_STRATUM,
      probs  = ~ SAMPLING_PROB
    ) |>
    as_sdr_design(
      sort_variable       = "SAMPLING_SORT_ORDER",
      replicates          = 80,
      use_normal_hadamard = TRUE
    ) |>
    subset(RESPONSE_STATUS != "Closed") |>
    redistribute_weights(
      reduce_if   = (RESPONSE_STATUS == "Survey Nonrespondent"),
      increase_if = (RESPONSE_STATUS == "Survey Respondent"),
      by          = "SAMPLING_STRATUM"
    ) |>
    as_data_frame_with_weights(
      full_wgt_name  = "FULL_SAMPLE_WGT",
      rep_wgt_prefix = "REP_WGT_" 
    ) |>
    haven::zap_labels()

# Create a synthetic dataset that also has weights and replicate weights ----

    synthetic_data <- synthpop::syn(
      data = example_orig_data |>
        mutate(
          SAMPLING_SORT_ORDER = factor(SAMPLING_SORT_ORDER) |>
            as.numeric()
        ) |>
        select(
          SAMPLING_STRATUM, SAMPLING_SORT_ORDER, SAMPLING_PROB,
          STABR,
          TOTSTAFF, LIBRARIA, 
          TOTOPEXP, TOTINCM,
          TOTCIR, ELMATCIR, PHYSCIR
        ),
      method = "cart", seed = 2025
    ) |> getElement("syn")

    synthetic_data <- synthetic_data |> mutate(
        FSCSKEY = paste0("SYN", 1:nrow(synthetic_data))
      ) |> relocate(FSCSKEY, .before = 1) |>
      svydesign(
        data   = _, 
        ids    = ~ 1,
        strata = ~ SAMPLING_STRATUM,
        probs  = ~ SAMPLING_PROB
      ) |>
      as_sdr_design(
        sort_variable       = "SAMPLING_SORT_ORDER",
        replicates          = 80,
        use_normal_hadamard = TRUE
      ) |>
      as_data_frame_with_weights(
        full_wgt_name  = "FULL_SAMPLE_WGT",
        rep_wgt_prefix = "REP_WGT_" 
      )

# Produce some simple comparisons

  libraries_synth <- synthetic_data |>
    select(-one_of(c("SAMPLING_STRATUM", "SAMPLING_SORT_ORDER",
                     "SAMPLING_PROB")))
  libraries_orig <- example_orig_data |>
    select(all_of(colnames(libraries_synth)))

# Save the datasets ----

  usethis::use_data(libraries_synth, libraries_orig, overwrite = TRUE)
