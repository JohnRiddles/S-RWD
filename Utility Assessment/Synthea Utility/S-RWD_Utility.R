# S-RWD_Utility.R
# To be run on ORNL to compare utility of synthetic data vs training data

Sys.time()

#########################################################################
### PARAMETERS
#########################################################################

# To be modified by users
base_data_directory = "/dummy"

# Synthetic Data
#data_dir_synthetic = "/dummy/restored_omop"
data_dir_synthetic = paste0(base_data_directory, "/100kx10/restored_omop")


#########################################################################
### OUTPUTS
#########################################################################

# UNIVARIATE DISTRIBUTIONS
# - Categorical differences: UNIVDISTS_categorical.csv
# - Continuous differences: UNIVDISTS_continuous.csv
# - Aggregated differences summary: UNIVDISTS_all.csv

# MISSING DATA
# - Missingness rates for all variables: MISSING_rates.csv

# VALIDITY
# - Valid categorical/continuous variables: VALIDITY.csv

# PAIRWISE ASSOCIATIONS
# - Correlations: PAIRWISEASSOC_Corrs.csv
# - Cramer Vs: PAIRWISEASSOC_CramerVs.csv

# HIGH-UTILITY CROSSTABS
#   - 2-way crosstabs: HIGH_UTIL_CROSSTABS.csv

#########################################################################
### PACKAGES
#########################################################################

library(dplyr)    # For data manipulation
library(duckdb)   # For using DuckDB
library(duckplyr)
library(purrr)    # For data manipulation
library(stringr)  # For working with strings/text

#########################################################################
### INPUTS
#########################################################################

# DATA

## Original Data
data_dir_training = paste0(base_data_directory, '/omop_synthea_sample_1mil_test')

if (!dir.exists(data_dir_training)) {  
  stop("The directory for the training data is not available.")
}

all_omop_table_names <- c(
  "concept", "concept_ancestor", "concept_relationship", "condition_occurrence",
  #"death", 
  "drug_exposure", #"measurement", "observation_period", 
  "person", 
  "procedure_occurrence", "visit_occurrence"
) |> set_names()

concept_tables <- all_omop_table_names |>
  keep(\(x) x %in% c("concept", "concept_ancestor", "concept_relationship")) |>
  map(\(x) file.path(data_dir_training, x)) |>
  map(read_parquet_duckdb)

training_data_tables <- all_omop_table_names |>
  discard(\(table_name) table_name %in% c("concept", "concept_ancestor", "concept_relationship")) |>
  map(\(table_name) file.path(data_dir_training, table_name)) |>
  keep(\(file_path) {
    file_exists <- file.exists(file_path)
    if (!file.exists(file_path)) {
      warning(str_glue("No training data found for OMOP table `{basename(file_path)}`."))
    }
    file_exists
  }) |>
  map(read_parquet_duckdb)


## Synthetic Data
if(!dir.exists(data_dir_synthetic)) {
  stop("The directory for the synthetic data is not available.")
}

synthetic_data_tables <- all_omop_table_names |>
  discard(\(table_name) table_name %in% c("concept", "concept_ancestor", "concept_relationship")) |>
  map(\(table_name) file.path(data_dir_synthetic, table_name)) |>
  keep(\(file_path) {
    file_exists <- file.exists(file_path)
    has_parquet_data <- length(list.files(file_path, pattern = "parquet")) > 0
    if (!file_exists || !has_parquet_data) {
      warning(str_glue("No synthetic data found for OMOP table `{basename(file_path)}`."))
    }
    file_exists & has_parquet_data
  }) |>
  map(read_parquet_duckdb)


# FUNCTIONS
source("Utility_functions_for_ORNL.R")


# VARIABLE TYPES
var_types = read.delim("variable_types.txt")


#########################################################################
### DATA PREP
#########################################################################

# To better compare the dates in the data sets, we extract the 
# year, month, days, and days since 1/1/2000 from the full date.

## condition_occurrence
training_data_tables$condition_occurrence = training_data_tables$condition_occurrence |>
  duckplyr::as_tbl() |>
  mutate(across(ends_with("date"), list('year' = \(x) year(date(x)),
                                        'month' = \(x) month(date(x)),
                                        'day' = \(x) day(date(x)),
                                        'since2000' = \(x) date(x) - as.Date('2000-01-01')))) |>
  mutate(condition_start_end_diff = condition_end_date_since2000 - condition_start_date_since2000)

synthetic_data_tables$condition_occurrence = synthetic_data_tables$condition_occurrence |>
  duckplyr::as_tbl() |>
  mutate(across(ends_with("date"), list('year' = \(x) year(date(x)),
                                        'month' = \(x) month(date(x)),
                                        'day' = \(x) day(date(x)),
                                        'since2000' = \(x) date(x) - as.Date('2000-01-01')))) |>
  mutate(condition_start_end_diff = condition_end_date_since2000 - condition_start_date_since2000)

## drug_exposure
training_data_tables$drug_exposure = training_data_tables$drug_exposure |>
  duckplyr::as_tbl() |>
  mutate(across(ends_with("date"), list('year' = \(x) year(date(x)),
                                        'month' = \(x) month(date(x)),
                                        'day' = \(x) day(date(x)),
                                        'since2000' = \(x) date(x) - as.Date('2000-01-01')))) |>
  mutate(drug_exposure_start_end_diff = drug_exposure_end_date_since2000 - drug_exposure_start_date_since2000)

synthetic_data_tables$drug_exposure = synthetic_data_tables$drug_exposure |>
  duckplyr::as_tbl() |>
  mutate(across(ends_with("date"), list('year' = \(x) year(date(x)),
                                        'month' = \(x) month(date(x)),
                                        'day' = \(x) day(date(x)),
                                        'since2000' = \(x) date(x) - as.Date('2000-01-01')))) |>
  mutate(drug_exposure_start_end_diff = drug_exposure_end_date_since2000 - drug_exposure_start_date_since2000)

## person
training_data_tables$person = training_data_tables$person |>
  duckplyr::as_tbl() |>
  mutate(birth_since2000 = date(birth_datetime) - as.Date('2000-01-01'))

synthetic_data_tables$person = synthetic_data_tables$person |>
  duckplyr::as_tbl() |>
  mutate(birth_since2000 = date(birth_datetime) - as.Date('2000-01-01'))

## procedure_occurrence
training_data_tables$procedure_occurrence = training_data_tables$procedure_occurrence |>
  duckplyr::as_tbl() |>
  mutate(across(ends_with("date"), list('year' = \(x) year(date(x)),
                                        'month' = \(x) month(date(x)),
                                        'day' = \(x) day(date(x)),
                                        'since2000' = \(x) date(x) - as.Date('2000-01-01'))))
synthetic_data_tables$procedure_occurrence = synthetic_data_tables$procedure_occurrence |>
  duckplyr::as_tbl() |>
  mutate(across(ends_with("date"), list('year' = \(x) year(date(x)),
                                        'month' = \(x) month(date(x)),
                                        'day' = \(x) day(date(x)),
                                        'since2000' = \(x) date(x) - as.Date('2000-01-01'))))

## visit_occurrence
training_data_tables$visit_occurrence = training_data_tables$visit_occurrence |>
  duckplyr::as_tbl() |>
  mutate(across(ends_with("date"), list('year' = \(x) year(date(x)),
                                        'month' = \(x) month(date(x)),
                                        'day' = \(x) day(date(x)),
                                        'since2000' = \(x) date(x) - as.Date('2000-01-01')))) |>
  mutate(visit_start_end_diff = visit_end_date_since2000 - visit_start_date_since2000)

synthetic_data_tables$visit_occurrence = synthetic_data_tables$visit_occurrence |>
  duckplyr::as_tbl() |>
  mutate(across(ends_with("date"), list('year' = \(x) year(date(x)),
                                        'month' = \(x) month(date(x)),
                                        'day' = \(x) day(date(x)),
                                        'since2000' = \(x) date(x) - as.Date('2000-01-01')))) |>
  mutate(visit_start_end_diff = visit_end_date_since2000 - visit_start_date_since2000)



#########################################################################
### UTILITY: UNIVARIATE DISTRIBUTIONS
#########################################################################

# tables found in both sets of data
data_table_names = intersect(
  names(training_data_tables),
  names(synthetic_data_tables)
)
data_table_names

# for categorical variables with many levels, only display the categories making
# up the top_pct*100%
top_pct = 0.5
top_levels = data.frame()

cont_diffs = data.frame()
cat_diffs = data.frame()

for(df in data_table_names){
  
  # CONTINUOUS
  ## get all continuous variables
  cont_vars = var_types %>%
    filter(folder==df & type=='cont') %>%
    pull(col_name)
  cont_vars = cont_vars[cont_vars %in% colnames(training_data_tables[[df]]) & cont_vars %in% colnames(synthetic_data_tables[[df]])]
  
  ## distributions
  cont_dists_df = dist_btwn_cont_vars(
    orig_data = training_data_tables[[df]],
    new_data = synthetic_data_tables[[df]],
    vars = cont_vars
  )
  
  ## save 
  cont_diffs = bind_rows(
    cont_diffs,
    cont_dists_df %>% mutate(TABLE = df, .before=1)
  )
  
  
  # CATEGORICAL
  ## get all categorical variables
  cat_vars = var_types %>%
    filter(folder==df & type=='cat') %>%
    pull(col_name)
  cat_vars = cat_vars[cat_vars %in% colnames(training_data_tables[[df]]) & cat_vars %in% colnames(synthetic_data_tables[[df]])]
  
  ## full distributions
  cat_dists_df = dist_btwn_cat_vars(
    orig_data = training_data_tables[[df]],
    new_data = synthetic_data_tables[[df]],
    vars = cat_vars
  )
  
  ## limit to top levels
  ### top levels for original data
  orig_summary = cat_dists_df %>%
    group_by(VAR) %>%
    arrange(VAR, -1*N_ORIG) %>%
    mutate(
      CUMPROP_ORIG = cumsum(P_ORIG),
      greater_top_pct = CUMPROP_ORIG>top_pct,
      gtp_diff = greater_top_pct - lag(greater_top_pct)
    )
  top_orig = orig_summary %>%
    filter(greater_top_pct==FALSE | gtp_diff==1) %>%
    select(VAR, DATA_TMP_VAR)
  
  ### top levels for synthetic data
  new_summary = cat_dists_df %>%
    group_by(VAR) %>%
    arrange(VAR, -1*N_NEW) %>%
    mutate(
      CUMPROP_NEW = cumsum(P_NEW),
      greater_top_pct = CUMPROP_NEW>top_pct,
      gtp_diff = greater_top_pct - lag(greater_top_pct)
    )
  top_new = new_summary %>%
    filter(greater_top_pct==FALSE | gtp_diff==1) %>%
    select(VAR, DATA_TMP_VAR)
  
  ### keep levels in top for either data set  
  all_top = bind_rows(top_orig, top_new) %>% unique() %>% mutate(keep=1)
  top_levels = bind_rows(
    top_levels,
    all_top %>% mutate(DATASET = df)
  )
  
  cat_dists1 = full_join(cat_dists_df, all_top)
  others = cat_dists1 %>% 
    filter(is.na(keep)) %>%
    group_by(VAR) %>%
    summarize(
      VAR = unique(VAR),
      N_NEW = sum(N_NEW),
      P_NEW = sum(P_NEW),
      N_ORIG = sum(N_ORIG),
      P_ORIG = sum(P_ORIG),
      HD = unique(HD)
    ) %>%
    mutate(DATA_TMP_VAR = 'Other', TABLE = df)
  
  cat_dists_save = cat_dists1 %>%
    filter(keep==1) %>%
    select(VAR, DATA_TMP_VAR, N_NEW, P_NEW, N_ORIG, P_ORIG, HD) %>%
    mutate(TABLE = df, .before=1)
  
  cat_dists_save = bind_rows(
    cat_dists_save,
    others
  )
  
  chisq_pvals = tapply(
    cat_dists_save %>% select(N_NEW, N_ORIG),
    cat_dists_save$VAR,
    function(x) chisq.test(x)$p.value)
  
  cat_dists_save = merge(
    cat_dists_save,
    data.frame(chisq_pvals, 'VAR' = names(chisq_pvals))
  )
  
  cat_diffs = bind_rows(
    cat_diffs,
    cat_dists_save
  )
  
}

# re-arrange cat_diffs data set
cat_diffs = cat_diffs %>%
  select(TABLE, VAR, DATA_TMP_VAR, N_NEW, P_NEW, N_ORIG, P_ORIG, HD, chisq_pvals) %>%
  arrange(TABLE, VAR, DATA_TMP_VAR)


# Aggregate differences of all variables
all_univariate_diffs = all_diffs(cat_diffs, cont_diffs)

# Save output
write.csv(cat_diffs, 'UNIVDISTS_categorical.csv', row.names = FALSE)
write.csv(cont_diffs, 'UNIVDISTS_continuous.csv', row.names = FALSE)
write.csv(all_univariate_diffs, 'UNIVDISTS_all.csv', row.names = FALSE)


#########################################################################
### UTILITY: MISSING RATES
#########################################################################

all_missing_rates = data.frame()

for(df in data_table_names){
  
  # get missing rates for df
  missing_rates_df = compare_missing_rates(
    orig_data = training_data_tables[[df]],
    new_data = synthetic_data_tables[[df]]
  )
  
  # save
  all_missing_rates = bind_rows(
    all_missing_rates,
    missing_rates_df %>% mutate(TABLE = df)
  )
  
}

all_missing_rates = all_missing_rates %>%
  select(TABLE, VARIABLE, ORIG_MISSING_PROP, NEW_MISSING_PROP, ORIG_UNIQUE_VALS, NEW_UNIQUE_VALS)

# Save
write.csv(all_missing_rates, 'MISSING_rates.csv', row.names = FALSE)


#########################################################################
### UTILITY: VALIDITY
#########################################################################

all_validity = data.frame()

for(df in data_table_names){
  
  # test for validity in df
  valid_df = utility_validity(
    orig_data = training_data_tables[[df]],
    new_data = synthetic_data_tables[[df]],
    vars = var_types %>% filter(folder==df & type %in% c('cat', 'cont')) %>% pull(col_name),
    var_types = var_types %>% filter(folder==df & type %in% c('cat', 'cont')) %>% pull(type)
  )
  
  # save
  all_validity = bind_rows(
    all_validity,
    valid_df %>% mutate(TABLE = df, .before = 1)
  )
  
}

write.csv(all_validity, 'VALIDITY.csv', row.names = FALSE)

#########################################################################
### UTILITY: PAIRWISE ASSOCIATIONS
#########################################################################

# CORRELATIONS
all_corrs = data.frame()

for(df in data_table_names){
  
  # get continuous variables in df
  cont_vars = var_types %>%
    filter(folder==df & type=='cont') %>%
    pull(col_name)
  
  # calculate correlations
  corrs_df = compare_pairwise_correlations(
    orig_data = training_data_tables[[df]],
    new_data = synthetic_data_tables[[df]],
    vars = cont_vars
  )
  
  all_corrs = bind_rows(
    all_corrs,
    corrs_df %>% mutate(TABLE = df, .before=1)
  )
  
}


# CRAMER'S V
all_cvs = data.frame()

for(df in data_table_names){
  
  # get categorical variables in df
  cat_vars = var_types %>%
    filter(folder==df & type=='cat') %>%
    pull(col_name)
  cat_vars = cat_vars[cat_vars %in% colnames(training_data_tables[[df]]) & cat_vars %in% colnames(synthetic_data_tables[[df]])]
  
  # calculate cramer's vs
  df_cvs = cramers_v(
    orig_data = training_data_tables[[df]],
    new_data = synthetic_data_tables[[df]],
    vars = cat_vars
  )
  
  # save
  all_cvs = bind_rows(
    all_cvs,
    df_cvs %>% 
      mutate(TABLE = df, .before = 1)
  )
  
}

# save
write.csv(all_corrs, 'PAIRWISEASSOC_Corrs.csv', row.names = FALSE)
write.csv(all_cvs, 'PAIRWISEASSOC_CramerVs.csv', row.names = FALSE)


#########################################################################
### UTILITY: CROSSTABS
#########################################################################

all_crosstabs = data.frame()

for(df in data_table_names){
  
  # get variables for crosstabs
  df_ctab_vars = var_types %>%
    filter(folder==df & high_util=='yes') %>%
    pull(col_name)
  df_ctab_vars = df_ctab_vars[df_ctab_vars %in% colnames(training_data_tables[[df]]) & df_ctab_vars %in% colnames(synthetic_data_tables[[df]])]
  
  # calculate crosstabs
  df_ctabs = compare_crosstabs(
    orig_data = training_data_tables[[df]],
    new_data = synthetic_data_tables[[df]],
    vars = df_ctab_vars,
    k = 2
  )
  
  # reformat
  for(i in 1:length(df_ctabs)){
    var1 = colnames(df_ctabs[[i]])[1]
    var2 = colnames(df_ctabs[[i]])[2]
    df_ctabsi = df_ctabs[[i]] %>%
      collect() %>%
      mutate(VAR1 = var1, .before=1) %>%
      mutate(VAR2 = var2, .after=1)
    names(df_ctabsi)[3:4] = c('LEVEL_VAR1', 'LEVEL_VAR2')
    
    # keep only top levels
    top_level_vars = top_levels %>% 
      filter(DATASET==df) %>%
      pull(VAR) %>%
      unique()
    if(var1 %in% top_level_vars){
      var1_toplevels = top_levels %>%
        filter(DATASET==df & VAR==var1) %>%
        pull(DATA_TMP_VAR)
    } else{
      var1_toplevels = df_ctabsi %>% pull(LEVEL_VAR1) %>% unique
    }
    if(var2 %in% top_level_vars){
      var2_toplevels = top_levels %>%
        filter(DATASET==df & VAR==var2) %>%
        pull(DATA_TMP_VAR)
    } else{
      var2_toplevels = df_ctabsi %>% pull(LEVEL_VAR2) %>% unique
    }
    
    df_ctabsi = df_ctabsi %>%
      mutate(
        LEVEL_VAR1 = if_else(LEVEL_VAR1 %in% var1_toplevels, as.character(LEVEL_VAR1), 'Other'),
        LEVEL_VAR2 = if_else(LEVEL_VAR2 %in% var2_toplevels, as.character(LEVEL_VAR2), 'Other')
      ) %>%
      group_by(LEVEL_VAR1, LEVEL_VAR2) %>%
      summarise(
        VAR1 = first(VAR1),
        VAR2 = first(VAR2),
        N_ORIG = sum(N_ORIG),
        N_NEW = sum(N_NEW),
        P_ORIG = sum(P_ORIG),
        P_NEW = sum(P_NEW)
      )
    
    all_crosstabs = bind_rows(
      all_crosstabs,
      df_ctabsi %>% mutate(TABLE = df)
    )
    
  }
  
}

all_crosstabs = all_crosstabs %>%
  select(TABLE, VAR1, VAR2, LEVEL_VAR1, LEVEL_VAR2, N_ORIG, N_NEW, P_ORIG, P_NEW) %>%
  mutate(
    P_DIFF = P_NEW - P_ORIG,
    P_RELDIFF = abs(P_DIFF)/P_ORIG
  )

# Save
write.csv(all_crosstabs, 'HIGH_UTIL_CROSSTABS.csv', row.names = FALSE)


#########################################################################
### UTILITY: U-STATISTIC
#########################################################################

# (SKIP FOR NOW)