# S-RWD_Utility.R
# To be run on N3C to compare utility of synthetic data vs training data

Sys.time()

#########################################################################
### PARAMETERS
#########################################################################

# Output Folder
out_dir = "/dummy_directory/"


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

omop_table_names <- c("person", "condition_occurrence",
                      "visit_occurrence", "drug_exposure",
                      "procedure_occurrence", "death", "measurement")|>
  set_names()
#omop_table_names = c('person', 'condition_occurrence') |>
#  set_names()

## Original data
training_data_tables <- list()
for (table_name in omop_table_names) {
  training_data_tables[[table_name]] <- datasets.list_files(table_name) |>
    datasets.download_files(alias = table_name) |>
    unlist() |>
    duckplyr::read_parquet_duckdb()
}

## Synthetic data
synth_table_names = paste0("copd_omop_", omop_table_names)
synthetic_data_tables = list()
for (table_name in synth_table_names) {
  synthetic_data_tables[[table_name]] <- datasets.list_files(table_name) |>
    datasets.download_files(alias = table_name) |>
    unlist() |>
    duckplyr::read_parquet_duckdb()
}
names(synthetic_data_tables) = names(training_data_tables)


# FUNCTIONS
source('Utility_functions_for_N3C.R')

# VARIABLE TYPES
var_types = read.delim("var_types_text.txt")


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

## death
training_data_tables$death = training_data_tables$death |>
  duckplyr::as_tbl() |>
  mutate(across(ends_with("date"), list('year' = \(x) year(date(x)),
                                        'month' = \(x) month(date(x)),
                                        'day' = \(x) day(date(x)),
                                        'since2000' = \(x) date(x) - as.Date('2000-01-01'))))

synthetic_data_tables$death = synthetic_data_tables$death |>
  duckplyr::as_tbl() |>
  mutate(across(ends_with("date"), list('year' = \(x) year(date(x)),
                                        'month' = \(x) month(date(x)),
                                        'day' = \(x) day(date(x)),
                                        'since2000' = \(x) date(x) - as.Date('2000-01-01'))))

## measurement
training_data_tables$measurement = training_data_tables$measurement |>
  duckplyr::as_tbl() |>
  mutate(across(ends_with("date"), list('year' = \(x) year(date(x)),
                                        'month' = \(x) month(date(x)),
                                        'day' = \(x) day(date(x)),
                                        'since2000' = \(x) date(x) - as.Date('2000-01-01'))))

synthetic_data_tables$measurement = synthetic_data_tables$measurement |>
  duckplyr::as_tbl() |>
  mutate(across(ends_with("date"), list('year' = \(x) year(date(x)),
                                        'month' = \(x) month(date(x)),
                                        'day' = \(x) day(date(x)),
                                        'since2000' = \(x) date(x) - as.Date('2000-01-01'))))


#########################################################################
### UTILITY: UNIVARIATE DISTRIBUTIONS
#########################################################################

# tables found in both sets of data
data_table_names = intersect(
  names(training_data_tables),
  names(synthetic_data_tables)
)
data_table_names

# number of unique values for each variables, data set
all_unique_values = data.frame()
for(table in data_table_names){
  print(table)
  orig_distinct = training_data_tables[[table]] %>%
    summarize(across(everything(), n_distinct)) %>%
    collect %>%
    t
  orig_distinct = data.frame('Variable' = rownames(orig_distinct),
                             'Orig_n_unique' = orig_distinct[,1])
  
  synth_distinct = synthetic_data_tables[[table]] %>%
    summarize(across(everything(), n_distinct)) %>%
    collect %>%
    t
  synth_distinct = data.frame('Variable' = rownames(synth_distinct),
                              'Synth_n_unique' = synth_distinct[,1])
  
  temp = merge(orig_distinct, synth_distinct,
               by = 'Variable', all = TRUE) %>%
    mutate(Data = table, .before=Variable)
  all_unique_values = rbind(all_unique_values, temp)
}
## attach variable type from previous version
write.csv(all_unique_values, paste0(out_dir, 'number_unique_values.csv'), row.names = FALSE)
dim(all_unique_values)
all_unique_values = merge(all_unique_values,
                          var_types,
                          by.x = c('Data', 'Variable'),
                          by.y = c('folder', 'col_name'),
                          all.x = TRUE)
dim(all_unique_values)
write.csv(all_unique_values, paste0(out_dir, 'number_unique_values_with_type.csv'), row.names = FALSE)



# for categorical variables with many levels, only display the categories making
# up the top_pct*100%
top_pct = 0.5
top_levels = data.frame()

cont_diffs = data.frame()
cat_diffs = data.frame()

# some data sets break the R session due to size--skip these
cont_skip = c('drug_exposure', 'measurement')

for(df in data_table_names){
  print(df)
  
  # CONTINUOUS
  if(!df %in% cont_skip){
    ## get all continuous variables
    cont_vars = var_types %>%
      filter(folder==df & type=='cont') %>%
      pull(col_name)
    ## limit to variables in both data sets
    cont_vars = cont_vars[cont_vars %in% colnames(training_data_tables[[df]]) & cont_vars %in% colnames(synthetic_data_tables[[df]])]
    
    ## distributions
    cont_dists_df = dist_btwn_cont_vars(
      orig_data = training_data_tables[[df]],
      new_data = synthetic_data_tables[[df]],
      vars = cont_vars
    )
    
    ## save
    write.csv(cont_dists_df, paste0(out_dir, 'temp_', df, '_cont_vars.csv'))
    cont_diffs = bind_rows(
      cont_diffs,
      cont_dists_df %>% mutate(TABLE = df, .before=1)
    )
  }
  
  
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
    arrange(VAR, -1*N_ORIG, .by_group=TRUE) %>%
    mutate(
      CUMPROP_ORIG = cumsum(P_ORIG),
      greater_top_pct = CUMPROP_ORIG>top_pct,
      gtp_diff = greater_top_pct - lag(greater_top_pct),
      order = row_number()
    )
  top_orig = orig_summary %>%
    filter(greater_top_pct==FALSE | order==1 | gtp_diff==1) %>%
    select(VAR, DATA_TMP_VAR)
  
  ### top levels for synthetic data
  new_summary = cat_dists_df %>%
    group_by(VAR) %>%
    arrange(VAR, -1*N_NEW, .by_group=TRUE) %>%
    mutate(
      CUMPROP_NEW = cumsum(P_NEW),
      greater_top_pct = CUMPROP_NEW>top_pct,
      gtp_diff = greater_top_pct - lag(greater_top_pct),
      order = row_number()
    )
  top_new = new_summary %>%
    filter(greater_top_pct==FALSE | order==1 | gtp_diff==1) %>%
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
  
  write.csv(cat_dists_save, paste0(out_dir, 'temp_', df, '_cat_vars.csv'))
  cat_diffs = bind_rows(
    cat_diffs,
    cat_dists_save
  )
  
}

# if had to run each individually due to memory size and need to stack together
temp_cat_files = list.files(out_dir)[grepl('cat_vars', list.files(out_dir)) & grepl('temp', list.files(out_dir))]
cat_diffs = data.frame()
for(cat_file in temp_cat_files){
  d = read.csv(paste0(out_dir, cat_file))
  cat_diffs = bind_rows(cat_diffs, d)
}

temp_cont_files = list.files(out_dir)[grepl('cont_vars', list.files(out_dir)) & grepl('temp', list.files(out_dir))]
cont_diffs = data.frame()
for(cont_file in temp_cont_files){
  d = read.csv(paste0(out_dir, cont_file))
  cont_diffs = bind_rows(cont_diffs, d)
}

# re-arrange cat_diffs and cont_diffs data sets
cat_diffs = cat_diffs %>%
  select(TABLE, VAR, DATA_TMP_VAR, N_NEW, P_NEW, N_ORIG, P_ORIG, HD, chisq_pvals) %>%
  arrange(TABLE, VAR, DATA_TMP_VAR)
cont_diffs = cont_diffs %>%
  select(DATASET, VAR, Min, Q01, Q05, Q10, Q25, Median, Q75, Q90, Q95, Q99, Max, Mean, Sd, KS_PVAL)


# Aggregate differences of all variables
all_univariate_diffs = all_diffs(cat_diffs, cont_diffs)

# Save output
write.csv(cat_diffs, paste0(out_dir, 'UNIVDISTS_categorical.csv'), row.names = FALSE)
write.csv(cont_diffs, paste0(out_dir, 'UNIVDISTS_continuous.csv'), row.names = FALSE)
write.csv(all_univariate_diffs, paste0(out_dir, 'UNIVDISTS_all.csv'), row.names = FALSE)


#########################################################################
### UTILITY: MISSING RATES
#########################################################################

all_missing_rates = data.frame()

skip_missing = c('drug_exposure', 'measurement')

for(df in data_table_names[!data_table_names %in% skip_missing]){
  
  print(df)
  
  # get missing rates for df
  missing_rates_df = compare_missing_rates(
    orig_data = training_data_tables[[df]],
    new_data = synthetic_data_tables[[df]]
  )
  
  # save
  write.csv(missing_rates_df, paste0(out_dir, 'temp_', df, '_missing.csv'))
  all_missing_rates = bind_rows(
    all_missing_rates,
    missing_rates_df %>% mutate(TABLE = df)
  )
  
}

all_missing_rates = all_missing_rates %>%
  select(TABLE, VARIABLE, ORIG_MISSING_PROP, NEW_MISSING_PROP, ORIG_UNIQUE_VALS, NEW_UNIQUE_VALS)

# Save
write.csv(all_missing_rates, paste0(out_dir, 'MISSING_rates.csv'), row.names = FALSE)


#########################################################################
### UTILITY: VALIDITY
#########################################################################

all_validity = data.frame()

for(df in data_table_names){
  
  print(df)
  
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

write.csv(all_validity, paste0(out_dir, 'VALIDITY.csv'), row.names = FALSE)

