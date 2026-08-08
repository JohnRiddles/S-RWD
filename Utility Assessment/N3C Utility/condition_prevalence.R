
# condition_prevalence
# S-RWD
# calculate prevalence of each condition to compare between original and
# synthetic data

# packages
library(reticulate)
library(arrow)
library(duckplyr)
library(dplyr)
library(purrr)
library(ggplot2)

# To be modified by user
output_folder = '/dummy_directory/'

################################################################################
################# DATA #########################################################
################################################################################

## original data
omop_tables <- list()
omop_table_names <- c("person", "measurement",
                      "condition_occurrence",
                      "visit_occurrence")
for (table_name in omop_table_names) {
  omop_tables[[table_name]] <- datasets.list_files(table_name) |>
    datasets.download_files(alias = table_name) |>
    unlist() |>
    duckplyr::read_parquet_duckdb() |>
    duckplyr::as_tbl()
}

## synthetic data
synthetic_tables = list()
synth_table_names = paste0("copd_omop_", omop_table_names)
for (table_name in synth_table_names) {
  synthetic_tables[[table_name]] <- datasets.list_files(table_name) |>
    datasets.download_files(alias = table_name) |>
    unlist() |>
    duckplyr::read_parquet_duckdb()
}
names(synthetic_tables) = names(omop_tables)


################################################################################
############## CONDITION PREVALENCE ############################################
################################################################################

colnames(omop_tables[['person']])
colnames(omop_tables[['condition_occurrence']])

colnames(synthetic_tables[['person']])
colnames(synthetic_tables[['condition_occurrence']])

## number of patients
n_orig_patients = omop_tables[['person']] %>%
  pull(person_id) %>%
  unique() %>%
  length()
n_orig_patients

n_synth_patients = synthetic_tables[['person']] %>%
  pull(person_id) %>%
  unique() %>%
  length()
n_synth_patients


# conditions prevalence
## original
prev_orig = omop_tables[['condition_occurrence']] %>%
  select(person_id, condition_concept_name) %>%
  dplyr::distinct() %>%
  count(condition_concept_name) %>%
  mutate(n_total_patients = n_orig_patients) %>%
  mutate(cond_prev_orig = n/n_total_patients) %>%
  arrange(-cond_prev_orig)

head(prev_orig, 50) %>% print(n=50)

# highest is condition_concept_id = 255573 -- COPB: 80%
# followed by essential hypertension (320128): 76%,
# dyspnea (312437) [shortness of breath]: 64%,
# hyperlipidemia (432867) [high cholesterol]: 61%,
# gastroesophageal reflux (4144111): 49%
#
# using condition_concept_name is different, but similar, at least for COPD

#to_save = prev_orig %>% head(1000) %>% collect
#write.csv(to_save,'condition_prevalence_orig.csv')

### distribution of condition prevalance rates
# NOTE: Need to 'collect()' before running these
#nrow(prev_orig)
#summary(prev_orig$cond_prev_orig)
#hist(log(prev_orig$cond_prev_orig, 10))


## synthetic
### condition concept name not in synthetic data--attach from original
condition_names = omop_tables[['condition_occurrence']] %>%
  select(condition_concept_name, condition_concept_id) %>%
  count(condition_concept_name, condition_concept_id) %>%
  #dplyr::distinct() %>%
  collect

dim(condition_names)
length(unique(condition_names$condition_concept_id))

dedup_condition_ids = condition_names %>%
  group_by(condition_concept_id) %>%
  arrange(-n, by_group=TRUE) %>%
  slice_head(n=1)

dim(dedup_condition_ids)

prev_synth = synthetic_tables[['condition_occurrence']] %>%
  select(person_id, condition_concept_id) %>%
  dplyr::distinct() %>%
  count(condition_concept_id) %>%
  collect
prev_synth = merge(prev_synth,
                   dedup_condition_ids %>% select(condition_concept_id, condition_concept_name),
                   by = 'condition_concept_id',
                   all.x = TRUE)
prev_synth = prev_synth %>%
  mutate(n_total_patients = n_synth_patients) %>%
  group_by(condition_concept_name) %>%
  summarize(cond_prev_synth = sum(n)/n_total_patients) %>%
  arrange(-cond_prev_synth)

head(prev_synth, 50) %>% print(n=50)

## compare original vs. synthetic condition prevalence
prev_orig_coll = prev_orig %>% collect
all_cond_prev = merge(prev_orig_coll %>% select(condition_concept_name, cond_prev_orig),
                      prev_synth,
                      by = 'condition_concept_name',
                      all = TRUE)
all_cond_prev = all_cond_prev %>% arrange(-cond_prev_orig)

#datasets.write_table(all_cond_prev, "condition_prevalence_comparison")
datasets.write_table(all_cond_prev, "condition_prevalence_comparison_20260722")
all_cond_prev = datasets.read_table("condition_prevalence_comparison_20260722")

plot(all_cond_prev$cond_prev_orig[1:100], all_cond_prev$cond_prev_synth[1:100])

ggplot(all_cond_prev[1:100,], aes(x=cond_prev_orig, y=cond_prev_synth)) +
  geom_point() +
  labs(x='Original Condition Prevalence',
       y='Synthetic Condition Prevalence',
       title='Condition Prevalence for Top 100 Conditions')


################################################################################
############### MEASURES #######################################################
################################################################################

## We make two different comparisons with measurement:
## 1) which measurements were taken at the highest rates for unique patients
## 2) values of measurements, where we take the most recent measurement for
##    only a subset of measurements

colnames(omop_tables[['measurement']])
colnames(synthetic_tables[['measurement']])

## Measure rates: original
meas_rate_orig = omop_tables[['measurement']] %>%
  select(person_id, measurement_concept_name) %>%
  dplyr::distinct() %>%
  count(measurement_concept_name) %>%
  mutate(n_total_patients = n_orig_patients) %>%
  mutate(meas_rate_orig = n/n_total_patients) %>%
  arrange(-meas_rate_orig)

head(meas_rate_orig, 100) %>% print(n=100000)

## Measure rates: synthetic
### measurement concept name not in synthetic data--attach from original
measurement_names = omop_tables[['measurement']] %>%
  select(measurement_concept_name, measurement_concept_id) %>%
  count(measurement_concept_name, measurement_concept_id) %>%
  #dplyr::distinct() %>%
  collect

dim(measurement_names)
length(unique(measurement_names$measurement_concept_id))

dedup_measurement_ids = measurement_names %>%
  group_by(measurement_concept_id) %>%
  arrange(-n, by_group=TRUE) %>%
  slice_head(n=1)

dim(dedup_measurement_ids)

meas_rate_synth = synthetic_tables[['measurement']] %>%
  select(person_id, measurement_concept_id) %>%
  dplyr::distinct() %>%
  count(measurement_concept_id) %>%
  collect
meas_rate_synth = merge(meas_rate_synth,
                        dedup_measurement_ids %>% select(measurement_concept_id, measurement_concept_name),
                        by = 'measurement_concept_id',
                        all.x = TRUE)

meas_rate_synth = meas_rate_synth %>%
  group_by(measurement_concept_name) %>%
  summarize(total_meas_synth = sum(n)) %>%
  mutate(n_total_patients = n_synth_patients) %>%
  mutate(meas_rate_synth = total_meas_synth/n_total_patients) %>%
  arrange(-meas_rate_synth)

head(meas_rate_synth, 100) %>% print(n=100)


## compare original vs. synthetic measurement rates
meas_rate_orig_coll = meas_rate_orig %>% collect
all_meas_rate = merge(meas_rate_orig_coll %>% select(measurement_concept_name, meas_rate_orig),
                      meas_rate_synth %>% select(measurement_concept_name, meas_rate_synth),
                      by = 'measurement_concept_name',
                      all = TRUE)
all_meas_rate = all_meas_rate %>% arrange(-meas_rate_orig)

datasets.write_table(all_meas_rate, "measurement_rates_comparison")
all_meas_rate = datasets.read_table("measurement_rates_comparison")

plot(all_meas_rate$meas_rate_orig[1:100], all_meas_rate$meas_rate_synth[1:100])

ggplot(all_meas_rate[1:100,], aes(x=meas_rate_orig, y=meas_rate_synth)) +
  geom_point() +
  labs(x='Original Measurement Rates',
       y='Synthetic Measurement Rates',
       title='Measurement Rates for Top 100 Measures')


## Measurements in which to compare values
### rate > 25%
meas_over25 = meas_rate_orig %>%
  filter(meas_rate_orig > 0.25) %>%
  collect() %>%
  arrange(-meas_rate_orig)
### and random 50 where rate is 1-2%
set.seed(123)
meas_rand12 = meas_rate_orig %>%
  filter(meas_rate_orig > 0.01 & meas_rate_orig < 0.02) %>%
  mutate(randnum = runif()) %>%
  arrange(randnum) %>%
  head(50) %>%
  collect
meas_get_vals = rbind(meas_over25,
                      meas_rand12 %>% select(-randnum))

#write.csv(meas_get_vals, 'measurements_to_compare.csv')


## Measure values: original
meas_vals_orig = omop_tables[['measurement']] %>%
  filter(measurement_concept_name %in% meas_get_vals$measurement_concept_name &
           !is.na(value_as_number)) %>%
  select(person_id, measurement_concept_name, measurement_date, value_as_number) %>%
  group_by(measurement_concept_name, person_id) %>%
  slice_max(order_by = measurement_date, n=1, with_ties=FALSE) %>%
  ungroup() %>%
  group_by(measurement_concept_name) %>%
  summarize(n = n(),
            min = min(value_as_number),
            mean = mean(value_as_number),
            median = median(value_as_number),
            max = max(value_as_number)) %>%
  collect()

## Measure values: synthetic
comp_measurement_ids = dedup_measurement_ids %>%
  filter(measurement_concept_name %in% meas_get_vals$measurement_concept_name) %>%
  pull(measurement_concept_id)
meas_vals_synth = synthetic_tables[['measurement']] %>%
  compute(prudence = 'lavish') %>%
  filter(measurement_concept_id %in% comp_measurement_ids &
           !is.na(value_as_number)) %>%
  select(person_id, measurement_concept_id, measurement_date, value_as_number) %>%
  group_by(measurement_concept_id, person_id) %>%
  slice_max(order_by = measurement_date, n=1, with_ties=FALSE) %>%
  ungroup() %>%
  group_by(measurement_concept_id) %>%
  summarize(n = n(),
            min = min(value_as_number),
            mean = mean(value_as_number),
            median = median(value_as_number),
            max = max(value_as_number)) %>%
  collect()



################################################################################
################# VISITS #######################################################
################################################################################

# Compare visits:
# - days between 1st & 2nd, 2nd & 3rd, etc. up to 5th visit
# - days between 1st and last visit

colnames(omop_tables[['visit_occurrence']])
colnames(synthetic_tables[['visit_occurrence']])

# original

#test_patients = omop_tables[['visit_occurrence']] %>% pull(person_id) %>% unique() %>% head(20)

#orig_visit_stats = omop_tables[['visit_occurrence']] %>%
#filter(person_id %in% test_patients) %>%
#  collect()
orig_visit_stats = omop_tables[['visit_occurrence']] %>% #orig_visit_stats %>%
  # create variable of visit number by person
  mutate(x = 1) %>%
  group_by(person_id) %>%
  arrange(visit_start_date, .by_group = TRUE) %>%
  mutate(person_visit_num = cumsum(x),
         person_max = n()) %>%
  ungroup()
orig_visit_stats = orig_visit_stats %>%
  # filter to first 5 and last visit
  filter(person_visit_num %in% 1:5 | person_visit_num == person_max) %>%
  select(person_id, person_max, visit_start_date, person_visit_num) %>%
  collect

orig_visit_stats1 = orig_visit_stats %>%
  group_by(person_id) %>%
  summarize(total_visits = person_max[1])
orig_visit_stats2 = orig_visit_stats %>%
  filter(person_max > 1) %>%
  group_by(person_id) %>%
  summarize(days_1_last = as.numeric(visit_start_date[person_visit_num==person_max] -
                                       visit_start_date[person_visit_num==1]),
            days_1_2 = as.numeric(visit_start_date[person_visit_num==2] - visit_start_date[person_visit_num==1]))
orig_visit_stats3 = orig_visit_stats %>%
  filter(person_max > 2) %>%
  group_by(person_id) %>%
  summarize(days_2_3 = as.numeric(visit_start_date[person_visit_num==3] - visit_start_date[person_visit_num==2]))
orig_visit_stats4 = orig_visit_stats %>%
  filter(person_max > 3) %>%
  group_by(person_id) %>%
  summarize(days_3_4 = as.numeric(visit_start_date[person_visit_num==4] - visit_start_date[person_visit_num==3]))
orig_visit_stats5 = orig_visit_stats %>%
  filter(person_max > 4) %>%
  group_by(person_id) %>%
  summarize(days_4_5 = as.numeric(visit_start_date[person_visit_num==5] - visit_start_date[person_visit_num==4]))

all_orig_visit_stats = list(orig_visit_stats1,
                            orig_visit_stats2,
                            orig_visit_stats3,
                            orig_visit_stats4,
                            orig_visit_stats5) %>%
  reduce(full_join, by='person_id')


# synthetic
compute(prudence = 'lavish')
synth_visit_stats = synthetic_tables[['visit_occurrence']] %>%
  # create variable of visit number by person
  mutate(x = 1) %>%
  arrange(visit_start_date, .by = person_id) %>%
  compute(prudence = 'lavish') %>%
  mutate(person_visit_num = cumsum(x), .by=person_id) %>%
  mutate(person_max = n(), .by=person_id)
synth_visit_stats = synth_visit_stats %>%
  # filter to first 5 and last visit
  filter(person_visit_num %in% 1:5 | person_visit_num == person_max) %>%
  select(person_id, person_max, visit_start_date, person_visit_num) %>%
  collect

synth_visit_stats1 = synth_visit_stats %>%
  group_by(person_id) %>%
  summarize(total_visits = person_max[1])
synth_visit_stats2 = synth_visit_stats %>%
  filter(person_max > 1) %>%
  group_by(person_id) %>%
  summarize(days_1_last = as.numeric(visit_start_date[person_visit_num==person_max] -
                                       visit_start_date[person_visit_num==1]),
            days_1_2 = as.numeric(visit_start_date[person_visit_num==2] - visit_start_date[person_visit_num==1]))
synth_visit_stats3 = synth_visit_stats %>%
  filter(person_max > 2) %>%
  group_by(person_id) %>%
  summarize(days_2_3 = as.numeric(visit_start_date[person_visit_num==3] - visit_start_date[person_visit_num==2]))
synth_visit_stats4 = synth_visit_stats %>%
  filter(person_max > 3) %>%
  group_by(person_id) %>%
  summarize(days_3_4 = as.numeric(visit_start_date[person_visit_num==4] - visit_start_date[person_visit_num==3]))
synth_visit_stats5 = synth_visit_stats %>%
  filter(person_max > 4) %>%
  group_by(person_id) %>%
  summarize(days_4_5 = as.numeric(visit_start_date[person_visit_num==5] - visit_start_date[person_visit_num==4]))

all_synth_visit_stats = list(synth_visit_stats1,
                             synth_visit_stats2,
                             synth_visit_stats3,
                             synth_visit_stats4,
                             synth_visit_stats5) %>%
  reduce(full_join, by='person_id')

#write.csv(all_synth_visit_stats, 'visit_stats_synth.csv')
#write.csv(all_orig_visit_stats, 'visit_stats_orig.csv')

#all_synth_visit_stats = read.csv('visit_stats_synth.csv')
#all_orig_visit_stats = read.csv('visit_stats_orig.csv')

# create plots to display differences in distribution
combined = rbind(all_synth_visit_stats %>% mutate(df = 'synthetic'),
                 all_orig_visit_stats %>% mutate(df = 'original'))
datasets.write_table(combined, "visit_days")

ggplot(combined, aes(x=total_visits, fill=df)) +
  geom_histogram(aes(y=after_stat(density)), position='identity', alpha=0.5)

ggplot(combined, aes(x=days_1_last, fill=df)) +
  geom_histogram(aes(y=after_stat(density)), position='identity', alpha=0.5)

ggplot(combined, aes(x=days_1_2, fill=df)) +
  geom_histogram(aes(y=after_stat(density)), position='identity', alpha=0.5)

ggplot(combined, aes(x=days_2_3, fill=df)) +
  geom_histogram(aes(y=after_stat(density)), position='identity', alpha=0.5)

ggplot(combined, aes(x=days_3_4, fill=df)) +
  geom_histogram(aes(y=after_stat(density)), position='identity', alpha=0.5)

ggplot(combined, aes(x=days_4_5, fill=df)) +
  geom_histogram(aes(y=after_stat(density)), position='identity', alpha=0.5)
