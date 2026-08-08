################################ 2_input_file.r ##########################################

################################ Introduction
# This R script is running in N3C Enclave Code Workspace:
# 1. read in N3C OMOP COPD cohort original and synthetic person table
# 2. recode ethnicity, gender, year_of_birth, race
# 3. do some data checks on both original and synthetic data
# 4. Attach the person-level death, obesity, health condition flags that created from 1_flag_creation output files "copd_person_all_flags" and "copd_person_all_flags_s"
#    Indicators are specified in the "customize_concept_sets.xlsx".
# 5. Save the person-level original and synthetic files for running risk measures

######## Library  ##########
library(dplyr)
library(arrow)
library(tidyr)
getwd()

#################### Functions ####################

count_flag <- function(data, flag_name) {
  data |>
    dplyr::select(starts_with("flag_")) |>
    dplyr::summarize(.by = all_of(flag_name), n = dplyr::n()) |>
    dplyr::mutate(percentage = round(n / sum(n) * 100, 2))
}

#############################################################################################

###################################### Data Processing ######################################

###################################### Original Data

person <- datasets.read_table("person") |>
  dplyr::select(person_id, 
                gender_concept_id, gender_concept_name,
                location_id,
                year_of_birth, 
                ethnicity_concept_id, ethnicity_concept_name,
                race_concept_id, race_concept_name)

cat("\n", "Original - Total person count in COPD cohort:", "\n") 
nrow(person)

## Recode
person_r <- person |>
  mutate(ethnicity = ifelse (!(ethnicity_concept_id %in% c(38003563, 38003564)), "Unknown", ethnicity_concept_name),
         sex = ifelse(gender_concept_id %in% c(8521, 8551, 4214687, 0), "Unknown", gender_concept_name),
         year_of_birth = ifelse(year_of_birth == 0, NA, year_of_birth),
         age = ifelse((2024 - year_of_birth) > 100, 999, (2025 - year_of_birth)),
         race_r = ifelse(race_concept_name %in% c("Other Race", "Other", "What Race Ethnicity: Race Ethnicity None Of These"), "Other",
                         ifelse (race_concept_name %in% c("No matching concept", "No Information", "Refuse to answer", 
                                                          "Unknown", "Unknown racial group", "Hispanic", NA), "Unknown", 
                                 ifelse (race_concept_name %in% c("More than one race", "Multiple race", "Multiple races"), "Multiple races", 
                                         race_concept_name))))

## Data Check: demographic in COPD cohort
crosswalk_ethnicity <- person |> count(ethnicity_concept_name, ethnicity_concept_id) |> mutate(percentage = round(n / sum(n) * 100, 2))
crosswalk_r_ethnicity <- person_r |> count(ethnicity, ethnicity_concept_name, ethnicity_concept_id) |> mutate(percentage = round(n / sum(n) * 100, 2))
crosswalk_sex <- person |> count(gender_concept_name, gender_concept_id) |> mutate(percentage = round(n / sum(n) * 100, 2))
crosswalk_r_sex <- person_r |> count(sex, gender_concept_name, gender_concept_id) |> mutate(percentage = round(n / sum(n) * 100, 2))
crosswalk_race <- person |> count(race_concept_name, race_concept_id) |> mutate(percentage = round(n / sum(n) * 100, 2))
crosswalk_r_race <- person_r |> count(race_r, race_concept_name, race_concept_id) |> mutate(percentage = round(n / sum(n) * 100, 2))
tab_year_of_birth <- person |> count(year_of_birth) |> mutate(percentage = round(n / sum(n) * 100, 2))
crosswalk_r_age <- person_r |> count(age, year_of_birth)
tab_age <- person_r |> count(age) |> mutate(percentage = round(n / sum(n) * 100, 2))
crosswalk_r_race_ethnicity <- person_r |> count(ethnicity, ethnicity_concept_name, race_r, race_concept_name)
tab_race_ethnicity <- person_r |> count(ethnicity, race_r) |> mutate(percentage = round(n / sum(n) * 100, 2))

crosswalk_ethnicity
crosswalk_r_ethnicity
crosswalk_sex
crosswalk_r_sex
crosswalk_race
crosswalk_r_race
tab_year_of_birth
crosswalk_r_age
tab_age
crosswalk_r_race_ethnicity
tab_race_ethnicity

## Attach person-level death, obesity, comorbidity Flags ########
# person_flags <- arrow::read_parquet("person_flags.parquet")
person_flags <- datasets.read_table("copd_person_all_flags")
person_flags <- person_flags |> rename_with(~ paste0("flag_", .), -person_id)

cat("\n", "Original - Variables in the person-level flag file:", "\n") 
colnames(person_flags)
cat("\n", "Original - Total person count in the person-level flag file:", "\n") 
nrow(person_flags)

## Data Check: flags distribution (count)
check_flag_n <- person_flags |>
  select(-person_id) |>                              
  pivot_longer(cols = everything(),
               names_to = "variable",
               values_to = "value") |>
  mutate(value = case_when(
    is.na(value) ~ "value_NA",
    value == 0   ~ "value_0",
    value == 1   ~ "value_1"
  )) |>
  count(variable, value) |>
  pivot_wider(names_from = value,
              values_from = n,
              values_fill = 0) |>
  mutate(total = value_0 + value_1)

cat("\n", "Original - Check Flag 0/1 counts:", "\n") 
check_flag_n

## Merge flags to the person file
person_r <- left_join(person_r, person_flags, by = "person_id")

cat("\n", "Original - Total person count in COPD cohort:", "\n") 
nrow(person_flags)

## Data Check: flags distribution (proportion)
check_flag_p <- check_flag_n |>
  rowwise() |>
  mutate(value_0 = round(value_0 / total * 100, 2),
         value_1 = round(value_1 / total * 100, 2)) |>
  select(-total)

check_flag_p

# Print the derivation checks
datasets.write_table(crosswalk_r_age, "check_recoded_age")
datasets.write_table(crosswalk_r_ethnicity, "check_recoded_ethnicity")
datasets.write_table(crosswalk_r_race_ethnicity, "check_recoded_race_ethnicity")
datasets.write_table(crosswalk_r_race, "check_recoded_race")
datasets.write_table(crosswalk_r_sex, "check_recoded_sex")
datasets.write_table(check_flag_n, "check_flag_n")
datasets.write_table(check_flag_p, "check_flag_p")

################################# 

###################################### Synthetic Data 

person_s <- datasets.read_table("copd_omop_person_ds") |>
  dplyr::select(person_id, 
                gender_concept_id, #gender_concept_name,
                location_id,
                year_of_birth, 
                ethnicity_concept_id, #ethnicity_concept_name,
                race_concept_id) #race_concept_name)

cat("\n", "Synthetic - Total person count in COPD cohort:", "\n") 
nrow(person_s)

## Merge concept names
tmp <- person_r |> select(gender_concept_id, gender_concept_name) |> distinct(gender_concept_id, gender_concept_name)
person_s <- left_join(person_s, tmp, by = "gender_concept_id")
any(is.na(person_s$gender_concept_name))

tmp <- person_r |> select(ethnicity_concept_id, ethnicity_concept_name) |> distinct(ethnicity_concept_id, ethnicity_concept_name)
person_s <- left_join(person_s, tmp, by = "ethnicity_concept_id")
any(is.na(person_s$ethnicity_concept_name))

tmp <- person_r |> select(race_concept_id, race_concept_name) |> distinct(race_concept_id, race_concept_name)
person_s <- left_join(person_s, tmp, by = "race_concept_id")
any(is.na(person_s$race_concept_name))

## Recode
person_r_s <- person_s |>
  mutate(ethnicity = ifelse (!(ethnicity_concept_id %in% c(38003563, 38003564)), "Unknown", ethnicity_concept_name),
         sex = ifelse(gender_concept_id %in% c(8521, 8551, 4214687, 0), "Unknown", gender_concept_name),
         year_of_birth = ifelse(year_of_birth == 0, NA, year_of_birth),
         age = ifelse((2024 - year_of_birth) > 100, 999, (2025 - year_of_birth)),
         race_r = ifelse(race_concept_name %in% c("Other Race", "Other", "What Race Ethnicity: Race Ethnicity None Of These"), "Other",
                         ifelse (race_concept_name %in% c("No matching concept", "No Information", "Refuse to answer", 
                                                          "Unknown", "Unknown racial group", "Hispanic", NA), "Unknown", 
                                 ifelse (race_concept_name %in% c("More than one race", "Multiple race", "Multiple races"), "Multiple races", 
                                         race_concept_name))))

## Data Check: demographic in COPD cohort
s_crosswalk_ethnicity <- person_s |> count(ethnicity_concept_name, ethnicity_concept_id) |> mutate(percentage = round(n / sum(n) * 100, 2))
s_crosswalk_r_ethnicity <- person_r_s |> count(ethnicity, ethnicity_concept_name, ethnicity_concept_id) |> mutate(percentage = round(n / sum(n) * 100, 2))
s_crosswalk_sex <- person_s |> count(gender_concept_name, gender_concept_id) |> mutate(percentage = round(n / sum(n) * 100, 2))
s_crosswalk_r_sex <- person_r_s |> count(sex, gender_concept_name, gender_concept_id) |> mutate(percentage = round(n / sum(n) * 100, 2))
s_crosswalk_race <- person_s |> count(race_concept_name, race_concept_id) |> mutate(percentage = round(n / sum(n) * 100, 2))
s_crosswalk_r_race <- person_r_s |> count(race_r, race_concept_name, race_concept_id) |> mutate(percentage = round(n / sum(n) * 100, 2))
s_tab_year_of_birth <- person_s |> count(year_of_birth) |> mutate(percentage = round(n / sum(n) * 100, 2))
s_crosswalk_r_age <- person_r_s |> count(age, year_of_birth)
s_tab_age <- person_r_s |> count(age) |> mutate(percentage = round(n / sum(n) * 100, 2))
s_crosswalk_r_race_ethnicity <- person_r_s |> count(ethnicity, ethnicity_concept_name, race_r, race_concept_name)
s_tab_race_ethnicity <- person_r_s |> count(ethnicity, race_r) |> mutate(percentage = round(n / sum(n) * 100, 2))

s_crosswalk_ethnicity
s_crosswalk_r_ethnicity
s_crosswalk_sex
s_crosswalk_r_sex
s_crosswalk_race
s_crosswalk_r_race
s_tab_year_of_birth
s_crosswalk_r_age
s_tab_age
s_crosswalk_r_race_ethnicity
s_tab_race_ethnicity

## Attach person-level death, obesity, comorbidity Flags ########
person_flags_s <- datasets.read_table("copd_person_all_flags_s")
person_flags_s <- person_flags_s |> rename_with(~ paste0("flag_", .), -person_id)

cat("\n", "Synthetic - ariables in the person-level flag file:", "\n") 
colnames(person_flags_s)
cat("\n", "Synthetic - Total person count in the person-level flag file:", "\n") 
nrow(person_flags_s)

## Data Check: flags distribution (count)
s_check_flag_n <- person_flags_s |>
  select(-person_id) |>                              
  pivot_longer(cols = everything(),
               names_to = "variable",
               values_to = "value") |>
  mutate(value = case_when(
    is.na(value) ~ "value_NA",
    value == 0   ~ "value_0",
    value == 1   ~ "value_1"
  )) |>
  count(variable, value) |>
  pivot_wider(names_from = value,
              values_from = n,
              values_fill = 0) |>
  mutate(total = value_0 + value_1)

cat("\n", "Synthetic - Check Flag 0/1 counts:", "\n") 
s_check_flag_n

## Merge flags to the person file
person_r_s <- left_join(person_r_s, person_flags_s, by = "person_id")

cat("\n", "Synthetic - Total person count in COPD cohort:", "\n") 
nrow(person_flags_s)

## Data Check: flags distribution (proportion)
s_check_flag_p <- s_check_flag_n |>
  rowwise() |>
  mutate(value_0 = round(value_0 / total * 100, 2),
         value_1 = round(value_1 / total * 100, 2)) |>
  select(-total)

s_check_flag_p

# Save the derivation checks
datasets.write_table(s_crosswalk_r_age, "check_recoded_age_s")
datasets.write_table(s_crosswalk_r_ethnicity, "check_recoded_ethnicity_s")
datasets.write_table(s_crosswalk_r_race_ethnicity, "check_recoded_race_ethnicity_s")
datasets.write_table(s_crosswalk_r_race, "check_recoded_race_s")
datasets.write_table(s_crosswalk_r_sex, "check_recoded_sex_s")
datasets.write_table(s_check_flag_n, "check_flag_n_s")
datasets.write_table(s_check_flag_p, "check_flag_p_s")

# Save the person-level original and synthetic files for running risk measures
datasets.write_table(person_r_s, "person_r_s")
datasets.write_table(person_r, "person_r")