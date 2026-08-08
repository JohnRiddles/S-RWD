################################ 1_risk_100k_DataPrep.r ##########################################

################################ Introduction
# This script is to:
# 1. read in OMOP synthea sample synthetic 1million test data and the OMOP synthea original data (no recoding needed)
# 2. concept mapping 
# 3. do some data checks on both original and synthetic data
# 4. create the person-level condition flags (indicators) that specified in the "customize_concept_sets.xlsx".
#    Indicator names for these conditions are assigned, and the indicators are collapsed to unique instances on the basis of patient.

################################ Pre-run Steps
### (Option 1)

## RUN THIS IN TERMINAL:
## echo $HOME
# (This is to print the home directory path)
## pwd
# (This is to print the current directory)
## cp -r /dummy .
# (This is to copy directory into current folder)
## ls -d /dummy/*/
# (This is to list the folders under the 100k synthetic training folder)
## ml cray-R && R
# (This is to create R terminal)

### (Option 2)

## RUN THIS IN TERMINAL:
## module load cray-R
## which R
# (This is to make sure the path is written in R > Rterm: Linux under Remote [SSH: frontier.olcf.ornl.gov] tab in the Settings)
## Ctrl+Shift+P → R: Create R Terminal

###################################### Library ######################################

.libPaths("~/Rlibs")
# library(arrow)
library(dplyr)
library(knitr)
library(duckplyr)
library(openxlsx)
library(readxl)
# library(arrow)

# install.packages("openxlsx", lib = "~/Rlibs", repos = "https://cloud.r-project.org")

###################################### Functions ######################################

build_concept_lookupDuckdb <- function(concept) {
  # Required column
  if (!"concept_id" %in% colnames(concept)) {
    return(list())
  }

  # Optional columns
  name_col <- if ("concept_name" %in% colnames(concept)) {
    "concept_name"
  } else {
    NULL
  }
  dom_col <- if ("domain_id" %in% colnames(concept)) "domain_id" else NULL
  cls_col <- if ("concept_class_id" %in% colnames(concept)) {
    "concept_class_id"
  } else {
    NULL
  }

  # Columns to keep
  cols <- c("concept_id", name_col, dom_col, cls_col)

  # SQL‑translatable: select + distinct
  concept_lu <- concept |>
    dplyr::select(all_of(cols)) |>
    distinct(concept_id, .keep_all = TRUE)

  concept_lu
}

top_k <- function(df, id_col, k = 20, title = NULL, concept_lu) {
  # Validate input
  if (is.null(df) || !id_col %in% colnames(df)) {
    message("(skip: ", title %||% id_col, " missing)")
    return(invisible(NULL))
  }

  # SQL‑translatable counting
  out <- df |>
    dplyr::summarize(
      n = dplyr::n(),
      .by = all_of(id_col)
    ) |>
    dplyr::arrange(desc(n)) |> # SQL ORDER BY
    slice_head(n = k) |> # SQL LIMIT
    dplyr::left_join(
      concept_lu |>
        dplyr::select(concept_id, concept_name),
      by = join_by(!!sym(id_col) == concept_id)
    ) |>
    dplyr::collect() # exit prudent mode

  if (!is.null(title)) {
    cat("### ", title, "\n", sep = "")
  }

  kable(out)

  return(out)
}

create_concept_name <- function(df, id_col, name_col, concept_lu) {
  # Ensure the ID column exists in X
  if (is.null(df) || !id_col %in% colnames(df)) {
    message("(skip: ", title %||% id_col, " missing)")
    return(invisible(NULL))
  }

  df |>
    dplyr::left_join(
      concept_lu |>
        dplyr::select(concept_id, concept_name),
      by = join_by(!!sym(id_col) == concept_id)
    ) |>
    rename(!!sym(name_col) := concept_name)
}

count_flag <- function(data, flag_name) {
  data |>
    dplyr::select(starts_with("flag_")) |>
    dplyr::summarize(.by = all_of(flag_name), n = dplyr::n()) |>
    dplyr::mutate(percentage = round(n / sum(n) * 100, 2))
}

###################################### Specify the Directories ######################################

out_dir <- file.path(getwd(), "Output")

###################################### Data Processing ######################################

########## Original Data directory
orig_input_dir <- "/dummy/omop_synthea_sample_1mil_test"
orig_input_folders <- list.files(orig_input_dir)
print(orig_input_folders)

########## Synthetic Data directory
synth_input_dir <- "/dummy/restored_omop"
synth_input_folders <- list.files(synth_input_dir)
synth_input_folders <- synth_input_folders[
  !grepl('.csv|.txt', synth_input_folders)
]
print(synth_input_folders)

########## Data Check
cat("\n", "Synthetic data sets that are not in the original data:", "\n")
# print(setdiff(orig_input_folders, synth_input_folders))
print(setdiff(synth_input_folders, orig_input_folders))

input_folders <- intersect(orig_input_folders, synth_input_folders)
cat("\n", "Data sets that are in both original and synthetic:", "\n")
print(input_folders)

########## Synthetic Data Input
# Read in the synthetic testing data sets.
# The data sets are located in the .parquet file within each subfolder.

names(input_folders) <- input_folders

synth_data <- input_folders |>
  lapply(\(input_folders) {
    duckplyr::read_parquet_duckdb(
      path = file.path(synth_input_dir, input_folders)
    )
  })

cat("\n", "Inspect column names:", "\n")
lapply(synth_data, colnames)

cat(
  "\n",
  "Print number of rows in the synthetic data and compare to the original data:",
  "\n"
)
cnt <- lapply(synth_data, \(df) {
  df |> dplyr::summarize(n = dplyr::n())
}) |>
  bind_rows(.id = "table")
knitr::kable(cnt)

cat("\n", "Print the number of persons in the synthetic data:", "\n")
cnt <- lapply(synth_data, \(df) {
  df |> dplyr::summarize(n_person = n_distinct(person_id))
}) |>
  bind_rows(.id = "table")
knitr::kable(cnt)

########## Original Data Input
orig_data <- input_folders |>
  lapply(\(input_folders) {
    read_parquet_duckdb(
      path = file.path(orig_input_dir, input_folders)
    )
  })
cat("\n", "Inspect column names:", "\n")
lapply(orig_data, colnames)

cat("\n", "Print number of rows in the original training data:", "\n")
cnt <- lapply(orig_data, \(df) {
  df |> dplyr::summarize(n = dplyr::n())
}) |>
  bind_rows(.id = "table")
knitr::kable(cnt)

cat("\n", "Print the number of persons in the original training data:", "\n")
cnt <- lapply(orig_data, \(df) {
  df |> dplyr::summarize(n_person = n_distinct(person_id))
}) |>
  bind_rows(.id = "table")
knitr::kable(cnt)

########## Concept Mapping
# Note: `concept`, `concept_ancestor`, and `concept_relationship` are under
# the original data `orig_input_folders` folder.

concept_folders <- c("concept", "concept_ancestor", "concept_relationship")
names(concept_folders) <- concept_folders
concept_map <- concept_folders |>
  lapply(\(concept_folders) {
    read_parquet_duckdb(
      path = file.path(orig_input_dir, concept_folders)
    )
  })

cat("\n", "Inspect column names:", "\n")
lapply(concept_map, colnames)

cat("\n", "Count number of rows in each concept table:", "\n")
cnt <- lapply(concept_map, \(df) {
  df |> dplyr::summarize(n = dplyr::n())
}) |>
  bind_rows(.id = "table")
knitr::kable(cnt)

concept_lu <- build_concept_lookupDuckdb(concept_map[["concept"]])

cat("\n", "Count number of concepts by domain in concept lookup table:", "\n")
cat(
  "\n",
  "Check if any multiple concept ids match to one concept name in full concept mapping: TRUE",
  "\n"
)
cnt <- concept_lu |>
  dplyr::summarize(
    .by = domain_id,
    n = dplyr::n(),
    n_concept_id = n_distinct(concept_id),
    n_concept_name = n_distinct(concept_name)
  )
knitr::kable(cnt)

########## Data Check
# Report the top concept counts:
# top_50_concepts_synth.xlsx & top_50_concepts_orig.xlsx

top_co <- top_k(
  df = synth_data[["condition_occurrence"]],
  id_col = "condition_concept_id",
  k = 50,
  title = "Top condition concepts - synthetic",
  concept_lu = concept_lu
)
top_de <- top_k(
  df = synth_data[["drug_exposure"]],
  id_col = "drug_concept_id",
  k = 50,
  title = "Top drug concepts - synthetic",
  concept_lu = concept_lu
)
top_po <- top_k(
  df = synth_data[["procedure_occurrence"]],
  id_col = "procedure_concept_id",
  k = 50,
  title = "Top procedure  - synthetic",
  concept_lu = concept_lu
)
top_vo <- top_k(
  df = synth_data[["visit_occurrence"]],
  id_col = "visit_concept_id",
  k = 50,
  title = "Top visit concepts - synthetic",
  concept_lu = concept_lu
)

wb <- createWorkbook()
addWorksheet(wb, "condition_occurrence")
writeData(wb, "condition_occurrence", top_co)

addWorksheet(wb, "drug_exposure")
writeData(wb, "drug_exposure", top_de)

addWorksheet(wb, "procedure_occurrence")
writeData(wb, "procedure_occurrence", top_po)

addWorksheet(wb, "visit_occurrence")
writeData(wb, "visit_occurrence", top_vo)

saveWorkbook(
  wb,
  file.path(out_dir, "top_50_concepts_synth.xlsx"),
  overwrite = TRUE
)

top_co <- top_k(
  df = orig_data[["condition_occurrence"]],
  id_col = "condition_concept_id",
  k = 50,
  title = "Top condition concepts - original",
  concept_lu = concept_lu
)
top_de <- top_k(
  df = orig_data[["drug_exposure"]],
  id_col = "drug_concept_id",
  k = 50,
  title = "Top drug concepts - original",
  concept_lu = concept_lu
)
top_po <- top_k(
  df = orig_data[["procedure_occurrence"]],
  id_col = "procedure_concept_id",
  k = 50,
  title = "Top procedure concepts - original",
  concept_lu = concept_lu
)
top_vo <- top_k(
  df = orig_data[["visit_occurrence"]],
  id_col = "visit_concept_id",
  k = 50,
  title = "Top visit concepts - original",
  concept_lu = concept_lu
)

wb <- createWorkbook()
addWorksheet(wb, "condition_occurrence")
writeData(wb, "condition_occurrence", top_co)

addWorksheet(wb, "drug_exposure")
writeData(wb, "drug_exposure", top_de)

addWorksheet(wb, "procedure_occurrence")
writeData(wb, "procedure_occurrence", top_po)

addWorksheet(wb, "visit_occurrence")
writeData(wb, "visit_occurrence", top_vo)

saveWorkbook(
  wb,
  file.path(out_dir, "top_50_concepts_orig.xlsx"),
  overwrite = TRUE
)

########## Attach Concept Name to Synthetic Data

synth_data[["condition_occurrence"]] <- create_concept_name(
  df = synth_data[["condition_occurrence"]],
  id_col = "condition_concept_id",
  name_col = "condition_concept_name",
  concept_lu = concept_lu
)
synth_data[["visit_occurrence"]] <- create_concept_name(
  df = synth_data[["visit_occurrence"]],
  id_col = "visit_concept_id",
  name_col = "visit_concept_name",
  concept_lu = concept_lu
)
synth_data[["procedure_occurrence"]] <- create_concept_name(
  df = synth_data[["procedure_occurrence"]],
  id_col = "procedure_concept_id",
  name_col = "procedure_concept_name",
  concept_lu = concept_lu
)
synth_data[["drug_exposure"]] <- create_concept_name(
  df = synth_data[["drug_exposure"]],
  id_col = "drug_concept_id",
  name_col = "drug_concept_name",
  concept_lu = concept_lu
)
synth_data[["person"]] <- create_concept_name(
  df = synth_data[["person"]],
  id_col = "gender_concept_id",
  name_col = "gender_concept_name",
  concept_lu = concept_lu
)
synth_data[["person"]] <- create_concept_name(
  df = synth_data[["person"]],
  id_col = "ethnicity_concept_id",
  name_col = "ethnicity_concept_name",
  concept_lu = concept_lu
)
synth_data[["person"]] <- create_concept_name(
  df = synth_data[["person"]],
  id_col = "race_concept_id",
  name_col = "race_concept_name",
  concept_lu = concept_lu
)

# Write to .parquet
# for (nm in names(synth_data)) {
#   rel <- synth_data[[nm]]
#   stopifnot(inherits(rel, "prudent_duckplyr_df"))

#   df <- dplyr::collect(rel)  # materialize to an R tibble/data.frame
#   arrow::write_parquet(df, file.path(out_dir, paste0(nm, "_synth.parquet")),
#                        compression = "zstd")
#   rm(df)
#   rm(rel)
# }

# ID Checks
# check one‑to‑many matches, where a single condition_concept_id maps to multiple different condition_concept_name
cat(
  "\n",
  "Count check one‑to‑many concept matches in synthetic test data:",
  "\n"
)
cnt <- synth_data[["condition_occurrence"]] |>
  dplyr::summarize(
    n_person = n_distinct(person_id),
    n_condition_concept_id = n_distinct(condition_concept_id),
    n_condition_concept_name = n_distinct(condition_concept_name)
  )
knitr::kable(cnt)

cnt <- synth_data[["visit_occurrence"]] |>
  dplyr::summarize(
    n_person = n_distinct(person_id),
    n_visit_concept_id = n_distinct(visit_concept_id),
    n_visit_concept_name = n_distinct(visit_concept_name)
  )
knitr::kable(cnt)

cnt <- synth_data[["procedure_occurrence"]] |>
  dplyr::summarize(
    n_person = n_distinct(person_id),
    n_procedure_concept_id = n_distinct(procedure_concept_id),
    n_procedure_concept_name = n_distinct(procedure_concept_name)
  )
knitr::kable(cnt)

cnt <- synth_data[["drug_exposure"]] |>
  dplyr::summarize(
    n_person = n_distinct(person_id),
    n_drug_concept_id = n_distinct(drug_concept_id),
    n_pdrug_concept_name = n_distinct(drug_concept_name)
  )
knitr::kable(cnt)

crosswalk_ethnicity <- synth_data[["person"]] |>
  count(ethnicity_concept_id, ethnicity_concept_name) |>
  mutate(percentage = round(n / sum(n) * 100, 2))
crosswalk_gender <- synth_data[["person"]] |>
  count(gender_concept_id, gender_concept_name) |>
  mutate(percentage = round(n / sum(n) * 100, 2))
crosswalk_race <- synth_data[["person"]] |>
  count(race_concept_id, race_concept_name) |>
  mutate(percentage = round(n / sum(n) * 100, 2))
tab_year_of_birth <- synth_data[["person"]] |>
  count(year_of_birth) |>
  mutate(percentage = round(n / sum(n) * 100, 2))

wb <- createWorkbook()
addWorksheet(wb, "ethnicity")
writeData(wb, "ethnicity", crosswalk_ethnicity)

addWorksheet(wb, "gender")
writeData(wb, "gender", crosswalk_gender)

addWorksheet(wb, "race")
writeData(wb, "race", crosswalk_race)

addWorksheet(wb, "year_of_birth")
writeData(wb, "year_of_birth", tab_year_of_birth)

saveWorkbook(
  wb,
  file.path(out_dir, "synth_demographic_crosswalks.xlsx"),
  overwrite = TRUE
)

########## Attach Concept Name to Original Data

orig_data[["condition_occurrence"]] <- create_concept_name(
  df = orig_data[["condition_occurrence"]],
  id_col = "condition_concept_id",
  name_col = "condition_concept_name",
  concept_lu = concept_lu
)
orig_data[["visit_occurrence"]] <- create_concept_name(
  df = orig_data[["visit_occurrence"]],
  id_col = "visit_concept_id",
  name_col = "visit_concept_name",
  concept_lu = concept_lu
)
orig_data[["procedure_occurrence"]] <- create_concept_name(
  df = orig_data[["procedure_occurrence"]],
  id_col = "procedure_concept_id",
  name_col = "procedure_concept_name",
  concept_lu = concept_lu
)
orig_data[["drug_exposure"]] <- create_concept_name(
  df = orig_data[["drug_exposure"]],
  id_col = "drug_concept_id",
  name_col = "drug_concept_name",
  concept_lu = concept_lu
)
orig_data[["person"]] <- create_concept_name(
  df = orig_data[["person"]],
  id_col = "gender_concept_id",
  name_col = "gender_concept_name",
  concept_lu = concept_lu
)
orig_data[["person"]] <- create_concept_name(
  df = orig_data[["person"]],
  id_col = "ethnicity_concept_id",
  name_col = "ethnicity_concept_name",
  concept_lu = concept_lu
)
orig_data[["person"]] <- create_concept_name(
  df = orig_data[["person"]],
  id_col = "race_concept_id",
  name_col = "race_concept_name",
  concept_lu = concept_lu
)

# Write to .parquet file
# for (nm in names(orig_data)) {
#   rel <- orig_data[[nm]]
#   stopifnot(inherits(rel, "prudent_duckplyr_df"))

#   df <- dplyr::collect(rel)  # materialize to an R tibble/data.frame
#   arrow::write_parquet(df, file.path(out_dir, paste0(nm, "_orig.parquet")),
#                        compression = "zstd")
#   rm(df)
# }

# ID Check
# check one‑to‑many matches, where a single condition_concept_id maps to multiple different condition_concept_name
cat(
  "\n",
  "Count check one‑to‑many concept matches in original training data:",
  "\n"
)
cnt <- orig_data[["condition_occurrence"]] |>
  dplyr::summarize(
    n_person = n_distinct(person_id),
    n_condition_concept_id = n_distinct(condition_concept_id),
    n_condition_concept_name = n_distinct(condition_concept_name)
  )
knitr::kable(cnt)

cnt <- orig_data[["visit_occurrence"]] |>
  dplyr::summarize(
    n_person = n_distinct(person_id),
    n_visit_concept_id = n_distinct(visit_concept_id),
    n_visit_concept_name = n_distinct(visit_concept_name)
  )
knitr::kable(cnt)

cnt <- orig_data[["procedure_occurrence"]] |>
  dplyr::summarize(
    n_person = n_distinct(person_id),
    n_procedure_concept_id = n_distinct(procedure_concept_id),
    n_procedure_concept_name = n_distinct(procedure_concept_name)
  )
knitr::kable(cnt)

cnt <- orig_data[["drug_exposure"]] |>
  dplyr::summarize(
    n_person = n_distinct(person_id),
    n_drug_concept_id = n_distinct(drug_concept_id),
    n_drug_concept_name = n_distinct(drug_concept_name)
  )
knitr::kable(cnt)

crosswalk_ethnicity <- orig_data[["person"]] |>
  count(ethnicity_concept_id, ethnicity_concept_name) |>
  mutate(percentage = round(n / sum(n) * 100, 2))
crosswalk_gender <- orig_data[["person"]] |>
  count(gender_concept_id, gender_concept_name) |>
  mutate(percentage = round(n / sum(n) * 100, 2))
crosswalk_race <- orig_data[["person"]] |>
  count(race_concept_id, race_concept_name) |>
  mutate(percentage = round(n / sum(n) * 100, 2))
tab_year_of_birth <- orig_data[["person"]] |>
  count(year_of_birth) |>
  mutate(percentage = round(n / sum(n) * 100, 2))

wb <- createWorkbook()
addWorksheet(wb, "ethnicity")
writeData(wb, "ethnicity", crosswalk_ethnicity)

addWorksheet(wb, "gender")
writeData(wb, "gender", crosswalk_gender)

addWorksheet(wb, "race")
writeData(wb, "race", crosswalk_race)

addWorksheet(wb, "year_of_birth")
writeData(wb, "year_of_birth", tab_year_of_birth)

saveWorkbook(
  wb,
  file.path(out_dir, "orig_demographic_crosswalks.xlsx"),
  overwrite = TRUE
)

########## Data ID Checks

cat("\n", "ID Checks on condition_occurrence (original):", "\n")
orig_data[["condition_occurrence"]] |>
  dplyr::summarize(
    person_id_count = n_distinct(person_id),
    condition_concept_id_count = n_distinct(condition_concept_id),
    condition_occurrence_id_count = n_distinct(condition_occurrence_id)
  ) |>
  collect() |>
  knitr::kable()

cat("\n", "ID Checks on condition_occurrence (synthetic):", "\n")
synth_data[["condition_occurrence"]] |>
  dplyr::summarize(
    person_id_count = n_distinct(person_id),
    condition_concept_id_count = n_distinct(condition_concept_id),
    condition_occurrence_id_count = n_distinct(condition_occurrence_id)
  ) |>
  collect() |>
  knitr::kable()
####
cat("\n", "ID Checks on drug_exposure (original):", "\n")
orig_data[["drug_exposure"]] |>
  dplyr::summarize(
    person_id_count = n_distinct(person_id),
    drug_concept_id_count = n_distinct(drug_concept_id),
    drug_type_concept_id_count = n_distinct(drug_type_concept_id),
    visit_occurrence_id_count = n_distinct(visit_occurrence_id)
  ) |>
  collect() |>
  knitr::kable()

cat("\n", "ID Checks on drug_exposure (snythetic):", "\n")
synth_data[["drug_exposure"]] |>
  dplyr::summarize(
    person_id_count = n_distinct(person_id),
    drug_concept_id_count = n_distinct(drug_concept_id),
    drug_type_concept_id_count = n_distinct(drug_type_concept_id),
    visit_occurrence_id_count = n_distinct(visit_occurrence_id)
  ) |>
  collect() |>
  knitr::kable()
####
cat("\n", "ID Checks on person (original):", "\n")
orig_data[["person"]] |>
  dplyr::summarize(
    person_id_count = n_distinct(person_id)
  ) |>
  collect() |>
  knitr::kable()

cat("\n", "ID Checks on person (synthetic):", "\n")
synth_data[["person"]] |>
  dplyr::summarize(
    person_id_count = n_distinct(person_id)
  ) |>
  collect() |>
  knitr::kable()
####
cat("\n", "ID Checks on procedure_occurrence (original):", "\n")
orig_data[["procedure_occurrence"]] |>
  dplyr::summarize(
    person_id_count = n_distinct(person_id),
    procedure_type_concept_id_count = n_distinct(procedure_type_concept_id),
    procedure_concept_id_count = n_distinct(procedure_concept_id),
    visit_occurrence_id_count = n_distinct(visit_occurrence_id)
  ) |>
  collect() |>
  knitr::kable()

cat("\n", "ID Checks on procedure_occurrence (synthetic):", "\n")
synth_data[["procedure_occurrence"]] |>
  dplyr::summarize(
    person_id_count = n_distinct(person_id),
    procedure_type_concept_id_count = n_distinct(procedure_type_concept_id),
    procedure_concept_id_count = n_distinct(procedure_concept_id),
    visit_occurrence_id_count = n_distinct(visit_occurrence_id)
  ) |>
  collect() |>
  knitr::kable()
#####
cat("\n", "ID Checks on visit_occurrence (original):", "\n")
orig_data[["visit_occurrence"]] |>
  dplyr::summarize(
    person_id_count = n_distinct(person_id),
    visit_type_concept_id_count = n_distinct(visit_type_concept_id),
    visit_occurrence_id_count = n_distinct(visit_occurrence_id)
  ) |>
  collect() |>
  knitr::kable()

cat("\n", "ID Checks on visit_occurrence (synthetic):", "\n")
synth_data[["visit_occurrence"]] |>
  dplyr::summarize(
    person_id_count = n_distinct(person_id),
    visit_type_concept_id_count = n_distinct(visit_type_concept_id),
    visit_occurrence_id_count = n_distinct(visit_occurrence_id)
  ) |>
  collect() |>
  knitr::kable()

########## Condition flag creation
# The purpose of this step is to create person-level condition flags.

#### For Synthetic data
fusion_df <- read_excel(file.path(
  getwd(),
  "Input",
  "customize_concept_sets.xlsx"
)) |>
  dplyr::select(-concept_set_name, -Note)
df_CO <- synth_data[["condition_occurrence"]] |>
  dplyr::inner_join(
    filter(fusion_df, Table == "CO"),
    by = c("condition_concept_id" = "concept_id")
  )
# flag_p_CO <- df_CO |>
#   dplyr::select(person_id, indicator_suffix) |>
#   count(person_id, indicator_suffix, name = "n") |>
#   mutate(flag = 1) |>
#   dplyr::select(-n) |>
#   tidyr::pivot_wider(
#     names_from = indicator_suffix,
#     values_from = flag,
#     values_fill = 0,                       # fill missing with 0
#     names_prefix = "flag_"
#   )
# Replace pivot_wider() with SQL-style aggregation, ensure "no materialization in R"
flag_p_CO <- df_CO |>
  distinct(person_id, indicator_suffix, type) |>
  dplyr::summarize(
    .by = person_id,
    flag_ABUSEVICTIM = max(if_else(indicator_suffix == "ABUSEVICTIM", 1L, 0L)),
    flag_ANXIETY = max(if_else(indicator_suffix == "ANXIETY", 1L, 0L)),
    flag_CVA = max(if_else(indicator_suffix == "CVA", 1L, 0L)),
    flag_DRMISUSE = max(if_else(indicator_suffix == "DRMISUSE", 1L, 0L)),
    flag_DROD = max(if_else(indicator_suffix == "DROD", 1L, 0L)),
    flag_FACELAC = max(if_else(indicator_suffix == "FACELAC", 1L, 0L)),
    flag_LSCCONT = max(if_else(indicator_suffix == "LSCCONT", 1L, 0L)),
    flag_MISCR1 = max(if_else(indicator_suffix == "MISCR1", 1L, 0L)),
    flag_PREGNANCY = max(if_else(indicator_suffix == "PREGNANCY", 1L, 0L)),
    flag_SCISOLT = max(if_else(indicator_suffix == "SCISOLT", 1L, 0L)),
    flag_SPRFXA = max(if_else(indicator_suffix == "SPRFXA", 1L, 0L)),
    flag_SPRFXWFA = max(if_else(indicator_suffix == "SPRFXWFA", 1L, 0L)),
    flag_STRESS = max(if_else(indicator_suffix == "STRESS", 1L, 0L))
  )

df_DE <- synth_data[["drug_exposure"]] |>
  dplyr::inner_join(
    filter(fusion_df, Table == "DE"),
    by = c("drug_concept_id" = "concept_id")
  )
flag_p_DE <- df_DE |>
  distinct(person_id, indicator_suffix, type) |>
  dplyr::summarize(
    .by = person_id,
    flag_OPIOIDDRUG = max(if_else(indicator_suffix == "OPIOIDDRUG", 1L, 0L)),
    flag_STDDRUG = max(if_else(indicator_suffix == "STDDRUG", 1L, 0L))
  )

df_PO <- synth_data[["procedure_occurrence"]] |>
  dplyr::inner_join(
    filter(fusion_df, Table == "PO"),
    by = c("procedure_concept_id" = "concept_id")
  )
flag_p_PO <- df_PO |>
  distinct(person_id, indicator_suffix, type) |>
  dplyr::summarize(
    .by = person_id,
    flag_BHPPROC = max(if_else(indicator_suffix == "BHPPROC", 1L, 0L)),
    flag_DXLPPROC = max(if_else(indicator_suffix == "DXLPPROC", 1L, 0L)),
    flag_OBGYNPROC = max(if_else(indicator_suffix == "OBGYNPROC", 1L, 0L)),
    flag_ONCPROC = max(if_else(indicator_suffix == "ONCPROC", 1L, 0L)),
    flag_PGSPROC = max(if_else(indicator_suffix == "PGSPROC", 1L, 0L))
  )

df_VO <- synth_data[["visit_occurrence"]] |>
  dplyr::inner_join(
    filter(fusion_df, Table == "VO"),
    by = c("visit_concept_id" = "concept_id")
  )
flag_p_VO <- df_VO |>
  distinct(person_id, indicator_suffix, type) |>
  dplyr::summarize(
    .by = person_id,
    flag_INPATIENTVISIT = max(if_else(
      indicator_suffix == "INPATIENTVISIT",
      1L,
      0L
    ))
  )

cat("\n", "Number of persons with specified conditions in synthetic data", "\n")
knitr::kable(dplyr::summarize(flag_p_CO, n = dplyr::n()))
cat("\n", "Number of persons with specified drugs in synthetic data", "\n")
knitr::kable(dplyr::summarize(flag_p_DE, n = dplyr::n()))
cat("\n", "Number of persons with specified procedures in synthetic data", "\n")
knitr::kable(dplyr::summarize(flag_p_PO, n = dplyr::n()))
cat("\n", "Number of persons with inpatient in synthetic data", "\n")
knitr::kable(dplyr::summarize(flag_p_VO, n = dplyr::n()))

####
synth_p <- synth_data[["person"]] |>
  dplyr::left_join(flag_p_CO, by = "person_id") |>
  dplyr::left_join(flag_p_DE, by = "person_id") |>
  dplyr::left_join(flag_p_PO, by = "person_id") |>
  dplyr::left_join(flag_p_VO, by = "person_id") |>
  dplyr::mutate(across(starts_with("flag_"), ~ dplyr::coalesce(.x, 0L)))
cat("\n", "Number of rows in synthetic person data BEFORE merging", "\n")
print(dplyr::summarize(synth_data[["person"]], n = dplyr::n()))
cat("\n", "Number of rows in synthetic person data AFTER merging", "\n")
print(dplyr::summarize(synth_p, n = dplyr::n()))

#### For Original data
df_CO <- orig_data[["condition_occurrence"]] |>
  dplyr::inner_join(
    filter(fusion_df, Table == "CO"),
    by = c("condition_concept_id" = "concept_id")
  )
flag_p_CO <- df_CO |>
  distinct(person_id, indicator_suffix, type) |>
  dplyr::summarize(
    .by = person_id,
    flag_ABUSEVICTIM = max(if_else(indicator_suffix == "ABUSEVICTIM", 1L, 0L)),
    flag_ANXIETY = max(if_else(indicator_suffix == "ANXIETY", 1L, 0L)),
    flag_CVA = max(if_else(indicator_suffix == "CVA", 1L, 0L)),
    flag_DRMISUSE = max(if_else(indicator_suffix == "DRMISUSE", 1L, 0L)),
    flag_DROD = max(if_else(indicator_suffix == "DROD", 1L, 0L)),
    flag_FACELAC = max(if_else(indicator_suffix == "FACELAC", 1L, 0L)),
    flag_LSCCONT = max(if_else(indicator_suffix == "LSCCONT", 1L, 0L)),
    flag_MISCR1 = max(if_else(indicator_suffix == "MISCR1", 1L, 0L)),
    flag_PREGNANCY = max(if_else(indicator_suffix == "PREGNANCY", 1L, 0L)),
    flag_SCISOLT = max(if_else(indicator_suffix == "SCISOLT", 1L, 0L)),
    flag_SPRFXA = max(if_else(indicator_suffix == "SPRFXA", 1L, 0L)),
    flag_SPRFXWFA = max(if_else(indicator_suffix == "SPRFXWFA", 1L, 0L)),
    flag_STRESS = max(if_else(indicator_suffix == "STRESS", 1L, 0L))
  )

df_DE <- orig_data[["drug_exposure"]] |>
  dplyr::inner_join(
    filter(fusion_df, Table == "DE"),
    by = c("drug_concept_id" = "concept_id")
  )
flag_p_DE <- df_DE |>
  distinct(person_id, indicator_suffix, type) |>
  dplyr::summarize(
    .by = person_id,
    flag_OPIOIDDRUG = max(if_else(indicator_suffix == "OPIOIDDRUG", 1L, 0L)),
    flag_STDDRUG = max(if_else(indicator_suffix == "STDDRUG", 1L, 0L))
  )

df_PO <- orig_data[["procedure_occurrence"]] |>
  dplyr::inner_join(
    filter(fusion_df, Table == "PO"),
    by = c("procedure_concept_id" = "concept_id")
  )
flag_p_PO <- df_PO |>
  distinct(person_id, indicator_suffix, type) |>
  dplyr::summarize(
    .by = person_id,
    flag_BHPPROC = max(if_else(indicator_suffix == "BHPPROC", 1L, 0L)),
    flag_DXLPPROC = max(if_else(indicator_suffix == "DXLPPROC", 1L, 0L)),
    flag_OBGYNPROC = max(if_else(indicator_suffix == "OBGYNPROC", 1L, 0L)),
    flag_ONCPROC = max(if_else(indicator_suffix == "ONCPROC", 1L, 0L)),
    flag_PGSPROC = max(if_else(indicator_suffix == "PGSPROC", 1L, 0L))
  )

df_VO <- orig_data[["visit_occurrence"]] |>
  dplyr::inner_join(
    filter(fusion_df, Table == "VO"),
    by = c("visit_concept_id" = "concept_id")
  )
flag_p_VO <- df_VO |>
  distinct(person_id, indicator_suffix, type) |>
  dplyr::summarize(
    .by = person_id,
    flag_INPATIENTVISIT = max(if_else(
      indicator_suffix == "INPATIENTVISIT",
      1L,
      0L
    ))
  )

cat("\n", "Number of persons with specified conditions in original data", "\n")
knitr::kable(dplyr::summarize(flag_p_CO, n = dplyr::n()))
cat("\n", "Number of persons with specified drugs in original data", "\n")
knitr::kable(dplyr::summarize(flag_p_DE, n = dplyr::n()))
cat("\n", "Number of persons with specified procedures in original data", "\n")
knitr::kable(dplyr::summarize(flag_p_PO, n = dplyr::n()))
cat("\n", "Number of persons with inpatient in original data", "\n")
knitr::kable(dplyr::summarize(flag_p_VO, n = dplyr::n()))

####
orig_p <- orig_data[["person"]] |>
  dplyr::left_join(flag_p_CO, by = "person_id") |>
  dplyr::left_join(flag_p_DE, by = "person_id") |>
  dplyr::left_join(flag_p_PO, by = "person_id") |>
  dplyr::left_join(flag_p_VO, by = "person_id") |>
  dplyr::mutate(across(starts_with("flag_"), ~ dplyr::coalesce(.x, 0L)))
cat("\n", "Number of rows in original person data BEFORE merging", "\n")
print(dplyr::summarize(orig_data[["person"]], n = dplyr::n()))
cat("\n", "Number of rows in original person data AFTER merging", "\n")
print(dplyr::summarize(orig_p, n = dplyr::n()))
