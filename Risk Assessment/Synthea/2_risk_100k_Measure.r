################################ 2_risk_100k_Measure.r ##########################################

################################ Introduction
# This script is to:
# Run the risk assessment using the person-level files (both original and synthetic) that are created from 1_risk_100k_DataPrep.r
# The risk assessment on (1) Identity disclosure (2) Attribute disclosure (3) Exact match (4) Outliers
# The risk measures are calculated and documented in "disclosure_measure_functions.R"

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
# library(arrow) # read parquet files
library(dplyr)
library(knitr)
library(duckplyr)
library(openxlsx)
library(readxl)

###################################### Functions ######################################

source(file.path(getwd(), "disclosure_measure_functions.R"))

###################################### Risk Measure ######################################

keys_var <- c(
  "gender_concept_name",
  "year_of_birth",
  "race_concept_name",
  "flag_FACELAC",
  "flag_PREGNANCY",
  "flag_SPRFXA",
  "flag_SPRFXWFA",
  "flag_INPATIENTVISIT"
)
cat("\n", "Key Variables:", "\n")
print(keys_var)

targets_var <- c(
  "flag_ABUSEVICTIM",
  "flag_ANXIETY",
  "flag_CVA",
  "flag_DRMISUSE",
  "flag_DROD",
  "flag_LSCCONT",
  "flag_MISCR1",
  "flag_SCISOLT",
  "flag_STRESS",
  "flag_OPIOIDDRUG",
  "flag_STDDRUG",
  "flag_BHPPROC",
  "flag_DXLPPROC",
  "flag_OBGYNPROC",
  "flag_ONCPROC",
  "flag_PGSPROC"
)
cat("\n", "Target Variables:", "\n")
print(targets_var)

########## Identity disclosure

IR_out <- get_identity_disclosure(
  data_s = synth_p,
  data_o = orig_p,
  keys = keys_var
)
cat("\n", "Identity Disclosure Risk:", "\n")
print(IR_out)
df <- as.data.frame(IR_out)

wb <- createWorkbook()
addWorksheet(wb, "identity")
writeData(wb, "identity", df)

saveWorkbook(wb, file.path(out_dir, "risk_measures.xlsx"), overwrite = TRUE)

########## Attribute disclosure

out <- setNames(vector("list", length(targets_var)), targets_var)

for (v in targets_var) {
  AR_out <- get_attribute_disclosure(
    data_s = synth_p,
    data_o = orig_p,
    keys = keys_var,
    target = v
  )

  df <- data.frame(
    Measure = names(AR_out),
    Percent = unlist(AR_out, use.names = FALSE)
  )

  out[[v]] <- df
  cat("\n", "Attribute disclosure on the Target Variable: ", v, "\n")
  print(knitr::kable(df))
}

result <- Reduce(
  function(x, y) merge(x, y, by = "Measure", all = TRUE),
  lapply(names(out), function(nm) {
    df <- out[[nm]]
    names(df)[names(df) == "Percent"] <- paste0("Percent_", nm)
    df
  })
)

addWorksheet(wb, "attribute")
writeData(wb, "attribute", result)

saveWorkbook(wb, file.path(out_dir, "risk_measures.xlsx"), overwrite = TRUE)

########## Exact Match

EM_out <- get_exact_match(
  data_s = synth_p,
  data_o = orig_p,
  vars = c(keys_var, targets_var)
)
cat("\n", "Exact Match % on the Key Variables: ", "\n")
knitr::kable(EM_out)

df <- data.frame(Measure = "Exact Match", EM_pcnt = EM_out)

addWorksheet(wb, "Exact Match")
writeData(wb, "Exact Match", df)

saveWorkbook(wb, file.path(out_dir, "risk_measures.xlsx"), overwrite = TRUE)

########## Outlier Check

out <- setNames(vector("list", length(targets_var)), targets_var)
for (v in targets_var) {
  O_out <- get_outlier_c(
    data_s = synth_p,
    data_o = orig_p,
    keys = keys_var,
    target = v,
    threshold = 5
  )

  df <- data.frame(
    Measure = "Outlier",
    outlier_pcnt = unlist(O_out, use.names = FALSE)
  )
  out[[v]] <- df
  cat("\n", "Outlier % on keys + Target Variable: ", v, "\n")
  knitr::kable(df)
}

result <- Reduce(
  function(x, y) merge(x, y, by = "Measure", all = TRUE),
  lapply(names(out), function(nm) {
    df <- out[[nm]]
    names(df)[names(df) == "outlier_pcnt"] <- paste0("Percent_", nm)
    df
  })
)
knitr::kable(result)
addWorksheet(wb, "Outlier P")
writeData(wb, "Outlier P", result)

saveWorkbook(wb, file.path(out_dir, "risk_measures.xlsx"), overwrite = TRUE)
