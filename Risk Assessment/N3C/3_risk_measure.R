################################ 3_risk_measure.r ##########################################

################################ Introduction
# This R script is running in N3C Enclave Code Workspace:
# Run the risk assessment using the person-level files (both original and synthetic) that are created from 2_input_file.R
# The risk assessment on (1) Identity disclosure (2) Attribute disclosure (3) Exact match (4) Outliers
# The risk measures are calculated and documented in "disclosure_measure_functions.R"

######## Library  ##########

library(dplyr)
library(knitr)
# library(duckplyr)
# library(openxlsx)
# library(readxl)

###################################### Functions ######################################

source(file.path(getwd(), "disclosure_measure_functions.R"))

###################################### Risk Measure ######################################
synth_p <- datasets.read_table("person_r_s")
orig_p <- datasets.read_table("person_r")

keys_var <- c(
  "sex",
  "age",
  "race_r",
  #"flag_FACELAC",
  #"flag_PREGNANCY",
  "flag_SPRFXA",
  "flag_SPRFXWFA",
  "flag_INPATIENTVISIT",
  "flag_OBESITY",
  "flag_DEATH"
)
cat("\n", "Key Variables:", "\n")
print(keys_var)

targets_var <- c(
  "flag_ALCD",
  "flag_AMNESIA",
  "flag_ANXTYD",
  "flag_BIPOLARD",
  "flag_BRAIND",
  "flag_DEMENTIA",
  "flag_DPRESSD",
  "flag_HYSTERECTOMY",
  "flag_LD",
  "flag_MDE",
  "flag_MRMJRDPRESS",
  "flag_OPIOIDDRUG",
  "flag_UI"
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
datasets.write_table(df, "identity_disclosure_risk")
# wb <- createWorkbook()
# addWorksheet(wb, "identity")
# writeData(wb, "identity", df)
#
# saveWorkbook(wb, file.path(out_dir,"risk_measures.xlsx"), overwrite = TRUE)

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

datasets.write_table(result, "attribute_disclosure_risk")
# addWorksheet(wb, "attribute")
# writeData(wb, "attribute", result)
#
# saveWorkbook(wb, file.path(out_dir,"risk_measures.xlsx"), overwrite = TRUE)

########## Exact Match

EM_out <- get_exact_match(
  data_s = synth_p,
  data_o = orig_p,
  vars = c(keys_var, targets_var)
)
cat("\n", "Exact Match % on the Key Variables: ", "\n")
knitr::kable(EM_out)

df <- data.frame(Measure = "Exact Match", EM_pcnt = EM_out)

datasets.write_table(df, "exact_match_risk")
# addWorksheet(wb, "Exact Match")
# writeData(wb, "Exact Match", df)
#
# saveWorkbook(wb, file.path(out_dir,"risk_measures.xlsx"), overwrite = TRUE)

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

datasets.write_table(result, "outlier_result")
# addWorksheet(wb, "Outlier P")
# writeData(wb, "Outlier P", result)
#
# saveWorkbook(wb, file.path(out_dir,"risk_measures.xlsx"), overwrite = TRUE)
