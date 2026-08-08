library(haven)
library(dplyr)
library(survey)
library(srvyr)
library(sparklyr)

# Load imputed L-SDR data ----

  lsdr_imp <- read_sas(
    "//westat.com/dfs/SDR_NCSES/7. Data Delivery and Dissemination/StatConfid/2023/Task 2/data/Scenario_1/lsdr_imp_final.sas7bdat"
  )
  
  lsdr_pert1 <- read_sas(
    "//westat.com/archive/SDR_ARCHIVE/September2023/7. Data Delivery and Dissemination/StatConfid/2023/Task 2/data/Scenario_1/lsdr_sdcpert_final.sas7bdat"
  )
  
  lsdr_stacked <- bind_rows(
    'imp'   = lsdr_imp, 
    'pert1' = lsdr_pert1,
    .id     = ".dataset"
  )

# Identify changed variables ----
  changed_vars = names(lsdr_stacked)[grepl('_3', names(lsdr_stacked))] %>% 
    substr(., 1, nchar(.)-2)
  changed_vars = changed_vars[!grepl('WTLONG', changed_vars)&
                                !grepl('change', changed_vars)&
                                !grepl('CHANGE', changed_vars)&
                                !grepl('diff', changed_vars)]
  
# Add explicit missing values ----
  
  missing_vars = c('SALARY_15', 'SALARY_17', 'SALARY_19', 'EARN_15', 'EARN_17', 'EARN_19')

  for(var in names(lsdr_stacked)[grepl(paste0(missing_vars, collapse = '|'), names(lsdr_stacked))]){
    lsdr_stacked[[var]] = ifelse(lsdr_stacked[[var]]==9999998, NA, lsdr_stacked[[var]])
  }

# Derive variables of analytic interest ----
  
  # Categorical variable for years since PhD 
  lsdr_stacked$YRSPHD = ifelse(2019 - lsdr_stacked$SDRYR <= 10, 1,
                           ifelse(2019 - lsdr_stacked$SDRYR <= 20, 2,
                                  ifelse(2019 - lsdr_stacked$SDRYR <= 30, 3,
                                         ifelse(2019 - lsdr_stacked$SDRYR <= 100, 4, 0))))
  
  # Unemployment spells
  lsdr_stacked$UNEMP_SPELLS = I(lsdr_stacked$LFSTAT_15==2) + 
    I(lsdr_stacked$LFSTAT_17==2) +
    I(lsdr_stacked$LFSTAT_19==2) 
  
  lsdr_stacked$UNEMP_SPELLS = as.numeric(lsdr_stacked$UNEMP_SPELLS)
  
  # Sector changes
  lsdr_stacked$SECTOR_CHANGES = I(lsdr_stacked$EMSECDT_15 != lsdr_stacked$EMSECDT_17) +
    I(lsdr_stacked$EMSECDT_17 != lsdr_stacked$EMSECDT_19)
  lsdr_stacked$SECTOR_CHANGES = as.numeric(lsdr_stacked$SECTOR_CHANGES)
  for(i in 1:5){
    lsdr_stacked[[paste0('SECTOR_CHANGES_', i)]] = 
      as.numeric( I(lsdr_stacked[[paste0('EMSECDT_15_',i)]] != 
                      lsdr_stacked[[paste0('EMSECDT_17_', i)]]) +
                    I(lsdr_stacked[[paste0('EMSECDT_17_',i)]] != 
                        lsdr_stacked[[paste0('EMSECDT_19_', i)]]) )
  }
  
  # LOC Changes
  lsdr_stacked$LOC_CHANGES = as.numeric( I(lsdr_stacked$FNINUS_15 != lsdr_stacked$FNINUS_17) +
                                       I(lsdr_stacked$FNINUS_17 != lsdr_stacked$FNINUS_19) )
  
  # I In Field
  lsdr_stacked$I_INFIELD_15 = ifelse(lsdr_stacked$OCEDRLP_15 %in% c(1, 2), 1,
                                 ifelse(lsdr_stacked$OCEDRLP_15==3, 2, 0))
  
  # I2 In Field
  lsdr_stacked$I2_INFIELD_15 = ifelse(lsdr_stacked$OCEDRLP_15 %in% c('1','2'), 1,
                                  ifelse(lsdr_stacked$OCEDRLP_15=='3' &
                                           lsdr_stacked$NROCNA_15=='N', 2,
                                         ifelse(lsdr_stacked$OCEDRLP_15=='3' & 
                                                  lsdr_stacked$NROCNA_15=='Y', 3, 0)))
  
  lsdr_stacked$I2_INFIELD_17 = ifelse(lsdr_stacked$OCEDRLP_17 %in% c('1','2'), 1,
                                  ifelse(lsdr_stacked$OCEDRLP_17=='3' &
                                           lsdr_stacked$NROCNA_17=='N', 2,
                                         ifelse(lsdr_stacked$OCEDRLP_17=='3' & 
                                                  lsdr_stacked$NROCNA_17=='Y', 3, 0)))
  
  lsdr_stacked$I2_INFIELD_19 = ifelse(lsdr_stacked$OCEDRLP_19 %in% c('1','2'), 1,
                                  ifelse(lsdr_stacked$OCEDRLP_19=='3' &
                                           lsdr_stacked$NROCNA_19=='N', 2,
                                         ifelse(lsdr_stacked$OCEDRLP_19=='3' & 
                                                  lsdr_stacked$NROCNA_19=='Y', 3, 0)))
  
# Split the stacked data ----
  
  lsdr_imp   <- lsdr_stacked |> filter(.dataset == "imp")
  lsdr_pert1 <- lsdr_stacked |> filter(.dataset == "imp")
  rm(lsdr_stacked)
  
# Create a survey design object ----
  
  lsdr_surv = svrepdesign(
    lsdr_imp,
    repweights = lsdr_imp[,paste0('WTLONG1519_', 1:104)],
    weights    = lsdr_imp$WTLONG1519,
    type       = 'other',
    scale      = 4/104,
    rscales    = rep(1, times = 104)
  ) |> srvyr::as_survey_rep()
  
# Clean up namespace ----
  
  rm(list = c("changed_vars", "missing_vars", "i", "var"))
  
# Create a Spark cluster with the SDR data ----
  
  Sys.setenv("SPARK_MEM" = "13g")
  # Set driver and executor memory allocations
  config <- spark_config()
  config$spark.driver.memory <- "4G"
  config$spark.executor.memory <- "1G"
  
  sc <- spark_connect(master = "local", version = "3.5.5", config = config)

  spark_lsdr_imp   <- copy_to(dest = sc, df = lsdr_imp, name = "lsdr_imp")
  spark_lsdr_pert1 <- copy_to(dest = sc, df = lsdr_pert1, name = "lsdr_pert1")
  