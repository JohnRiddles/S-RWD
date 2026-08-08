################################################################################################################
################## DISTRIBUTION DISTANCES ######################################################################
################################################################################################################
################################################################################################################

#' @title Internal helper function to compute Hellinger's distance
#' @description Helper function to compute 
#' Hellinger's Distance given a data frame
#' containing two different empirical distributions
#' @param data A data frame with one row
#' per category of a variable of interest (e.g., PhD field).
#' @param p_var A character string giving the name
#' of the variable containing an empirical distribution
#' function's values (e.g., sample percentages from original data).
#' @param q_var A character string giving the name
#' of a different variable containing an alternative
#' empirical distribution function's values (
#' e.g., sample percentages from synthetic data).
#' @returns A data frame with the column named 'HD',
#' giving Hellinger's Distance
#' @keywords internal
HD <- function(data, p_var, q_var) {
  data |>
    mutate(P = !!sym(p_var)/sum(!!sym(p_var)),
           Q = !!sym(q_var)/sum(!!sym(q_var))) |>
    summarize(
      HD = (1/sqrt(2)) * sqrt(sum((sqrt(P) - sqrt(Q))^2))
    )
}

#' @title Internal helper function to compute Hellinger's distance when variables do not come from the same data set
#' @description Helper function to compute 
#' Hellinger's Distance given two different vectors.
#' Taken directly from 'dad' package.
#' @param x1 A categorical vector
#' @param x2 A categorical vector
#' @returns A data frame with the column named 'HD',
#' giving Hellinger's Distance
#' @keywords internal
ddhellinger = function (x1, x2) {
  if (is.vector(x1)) 
    x1 <- data.frame(x = x1, stringsAsFactors = TRUE)
  if (is.vector(x2)) 
    x2 <- data.frame(x = x2, stringsAsFactors = TRUE)
  if (!is.data.frame(x1)) 
    stop("x1 must be a data frame or a vector.")
  if (!is.data.frame(x2)) 
    stop("x2 must be a data frame or a vector.")
  k <- ncol(x1)
  if (ncol(x2) != k) 
    stop("ncol(x1) != ncol(x2)")
  if (!identical(colnames(x1), colnames(x2))) 
    warning("x1 and x2 do not have the same column names.")
  x1 <- as.data.frame(x1, stringsAsFactors = TRUE)
  x2 <- as.data.frame(x2, stringsAsFactors = TRUE)
  for (j in 1:ncol(x1)) {
    if (!is.factor(x1[, j])) 
      x1[, j] <- as.factor(x1[, j])
    if (!is.factor(x2[, j])) 
      x2[, j] <- as.factor(x2[, j])
    lev <- union(levels(x1[, j]), levels(x2[, j]))
    x1[, j] <- factor(as.character(x1[, j]), levels = lev)
    x2[, j] <- factor(as.character(x2[, j]), levels = lev)
  }
  p1 <- table(x1)/nrow(x1)
  p2 <- table(x2)/nrow(x2)
  return(ddhellingerpar(p1, p2))
}



#' @title Internal helper function to compute weighted summary statistics
#' @description Summarize the absolute differences between 
#' a variable and its imputed or synthetic version
#' @param data A data frame or data frame extension (e.g., a \code{tbl_spark} object).
#' @param var A character string giving the name of a variable in the data
#' @param new_suff A suffix used to distinguish the imputed variable from the original variable
#' @param og_suff The suffix for the original variable
#'
#' @returns A data frame with one column per summary statistic
#' @keywords internal
#' @examples
#' 
#' example_data <- data.frame(
#'   'SALARY_15' = rnorm(n = 10, mean = 75000, sd = 5000),
#'   'SALARY_15_1' = rnorm(n = 10, mean = 75000, sd = 5000)
#' )
#' 
#' summ_stats_var(
#'   data     = example_data, 
#'   var      = "SALARY_15", 
#'   new_suff = "_1", 
#'   og_suff  = ""
#' )
#' 
summ_stats_var <- function(data, var, new_suff, og_suff) {
  var_orig  <- paste0(var, og_suff)
  var_new   <- paste0(var, new_suff)
  
  na_count <- data |> 
    filter(is.na(!!sym(var_orig)) | is.na(!!sym(var_new))) |>
    summarize(N = n()) |>
    pull("N")
  
  data |> 
    filter(!is.na(!!sym(var_orig)), !is.na(!!sym(var_new))) |>
    mutate(DATA_TMP_DIST_VAR = abs(!!sym(var_orig) - !!sym(var_new))) |>
    summarize(Min    = min(DATA_TMP_DIST_VAR),
              Q1     = quantile(DATA_TMP_DIST_VAR, probs = 0.25),
              Median = quantile(DATA_TMP_DIST_VAR, probs = 0.5),
              Mean   = mean(DATA_TMP_DIST_VAR),
              Q3     = quantile(DATA_TMP_DIST_VAR, probs = 0.75),
              Max    = max(DATA_TMP_DIST_VAR)) |>
    collect() |>
    mutate(NA_Count = na_count)
}

#' @title Internal helper function to compute weighted summary statistics
#' @description Weighted summaries of the absolute differences between 
#' a variable and its imputed or synthetic version
#' @param data A data frame or data frame extension (e.g., a \code{tbl_spark} object).
#' @param var A character string giving the name of a variable in the data
#' @param wgt A character string giving the name of a weight variable in the data
#' @param new_suff A suffix used to distinguish the imputed variable from the original variable
#' @param og_suff The suffix for the original variable
#' @returns A data frame with one column per weighted summary statistic
#' @keywords internal
#' @examples
#' example_data <- data.frame(
#'   'SALARY_15' = rnorm(n = 10, mean = 75000, sd = 5000),
#'   'SALARY_15_1' = rnorm(n = 10, mean = 75000, sd = 5000),
#'   'WTLONG1519' = runif(n = 10, 5, 20)
#' )
#' 
#' wgt_summ_stats_var(
#'   data     = example_data, 
#'   var      = "SALARY_15", 
#'   wgt      = "WTLONG1519",
#'   new_suff = "_1", 
#'   og_suff  = ""
#' )
#' 
wgt_summ_stats_var <- function(data, var, wgt, new_suff, og_suff) {
  
  var_orig  <- paste0(var, og_suff)
  var_new   <- paste0(var, new_suff)
  
  na_count <- data |> 
    filter(is.na(!!sym(var_orig)) | is.na(!!sym(var_new))) |>
    filter(!!sym(wgt) > 0) |>
    summarize(N = n()) |>
    pull("N")
  
  if (!inherits(data, "tbl_spark")) {
    
    result <- data |> 
      filter(!is.na(!!sym(var_orig)), !is.na(!!sym(var_new))) |>
      mutate(DATA_TMP_DIST_VAR = abs(!!sym(var_orig) - !!sym(var_new))) |>
      mutate(DATA_TMP_WGT_VAR   = !!sym(wgt)) |>
      summarize(W_Min    = min(DATA_TMP_DIST_VAR),
                W_Q1     = survey:::qrule_hf4(DATA_TMP_DIST_VAR, w = DATA_TMP_WGT_VAR, p = 0.25),
                W_Median = survey:::qrule_hf4(DATA_TMP_DIST_VAR, w = DATA_TMP_WGT_VAR, p = 0.5),
                W_Mean   = weighted.mean(DATA_TMP_DIST_VAR, DATA_TMP_WGT_VAR),
                W_Q3     = survey:::qrule_hf4(DATA_TMP_DIST_VAR, w = DATA_TMP_WGT_VAR, p = 0.75),
                W_Max    = max(DATA_TMP_DIST_VAR)) |>
      collect() |>
      mutate(W_NA_Count = na_count)
    
  } else {
    
    data_for_query <- data |> 
      filter(!is.na(!!sym(var_orig)), !is.na(!!sym(var_new))) |>
      filter(!!sym(wgt) > 0) |>
      mutate(DATA_TMP_DIST_VAR = abs(!!sym(var_orig) - !!sym(var_new))) |>
      mutate(DATA_TMP_WGT_VAR   = !!sym(wgt))
    
    simple_stats <- data_for_query |>
      summarize(W_Min    = min(DATA_TMP_DIST_VAR),
                W_Mean   = sum(DATA_TMP_DIST_VAR * DATA_TMP_WGT_VAR)/sum(DATA_TMP_WGT_VAR),
                W_Max    = max(DATA_TMP_DIST_VAR)) |>
      collect() |>
      mutate(W_NA_Count = na_count)
    
    quantile_stats <- sparklyr::sdf_quantile(
      x             = compute(data_for_query), 
      column        = "DATA_TMP_DIST_VAR", 
      weight.column = "DATA_TMP_WGT_VAR",
      probabilities = c(0.25, 0.5, 0.75)
    )
    
    result <- simple_stats |>
      mutate(W_Q1     = quantile_stats[1],
             W_Median = quantile_stats[2],
             W_Q3     = quantile_stats[3]) |>
      select(W_Min, W_Q1, W_Median, W_Mean, W_Q3, W_Max, W_NA_Count)
  }
  return(result)
}

#' @title Internal helper function to compute summary statistics for a single variable 
#' in a single dataset
#' @description Summary statistics for a single variable
#' @param data A data frame or data frame extension (e.g., a \code{tbl_spark} object).
#' @param var A character string giving the name of a variable in the data
#'
#' @returns A data frame with one column per summary statistic
#' @keywords internal
#' @examples
#' 
#' example_data <- data.frame(
#'   'SALARY_15' = rnorm(n = 10, mean = 75000, sd = 5000)
#' )
#' 
#' cont_summary_stats(
#'   data     = example_data, 
#'   var      = "SALARY_15"
#' )
#' 
cont_summary_stats <- function(data, var) {
  
  data |> 
    mutate(DATA_TMP_VAR = as.numeric(!!sym(var))) |>
    filter(!is.na(DATA_TMP_VAR)) |>
    summarize(Min    = min(DATA_TMP_VAR),
              Q01    = quantile(DATA_TMP_VAR, probs = 0.01),
              Q05    = quantile(DATA_TMP_VAR, probs = 0.05),
              Q10    = quantile(DATA_TMP_VAR, probs = 0.10),
              Q25    = quantile(DATA_TMP_VAR, probs = 0.25),
              Median = quantile(DATA_TMP_VAR, probs = 0.5),
              Q75    = quantile(DATA_TMP_VAR, probs = 0.75),
              Q90    = quantile(DATA_TMP_VAR, probs = 0.90),
              Q95    = quantile(DATA_TMP_VAR, probs = 0.95),
              Q99     = quantile(DATA_TMP_VAR, probs = 0.99),
              Mean   = mean(DATA_TMP_VAR),
              Max    = max(DATA_TMP_VAR),
              Sd     = sd(DATA_TMP_VAR)) |>
    collect()
}

#' @title Utility measure: distance for categorical variables
#' @description Computes measures of distance
#' between the distributions of categorical variables
#' in the original versus new data. Currently only supports
#' Hellinger's distance.
#' @param orig_data A data frame or data frame extension (e.g., a \code{tbl_spark} object)
#' containing the original data.
#' @param new_data A data frame or data frame extension (e.g., a \code{tbl_spark} object)
#' containing the data from imputation or synthesis.
#' @param vars A character vector of categorical variables' names
#' to compared.
#' @param wgt An optional character string giving the name
#' of a weight variable (which must be present in both datasets)
#' to use for comparing weighted distributions.
#' @param new_suffixes An optional character vector
#' giving the suffixes used to denote different
#' implicates in \code{new_data}.
#'
#' @returns A data frame with one row per variable analyzed,
#' and columns \code{"HD"} and \code{"W_HD"} giving
#' Hellinger's distance for the unweighted distributions
#' and weighted distributions, respectively.
#'
#' @details
#' Hellinger's distance is computed for a variable \eqn{X}
#' with \eqn{k} categories. The distance depends on the 
#' vectors of estimated proportions in two datasets,
#' denoted \eqn{P = (p_1,\dots,p_k)} and \eqn{Q = (q_1,\dots,q_k)}.
#' The distance is defined as follows:
#' \deqn{
#'   HD = \frac{1}{\sqrt{2}} \sqrt{\sum_{i=1}^{k} \left(\sqrt{p_i} - \sqrt{q_i}\right)^2}
#' }
#' Other software packages (e.g., the R package 'dad') 
#' sometimes omit the term \eqn{\frac{1}{\sqrt(2)}}.
#' 
#' If a weights variable is specified, then the weighted Hellinger's distance (`W_HD`)
#' is computed using the formula above, but applied to proportions estimated using the weights.
#' 
#' @export
#' @examples
#' 
#' # Example of distances between categorical variables
#' 
#' orig_data <- data.frame(
#'   'RACETHM' = sample(1:4, size = 100, replace = TRUE),
#'   'WGT'     = runif(n = 100, 10, 15)
#' )
#' 
#' new_data <- data.frame(
#'   'RACETHM' = sample(1:4, size = 100, replace = TRUE),
#'   'WGT'     = runif(n = 100, 10, 15)
#' )
#' 
#' dist_btwn_cat_vars(
#'   orig_data    = orig_data,
#'   new_data     = new_data,
#'   vars         = "RACETHM",
#'   wgt          = "WGT"
#' )
#' 
#' # Example when the synthetic data has multiple implicates
#' new_data_multiple_implicates <- data.frame(
#'   'RACETHM_1' = sample(1:4, size = 100, replace = TRUE),
#'   'RACETHM_2' = sample(1:4, size = 100, replace = TRUE),
#'   'RACETHM_3' = sample(1:4, size = 100, replace = TRUE),
#'   'RACETHM_4' = sample(1:4, size = 100, replace = TRUE),
#'   'RACETHM_5' = sample(1:4, size = 100, replace = TRUE),
#'   'WGT'       = runif(n = 100, min = 10, max = 15)
#' )
#'
#' dist_btwn_cat_vars(
#'   orig_data    = orig_data,
#'   new_data     = new_data_multiple_implicates,
#'   vars         = "RACETHM",
#'   wgt          = "WGT",
#'   new_suffixes = paste0("_", 1:5)
#' )
#' 
dist_btwn_cat_vars <- function(
    orig_data, 
    new_data, 
    vars, 
    wgt = NULL,
    new_suffixes = NULL
) {
  
  if (is.null(new_suffixes)) {
    new_suffixes <- ""
  }
  
  if (!is.null(wgt)) {
    new_data <- new_data   |> mutate(DATA_TMP_WGT_VAR = !!sym(wgt))
    orig_data <- orig_data |> mutate(DATA_TMP_WGT_VAR = !!sym(wgt))
  } else {
    new_data <- new_data   |> mutate(DATA_TMP_WGT_VAR = 1)
    orig_data <- orig_data |> mutate(DATA_TMP_WGT_VAR = 1)
  }
  
  combined_results <- NULL
  for (var_name in vars) {
    
    if (!var_name %in% colnames(orig_data)) {
      sprintf("`new_data` does not contain the variable `%s`", var_name) |>
        stop()
    }
    
    for (new_suffix in new_suffixes) {
      
      new_var_name <- paste0(var_name, new_suffix)
      if (!new_var_name %in% colnames(new_data)) {
        error_msg <- sprintf(
          fmt = "The variable `%s` is not in `new_data`. Please check the arguments `vars` and `new_suffixes`.",
          new_var_name
        )
        stop(error_msg)
      }
      
      # Counts and proportions in orig_data
      orig_counts <- orig_data |>
        mutate(DATA_TMP_VAR = as.character(!!sym(var_name))) |>
        summarize(N_ORIG = n(),
                  W_N_ORIG = sum(DATA_TMP_WGT_VAR),
                  .by = DATA_TMP_VAR) %>%
        mutate(P_ORIG = N_ORIG/sum(N_ORIG),
               W_P_ORIG = W_N_ORIG/sum(W_N_ORIG))
      
      # Counts and proportions in new_data
      new_counts <- new_data |>
        mutate(DATA_TMP_VAR = as.character(!!sym(new_var_name))) |>
        summarize(N_NEW = n(),
                  W_N_NEW = sum(DATA_TMP_WGT_VAR),
                  .by = DATA_TMP_VAR) %>%
        mutate(P_NEW = N_NEW/sum(N_NEW),
               W_P_NEW = W_N_NEW/sum(W_N_NEW))
      
      # Combine
      if (!same_src(new_counts, orig_counts)) {
        new_counts  <- collect(new_counts)
        orig_counts <- collect(orig_counts)
      }
      
      combined_counts <- full_join(
        x = new_counts,
        y = orig_counts,
        by = "DATA_TMP_VAR"
      ) |> 
        collect() |>
        mutate(across(c(N_ORIG, W_N_ORIG, P_ORIG, W_P_ORIG, N_NEW, W_N_NEW, P_NEW, W_P_NEW),
                      \(x) ifelse(is.na(x), 0, x)))
      
      # Hellinger Distance
      unwtd_hd <- HD(combined_counts, "N_ORIG", "N_NEW")
      wtd_hd   <- HD(combined_counts, "W_N_ORIG", "W_N_NEW") |>
        rename(W_HD = HD)
      
      # Chi-square test
      #chisq = combined_counts %>% 
      #  select(N_NEW, N_ORIG) %>%
      #  chisq.test()
      #chisq_df = data.frame(CHISQ_P = chisq$p.value)
     
      # output: stacked distributions with Chi-square and Hellinger distance
      result = bind_cols(combined_counts, unwtd_hd, wtd_hd) %>%
        mutate(VAR = var_name, .before=1)
      combined_results = bind_rows(combined_results, result)
      
    }
  }
  
  return(combined_results)
}

#' @title Utility measure: Kolmogorov-Smirnov distance for continuous variables
#' @description Computes measures of distance
#' between the distributions of continuous variables
#' in the original versus new data.
#' @param orig_data A data frame or data frame extension (e.g., a \code{tbl_spark} object)
#' containing the original data.
#' @param new_data A data frame or data frame extension (e.g., a \code{tbl_spark} object)
#' containing the data from imputation or synthesis.
#' @param vars A character vector of categorical variables' names
#' to compared.
#' @returns A data frame with one row per variable analyzed.
#' The column "KS_DIST" gives the Kolmogorv-Smirnov distance.
#' The column "P_VALUE" gives the two-sided p-value for the 
#' asymptotic two-sample Kolmogorov-Smirnov test. The columns
#' "N_ORIG" and "N_NEW" give the total number of observations
#' with non-missing values for the variable in the
#' in the original and new datasets, respectively.
#' @details
#' Computes the Kolmogorv-Smirnov distance between the 
#' distributions of the variable in the two datasets.
#' This is simply the largest difference between the empirical 
#' cumulative distribution functions of the two datasets.
#' 
#' Also computes the two-sided p-value for the asymptotic 
#' two-sample Kolmogorv-Smirnov test. See [stats::ks.test()]
#' for more details.
#' @export
#' @examples
#' 
#' # Example of distances between categorical variables
#' 
#' compute_ks_distance(
#'   orig_data    = libraries_orig,
#'   new_data     = libraries_synth,
#'   vars         = c("LIBRARIA", "TOTSTAFF")
#' )

compute_ks_distance <- function(orig_data, new_data, vars) {
  
  vars_not_in_orig_data <- setdiff(vars, colnames(orig_data))
  if (length(vars_not_in_orig_data) > 0) {
    sprintf(
      "The following variables are not in `orig_data`: %s", 
      paste(vars_not_in_orig_data, collapse = ", ")
    ) |> stop()
  }
  vars_not_in_new_data <- setdiff(vars, colnames(new_data))
  if (length(vars_not_in_new_data) > 0) {
    sprintf(
      "The following variables are not in `new_data`: %s", 
      paste(vars_not_in_new_data, collapse = ", ")
    ) |> stop()
  }
  
  output <- lapply(vars, \(var_name) {
    # Compute test statistic and sample size
    ks_stats <- dplyr::full_join(
      x = orig_data |> dplyr::count(!!sym(var_name), name = "n_orig"),
      y = new_data  |> dplyr::count(!!sym(var_name), name = "n_new"),
      by = var_name
    ) |> dplyr::mutate(
      n_orig = ifelse(is.na(n_orig), 0, n_orig),
      n_new  = ifelse(is.na(n_new),  0, n_new)
    ) |> 
      dplyr::arrange(!!sym(var_name)) |>
      dplyr::mutate(
        ecdf_orig = cumsum(n_orig) / sum(n_orig),
        ecdf_new  = cumsum(n_new)  / sum(n_new),
        abs_ecdf_diff  = abs(ecdf_new - ecdf_orig)
      ) |> dplyr::summarize(
        KS_DIST = max(abs_ecdf_diff),
        N_ORIG  = sum(n_orig),
        N_NEW   = sum(n_new)
      ) |> dplyr::collect()
    
    # Compute p-value
    ks_stats[['P_VALUE']] <- stats::psmirnov(
      q = ks_stats[['KS_DIST']], 
      sizes = unlist(ks_stats[,c('N_ORIG', 'N_NEW')]), 
      z = if (TIES) 
        w, 
      alternative = "two.sided", exact = FALSE,
      simulate = FALSE, lower.tail = FALSE
    )
    
    ks_stats[['VARIABLE']] <- var_name
    return(ks_stats)
  }) |> bind_rows()
  
  output <- output |> dplyr::relocate(VARIABLE, .before = 1)
  
  return(output)
}

#' @title Utility: distances between continuous variables' distributions
#' @description Summaries of the distances between original variables in the data
#' and their imputed or synthetic versions
#' @param orig_data A data frame or data frame extension (e.g., a \code{tbl_spark} object).
#' @param new_data A data frame or data frame extension (e.g., a \code{tbl_spark} object).
#' @param vars Character strings giving the name of variables in the data frames.
#' @param wgt A character string giving the name of a weight variable in the data
#'
#' @returns A data frame with one column per summary statistic,
#' and one row per combination of variable and implicate.
#' @export
#' @examples
#' 
#' 
#' dist_btwn_cont_vars(
#'   orig_data = libraries_orig,
#'   new_data  = libraries_synth,
#'   vars      = c("TOTCIR", "ELMATCIR")
#' )
#' 
dist_btwn_cont_vars <- function(orig_data, new_data, vars, wgt = NULL) {
  
  if (!is.null(wgt)) {
    new_data <- new_data   |> mutate(DATA_TMP_WGT_VAR = !!sym(wgt))
    orig_data <- orig_data |> mutate(DATA_TMP_WGT_VAR = !!sym(wgt))
  } else {
    new_data <- new_data   |> mutate(DATA_TMP_WGT_VAR = 1)
    orig_data <- orig_data |> mutate(DATA_TMP_WGT_VAR = 1)
  }
  
  combined_results <- NULL
  
  for (var_name in vars) {
    
    # calculate summary statistics for var_name in each dataset
    orig_result <- cont_summary_stats(
      data     = orig_data,
      var      = var_name
    ) |>
      collect()
    
    new_result <- cont_summary_stats(
      data     = new_data,
      var      = var_name
    ) |>
      collect()
    
    # combine together
    result = bind_rows(
      orig_result %>% 
        mutate(VAR = var_name, .before=1) %>%
        mutate(DATASET = 'ORIG', .before = 1),
      new_result %>% 
        mutate(VAR = var_name, .before=1) %>%
        mutate(DATASET = 'NEW', .before = 1)
    )
    
    # calculate differences
    diffs = result %>% filter(DATASET=='NEW') %>% select(-c(DATASET, VAR)) - 
      result %>% filter(DATASET=='ORIG') %>% select(-c(DATASET, VAR))
    result = bind_rows(
      result,
      bind_cols(
        DATASET = 'Difference',
        VAR = var_name,
        diffs
      )
    )
    
    # UPDATE THIS? Or just delete? 
    if (!is.null(wgt)) {
      wtd_stats <- wgt_summ_stats_var(
        data     = data,
        var      = var_name,
        wgt      = wgt,
        new_suff = new_suffix,
        og_suff  = og_suffix
      )
      
      result <- cbind(result, wtd_stats)
    }
    
    combined_results = bind_rows(combined_results, result)  
  }
  
  # KS stats
  ks_stats = compute_ks_distance(orig_data, new_data, vars)
  
  combined_results = combined_results %>%
    left_join(
      ks_stats %>% select(VARIABLE, P_VALUE) %>% rename(KS_PVAL = P_VALUE),
      by = c('VAR' = 'VARIABLE')
    )
  
  return(combined_results)
}


#' @title Aggregate continuous and categorical distances
#' @description Summaries of the distances between all original variables in the data
#' and their imputed or synthetic versions in a single row for each variable
#' @param cat_diffs A data frame of categorical distances.
#' @param cont_diffs A data frame of continuous distances.
#'
#' @returns A data frame with one row per variable.
all_diffs = function(cat_diffs, cont_diffs){
  # stats to keep:
  # - data set (DATASET)
  # - variable names (VAR)
  # - p-value: KS for continuous, Chi-square for categorical
  # - mean for continuous
  # - standard deviation for continuous
  # - maximum difference between levels for categorical
  
  keep_cont_diffs = cont_diffs %>% 
    filter(DATASET == 'Difference') %>%
    select(TABLE, VAR, KS_PVAL, Mean, Sd) %>%
    mutate(sig = case_when(
      KS_PVAL < 0.001 ~ '***',
      KS_PVAL < 0.01 ~ '**',
      KS_PVAL < 0.05 ~ '*',
      TRUE ~ ''
    )) %>%
    rename(pval = KS_PVAL)
  
  keep_cat_diffs = cat_diffs %>%
    group_by(VAR) %>%
    summarise(TABLE = first(TABLE),
              VAR = first(VAR),
              pval = first(chisq_pvals),
              max_diff = max(abs(P_NEW - P_ORIG))) %>%
    mutate(sig = case_when(
      pval < 0.001 ~ '***',
      pval < 0.01 ~ '**',
      pval < 0.05 ~ '*',
      TRUE ~ ''
    ))
  
  out = bind_rows(keep_cont_diffs, keep_cat_diffs) %>%
    select(TABLE, VAR, pval, sig, Mean, Sd, max_diff)
  
  return(out)
}

################################################################################################################
################# MISSING RATES ################################################################################
################################################################################################################
################################################################################################################

#' @title Missing Rates
#' @description Compare missing rates across all variables in two different data frames
#' @param orig_data A data frame
#' @param new_data A data frame
#' @returns A data frame with columns of missing rates among data sets for each variable
compare_missing_rates = function(orig_data, new_data){
  
  # missingess rates across all variables
  orig_missings = orig_data %>%
    #duckplyr::as_tbl() %>%
    summarise(across(everything(), ~ mean(as.numeric(is.na(.))))) %>%
    collect()
  
  orig_uniques = orig_data %>%
    summarise(across(everything(), ~ n_distinct(.))) %>%
    collect()
  
  new_missings = new_data %>%
    #duckplyr::as_tbl() %>%
    summarise(across(everything(), ~ mean(as.numeric(is.na(.))))) %>%
    collect()
  
  new_uniques = new_data %>%
    summarise(across(everything(), ~ n_distinct(.))) %>%
    collect()
  
  # re-format
  orig_missings = orig_missings %>%
    t() %>%
    as.data.frame() %>%
    mutate(VARIABLE = row.names(.)) %>%
    rename(ORIG_MISSING_PROP = V1)
  
  orig_uniques = orig_uniques %>%
    t() %>%
    as.data.frame() %>%
    mutate(VARIABLE = row.names(.)) %>%
    rename(ORIG_UNIQUE_VALS = V1)
  
  new_missings = new_missings %>%
    t() %>%
    as.data.frame() %>%
    mutate(VARIABLE = row.names(.)) %>%
    rename(NEW_MISSING_PROP = V1)
  
  new_uniques = new_uniques %>%
    t() %>%
    as.data.frame() %>%
    mutate(VARIABLE = row.names(.)) %>%
    rename(NEW_UNIQUE_VALS = V1)
  
  # merge together
  out = Reduce(function(x, y) merge(x, y, by='VARIABLE', all=TRUE), list(orig_missings, new_missings, orig_uniques, new_uniques))
  
  return(out)
}

################################################################################################################
################## VALIDITY ####################################################################################
################################################################################################################
################################################################################################################

#' @title Utility: Validity
#' @description Tests that synthetic data values are valid given
#' original data.
#' @param orig_data A data frame (or tbl) corresponding 
#' to the original data.
#' @param new_data A data frame (or tbl) containing the 
#' imputed or synthetic data.
#' @param vars A character vector of variables to be analyzed.
#' @param var_types A character vector of 'cat' or 'cont'
#' @returns
#' A data set indicating whether each variable in the synthetic data
#' is valid given the original data, where a valid variable either
#' has all values within the original range for continuous variables
#' or no new values for catgorical variables.
utility_validity = function(orig_data, new_data, vars, var_types){
  
  valid_out = data.frame()
  
  for(i in 1:length(vars)){
    
    vari = vars[i]
    
    if(var_types[i]=='cat'){
      new_vals_unique = new_data %>% pull(!!sym(vari)) %>% unique()
      orig_vals_unique = orig_data %>% pull(!!sym(vari)) %>% unique()
      new_vals_not_in_orig = sum(!new_vals_unique %in% orig_vals_unique)
      valid = if_else(new_vals_not_in_orig==0, 1, 0)
    }
    
    if(var_types[i]=='cont'){
      min_orig = orig_data %>% pull(!!sym(vari)) %>% min(na.rm=TRUE)
      max_orig = orig_data %>% pull(!!sym(vari)) %>% max(na.rm=TRUE)
      min_new = new_data %>% pull(!!sym(vari)) %>% min(na.rm=TRUE)
      max_new = new_data %>% pull(!!sym(vari)) %>% max(na.rm=TRUE)
      valid = if_else(max_new <= max_orig & max_new >= min_orig &
                        min_new <= max_orig & min_new >= min_orig,
                      1, 0)
    }
    
    valid_out = bind_rows(
      valid_out,
      data.frame('VAR' = vari, 'VALID' = valid)
    )
    
  }
  
  return(valid_out)
  
}


################################################################################################################
############### PAIRWISE ASSOCIATIONS ##########################################################################
################################################################################################################
################################################################################################################

#' @title Utility: compare pairwise correlations
#' @description Computes correlations
#' for all pairs of variables, separately
#' in the original data and in the new data.
#' Computes differences in the correlation values 
#' between the original and new data.
#' @param orig_data A data frame (or tbl) corresponding 
#' to the original data.
#' @param new_data A data frame (or tbl) containing the 
#' imputed or synthetic data.
#' @param vars A character vector of variables to be analyzed.
#' @param method Type of correlation: either \code{"pearson"} (the default)
#' or \code{"spearman"}.
#' @returns
#' A data frame with one row per pair of variables specified by \code{vars}.
#' The columns \code{ORIG_CORR} and \code{NEW_CORR} give the correlations
#' in the original and new datasets, respectively. The column \code{DIFF}
#' gives the difference (\code{NEW_CORR} minus \code{ORIG_CORR}).
#' The column \code{REL_DIFF} gives the relative difference
#' (\code{DIFF} divided by \code{ORIG_CORR}).
#' @export
#' @examples
#' compare_pairwise_correlations(
#'   orig_data = libraries_orig,
#'   new_data  = libraries_synth,
#'   vars      = c("TOTCIR", "ELMATCIR", "LIBRARIA")
#' )

compare_pairwise_correlations <- function(
    orig_data,
    new_data,
    vars,
    method = "pearson"
) {
  
  # check for all missing or all one value
  num_vals_orig = orig_data %>% 
    summarise(across(everything(), ~ n_distinct(.))) %>%
    collect()
  orig_single_val_vars = names(num_vals_orig)[num_vals_orig==1]
  num_vals_new = new_data %>%
    summarise(across(everything(), ~ n_distinct(.))) %>%
    collect()
  new_single_val_vars = names(num_vals_new)[num_vals_new==1]
  
  # update list of variables to remove these
  vars = vars[!vars %in% c(orig_single_val_vars, new_single_val_vars)]
  
  var_pairs <- combn(x = vars, m = 2) |> t() |>
    as.data.frame() |>
    `colnames<-`(c("VAR_1", "VAR_2"))
  n_pairs <- nrow(var_pairs)
  
  corr_df <- var_pairs
  corr_df[['ORIG_CORR']] <- NA_real_
  corr_df[['NEW_CORR']] <- NA_real_
  
  # Compute correlation matrices for original and new data
  if (inherits(orig_data, "tbl_spark")) {
    orig_corr_mat <- sparklyr::ml_corr(
      x       = orig_data, 
      columns = vars,
      method  = method
    ) |> as.matrix()
    rownames(orig_corr_mat) <- colnames(orig_corr_mat)
  } else if (!inherits(orig_data, "tbl_df")) {
    orig_corr_mat <- stats::cor(
      x      = orig_data |> select(all_of(vars)),
      method = method,
      use    = "pairwise.complete.obs"
    )
  } else if (inherits(orig_data, "tbl_df")) {
    for (i in seq_len(n_pairs)) {
      var_1 <- var_pairs[['VAR_1']][i]
      var_2 <- var_pairs[['VAR_2']][i]
      if (inherits(orig_data, "duckplyr_df")) {
        orig_data <- orig_data |> duckplyr::as_tbl()
      }
      corr_df[['ORIG_CORR']][i] <- orig_data |>
        dplyr::filter(!is.na(!!rlang::sym(var_1)), !is.na(!!rlang::sym(var_2))) |>
        dplyr::summarize("ORIG_CORR" := cor(!!rlang::sym(var_1), !!rlang::sym(var_2))) |>
        dplyr::collect() |>
        dplyr::pull("ORIG_CORR")
    }
  }
  
  if (inherits(new_data, "tbl_spark")) {
    new_corr_mat <- sparklyr::ml_corr(
      x       = orig_data, 
      columns = vars,
      method  = method
    ) |> as.matrix()
    rownames(orig_corr_mat) <- colnames(orig_corr_mat)
  } else if (!inherits(new_data, "tbl_df")) {
    new_corr_mat <- stats::cor(
      x      = new_data |> select(all_of(vars)),
      method = method,
      use    = "pairwise.complete.obs"
    )
  } else if (inherits(new_data, "tbl_df")) {
    for (i in seq_len(n_pairs)) {
      var_1 <- var_pairs[['VAR_1']][i]
      var_2 <- var_pairs[['VAR_2']][i]
      if (inherits(new_data, "duckplyr_df")) {
        new_data <- new_data |> duckplyr::as_tbl()
      }
      corr_df[['NEW_CORR']][i] <- new_data |>
        dplyr::filter(!is.na(!!rlang::sym(var_1)), !is.na(!!rlang::sym(var_2))) |>
        dplyr::summarize("NEW_CORR" := cor(!!rlang::sym(var_1), !!rlang::sym(var_2))) |>
        dplyr::collect() |>
        dplyr::pull("NEW_CORR")
    }
  }
  
  
  # If the correlation output was a matrix, add its information to the combined summary data frame
  if (inherits(orig_data, "tbl_spark") || !inherits(orig_data, c("tbl_df", "tbl_sql"))) {
    for (var_pair_index in seq_len(n_pairs)) {
      v1 <- var_pairs[['VAR_1']][var_pair_index]
      v2 <- var_pairs[['VAR_2']][var_pair_index]
      corr_df[['ORIG_CORR']][var_pair_index] <- orig_corr_mat[v1, v2]
    }
  }
  if (inherits(new_data, "tbl_spark") || !inherits(new_data, c("tbl_df", "tbl_sql"))) {
    for (var_pair_index in seq_len(n_pairs)) {
      v1 <- var_pairs[['VAR_1']][var_pair_index]
      v2 <- var_pairs[['VAR_2']][var_pair_index]
      corr_df[['NEW_CORR']][var_pair_index] <- new_corr_mat[v1, v2]
    }
  }
  
  # Compute differences between correlations
  corr_df[['DIFF']] <- corr_df[['NEW_CORR']] - corr_df[['ORIG_CORR']]
  corr_df[['REL_DIFF']] <- corr_df[['DIFF']] / corr_df[['ORIG_CORR']]
  
  return(corr_df)
}


#' @title Utility: compare Cramer's V
#' @description Computes Cramer's
#' for all pairs of variables, separately
#' in the original data and in the new data.
#' Computes differences in the Cramer's V values 
#' between the original and new data.
#' @param orig_data A data frame (or tbl) corresponding 
#' to the original data.
#' @param new_data A data frame (or tbl) containing the 
#' imputed or synthetic data.
#' @param vars A character vector of variables to be analyzed.
#' @returns
#' A list of Cramer's V matrices with rows and columns specified by \code{vars}.
#' The matrices \code{ORIG_VS} and \code{NEW_VS} give the Cramer's Vs
#' in the original and new datasets, respectively. The column \code{DIFFS}
#' gives the difference (\code{NEW_VS} minus \code{ORIG_VS}).
#' The column \code{REL_DIFF} gives the relative difference
#' (\code{DIFFS} divided by \code{ORIG_VS}).
cramers_v = function(orig_data, new_data, vars){
  
  # check for all missing or all one value
  num_vals_orig = orig_data %>% 
    summarise(across(everything(), ~ n_distinct(.))) %>%
    collect()
  orig_single_val_vars = names(num_vals_orig)[num_vals_orig==1]
  num_vals_new = new_data %>%
    summarise(across(everything(), ~ n_distinct(.))) %>%
    collect()
  new_single_val_vars = names(num_vals_new)[num_vals_new==1]
  
  # update list of variables to remove these
  vars = vars[!vars %in% c(orig_single_val_vars, new_single_val_vars)]
  
  # get all 2-way combinations of variables
  var_pairs <- combn(x = vars, m = 2) |> t() |>
    as.data.frame() |>
    `colnames<-`(c("VAR_1", "VAR_2"))
  n_pairs <- nrow(var_pairs)
  
  # output:
  ## matrix of cramer's Vs for each combination
  out = var_pairs %>%
    mutate(
      V_ORIG = NA,
      V_NEW = NA
    )
  
  for(i in 1:nrow(var_pairs)){
    # original data
    table_orig = orig_data %>%
      filter(!is.na(!!sym(var_pairs$VAR_1[i])) & !is.na(!!sym(var_pairs$VAR_2[i]))) %>%
      group_by(!!sym(var_pairs$VAR_1[i]), !!sym(var_pairs$VAR_2[i])) %>%
      summarize(n=n()) %>%
      collect()
    table_orig_wide = reshape(
      data.frame(table_orig), 
      idvar = var_pairs$VAR_1[i], 
      timevar = var_pairs$VAR_2[i], 
      direction='wide'
    ) %>%
      replace(is.na(.), 0)
    
    chi2_orig = chisq.test(table_orig_wide[,-1])
    n_orig = sum(table_orig$n)
    denom_orig = min(nrow(table_orig_wide)-1, ncol(table_orig_wide)-2)
    V_orig = sqrt(chi2_orig$statistic/n_orig/denom_orig)

    # new data
    table_new = new_data %>%
      filter(!is.na(!!sym(var_pairs$VAR_1[i])) & !is.na(!!sym(var_pairs$VAR_2[i]))) %>%
      group_by(!!sym(var_pairs$VAR_1[i]), !!sym(var_pairs$VAR_2[i])) %>%
      summarize(n=n()) %>%
      collect()
    table_new_wide = reshape(
      data.frame(table_new), 
      idvar = var_pairs$VAR_1[i], 
      timevar = var_pairs$VAR_2[i], 
      direction='wide'
    ) %>%
      replace(is.na(.), 0)
    
    chi2_new = chisq.test(table_new_wide[,-1])
    n_new = sum(table_new$n)
    denom_new = min(nrow(table_new_wide)-1, ncol(table_new_wide)-2)
    V_new = sqrt(chi2_new$statistic/n_new/denom_new)
    
    # save Vs
    out$V_ORIG[i] = V_orig
    out$V_NEW[i] = V_new
    
  }
  
  # other outputs:
  ## raw difference
  #diffs = out_vs_new - out_vs_orig
  ## relative difference
  #rel_diffs = diffs / out_vs_orig
  out = out %>%
    mutate(
      DIFF = V_NEW - V_ORIG,
      REL_DIFF = DIFF/V_ORIG
    )
  
  return(out)
  
}


################################################################################################################
################ COMPARE CROSSTABS #############################################################################
################################################################################################################
################################################################################################################

#' @title Utility: Compare crosstabs across datasets 
#' @description Computes k-way cross-tabulations
#' in the original versus new data.
#' @param orig_data A data frame or data frame extension (e.g., a \code{tbl_spark} object)
#' containing the original data.
#' @param new_data A data frame or data frame extension (e.g., a \code{tbl_spark} object)
#' containing the data from imputation or synthesis.
#' @param vars A character vector of categorical variables' names
#' to compared.
#' @param k An integer between 2 and the number of variables in `vars`,
#' controlling the dimension of the cross-tabulations.
#' For example, specifying \code{k=3} will produce all three-way cross-tabulations from the variables
#' specified by \code{vars}.
#' @return A list of data frames representing cross-tabulations.
#' In each data frame, the variable `N_ORIG` and `N_NEW` give the counts
#' from the original and new datasets, respectively.
#' @export
#' @examples
#' # Create an example variable to use
#' 
#' library(dplyr)
#' 
#' libraries_orig <- libraries_orig |> mutate(
#'   STAFF_SIZE_CATEGORY = TOTSTAFF |> cut(
#'     breaks = c(0, 1.262, 3.3, 7.34, 17.868, Inf),
#'     labels = c("Small", "Small-to-Medium", "Medium",
#'                "Medium-to-Large", "Large"),
#'   ),
#'   HAS_ELECTRONIC_MATERIALS = ifelse(ELMATCIR > 0, "Yes", "No")
#' )
#' 
#' libraries_synth <- libraries_synth |> mutate(
#'   STAFF_SIZE_CATEGORY = TOTSTAFF |> cut(
#'     breaks = c(0, 1.262, 3.3, 7.34, 17.868, Inf),
#'     labels = c("Small", "Small-to-Medium", "Medium",
#'                "Medium-to-Large", "Large"),
#'   ),
#'   HAS_ELECTRONIC_MATERIALS = ifelse(ELMATCIR > 0, "Yes", "No")
#' )
#' 
#' # Compare crosstabs in the two datasets
#' compare_crosstabs(
#'   orig_data = libraries_orig,
#'   new_data  = libraries_synth,
#'   vars      = c("STABR", "STAFF_SIZE_CATEGORY",
#'                 "HAS_ELECTRONIC_MATERIALS"),
#'   k         = 2 
#' )
#' 
compare_crosstabs <- function(orig_data, new_data, vars, k = 2) {
  if (k < 2) {
    stop("`k` must be an integer greater than or equal to 2.")
  }
  if (k > length(vars)) {
    stop("`k` cannot be larger than the number of variables in `vars`.")
  }
  var_combos   <- utils::combn(x = vars, m = k)
  n_var_combos <- ncol(var_combos)
  
  lapply(
    X = seq_len(n_var_combos), 
    FUN = function(var_combo_index) {
      current_var_combo <- var_combos[,var_combo_index]
      dplyr::full_join(
        x = orig_data |> dplyr::count(!!!syms(current_var_combo), name = "N_ORIG"),
        y = new_data  |> dplyr::count(!!!syms(current_var_combo),  name = "N_NEW"),
        by = current_var_combo
      ) |>
        dplyr::mutate(
          N_ORIG = ifelse(is.na(N_ORIG), 0, N_ORIG),
          P_ORIG = N_ORIG/sum(N_ORIG),
          N_NEW  = ifelse(is.na(N_NEW),  0, N_NEW),
          P_NEW = N_NEW/sum(N_NEW)
        )
    }
  )
  
}
