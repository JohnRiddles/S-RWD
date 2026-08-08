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
      
      orig_counts <- orig_data |>
        mutate(DATA_TMP_VAR = !!sym(var_name)) |>
        summarize(DIST_ORIG = n(),
                  W_DIST_ORIG = sum(DATA_TMP_WGT_VAR),
                  .by = DATA_TMP_VAR)
      
      new_counts <- new_data |>
        mutate(DATA_TMP_VAR = !!sym(new_var_name)) |>
        summarize(DIST_NEW = n(),
                  W_DIST_NEW = sum(DATA_TMP_WGT_VAR),
                  .by = DATA_TMP_VAR)
      
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
        mutate(across(c(DIST_ORIG, W_DIST_ORIG),
                      \(x) ifelse(is.na(x), 0, x)))
      
      unwtd_hd <- HD(combined_counts, "DIST_ORIG", "DIST_NEW")
      wtd_hd   <- HD(combined_counts, "W_DIST_ORIG", "W_DIST_NEW") |>
        rename(W_HD = HD)
      
      result <- bind_cols(unwtd_hd, wtd_hd) |> 
        mutate(VARIABLE  = var_name,
               IMPLICATE = str_extract(new_suffix, "\\d+"))
      
      combined_results <- bind_rows(result, combined_results)
    }
  }
  
  combined_results <- combined_results |>
    relocate(VARIABLE, IMPLICATE, .before = 1) |>
    arrange(VARIABLE, IMPLICATE)

  if (length(new_suffixes) == 1 && (new_suffixes == "")) {
    combined_results[['IMPLICATE']] <- NULL
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
#' @param data A data frame or data frame extension (e.g., a \code{tbl_spark} object).
#' @param vars Character strings giving the name of variables in the data
#' @param wgt A character string giving the name of a weight variable in the data
#' @param new_suffixes A character vector giving the suffixes
#' used to distinguish imputed/synthetic variables from the original variables.
#' For example, with three implicates these might be \code{c("_1", "_2", "_3")}.
#' @param og_suffix A character string giving the suffix for every original variable
#' (the suffix must be shared by all of the variables in \code{vars}).
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
#'   vars      = c("TOTCIR", "ELMATCIR"),
#'   statistic = "mean"
#' )
#' 
dist_btwn_cont_vars <- function(data, vars, wgt = NULL, new_suffixes, og_suffix) {
  
  collected_results <- NULL
  
  for (new_suffix in new_suffixes) {
    for (var_name in vars) {
      
      result <- summ_stats_var(
        data     = data,
        var      = var_name,
        new_suff = new_suffix,
        og_suff  = og_suffix
      )
      
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
      
      result[['IMPLICATE']] <- new_suffix |> str_extract("\\d+")
      result[['VARIABLE']]  <- var_name
      collected_results <- bind_rows(collected_results, result) |>
        relocate(VARIABLE, IMPLICATE, .before = 1) |>
        arrange(VARIABLE, IMPLICATE)
    }
  }
  return(collected_results)
}

