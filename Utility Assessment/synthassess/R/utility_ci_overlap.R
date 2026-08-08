#' @title Internal helper function to compute interval overlap statistic
#' @description Computes interval overlap statistic
#' @param l1 The lower limit of the first confidence interval
#' @param u1 The upper limit of the first confidence interval
#' @param l2 The lower limit of the second confidence interval
#' @param u2 The upper limit of the second confidence interval
#' @keywords internal
compute_io <- function(l1, u1, l2, u2) {
  numerator = pmin(u1, u2) - pmax(l1, l2)
  io = 0.5 * (numerator/(u1-l1) + numerator/(u2-l2))
  return(io)
}
#' @title Utility: confidence interval overlap
#' @description 
#' Compare overlap in confidence intervals
#' for a statistic computed using original data vs
#' imputed/synthetic data.
#' @param orig_data A data frame (or tbl) corresponding 
#' to the original data.
#' @param new_data A data frame (or tbl) containing the 
#' imputed or synthetic data.
#' @param vars A character vector of variables to be analyzed.
#' @param new_suffixes An optional character vector of suffixes
#' used to distinguish multiple implicates.
#' @param statistic Either \code{"mean"}, \code{"total"}, or \code{"prop"}.
#' @param level The confidence level for the intervals. Default is 0.95.
#' Must be at least 0.5 and less than 1.
#' @returns 
#' A data frame with one row per combination of variable and implicate.
#' The column \code{OVERLAP} is the confidence interval overlap statistic,
#' while \code{NEW_EST_IN_ORIG_CI} indicates whether the point estimate
#' for the new data is contained in the confidence interval from the original data.
#' The remaining columns are the point estimates and confidence intervals
#' from each source.
#' @details
#' This method was proposed by Karr et al. (2006); see section 2.2.1, particularly equation (5)
#' of that reference.
#' @references
#' Karr, A. F., Kohnen, C. N., Oganian, A., Reiter, J. P., & Sanil, A. P. (2006). 
#' "A Framework for Evaluating the Utility of Data Altered to Protect Confidentiality." 
#' The American Statistician, 60(3), 224–232. 
#' https://doi.org/10.1198/000313006x124640
#' @seealso [svy_check_ci_overlap()] for use with survey data.
#' @export
#' @examples
#' check_ci_overlap(
#'   orig_data = libraries_orig,
#'   new_data = libraries_synth,
#'   vars = c("TOTCIR", "ELMATCIR"),
#'   statistic = "mean"
#' )
check_ci_overlap <- function(
    orig_data,
    new_data,
    vars,
    new_suffixes = NULL,
    statistic = "mean",
    level = 0.95
) {
  
  # Check inputs
  stopifnot(statistic %in% c("mean", "prop", "sum"))
  stopifnot(length(level) == 1)
  stopifnot(is.numeric(level) && !is.na(level))
  stopifnot(level >= 0.5 & level < 1)
  
  if (is.null(new_suffixes)) {
    new_suffixes <- ""
  }

  # Get critical values from normal distribution, for confidence intervals
  normal_quantiles <- c(
    'low' = qnorm(p = (1-level)/2, lower.tail = TRUE),
    'upp' = qnorm(p = 0.5 * (1 + level), lower.tail = TRUE)
  )
  
  combined_result <- NULL
  
  # Loop over list of variables
  for (var_name in vars) {
    
    # Get statistics for the original data
    if (statistic == "prop") {
      orig_stats <- orig_data |>
        mutate(CATEGORY = !!sym(var_name)) |>
        summarize(orig = n(), .by = CATEGORY) |>
        mutate(orig_samp_size = sum(orig), 
               orig = orig/samp_size,
               orig_se = sqrt((orig*(1-orig))/orig_samp_size)) |>
        mutate(CATEGORY = as.character(CATEGORY)) |>
        collect() |>
        mutate(orig_low = orig + orig_se * normal_quantiles['low'],
               orig_upp = orig + orig_se * normal_quantiles['upp'])
    } else {
      orig_stats <- orig_data |>
        ungroup() |>
        mutate(DATA_TMP_VAR = !!sym(var_name)) |>
        summarize(orig = mean(DATA_TMP_VAR, na.rm = TRUE),
                  sd_orig = sd(DATA_TMP_VAR, na.rm = TRUE),
                  orig_samp_size = sum(!is.na(DATA_TMP_VAR))) |>
        collect()

      if (statistic == "mean") {
        orig_stats <- orig_stats |>
          mutate(orig_se = sd_orig/sqrt(orig_samp_size),
                 orig_low = orig + orig_se * normal_quantiles['low'],
                 orig_upp = orig + orig_se * normal_quantiles['upp'])
      }
      
      if (statistic == "sum") {
        orig_stats <- orig_stats |>
          mutate(orig     = orig_samp_size * orig,
                 orig_se  = sd_orig,
                 orig_low = orig + orig_se * normal_quantiles['low'],
                 orig_upp = orig + orig_se * normal_quantiles['upp'])
      }
    }
    
    # Get statistics for the new data, separately for each implicate if applicable
    for (suffix in new_suffixes) {
      
      new_var_name <- paste0(var_name, suffix)
      implicate <- str_extract(suffix, "\\d+")

      if (!new_var_name %in% colnames(new_data)) {
        error_msg <- sprintf(
          fmt = "The variable `%s` is not in `new_data`. Please check the arguments `vars` and `new_suffixes`.",
          new_var_name
        )
        stop(error_msg)
      }
      
      if (statistic == "prop") {
        new_stats <- new_data |>
          mutate(CATEGORY = !!sym(var_name)) |>
          summarize(new = n(), .by = CATEGORY) |>
          mutate(new_samp_size = sum(new), 
                 new = new/new_samp_size,
                 new_se = sqrt((new*(1-new))/new_samp_size)) |>
          mutate(CATEGORY = as.character(CATEGORY)) |>
          collect() |>
          mutate(new_low = new + new_se * normal_quantiles['low'],
                 new_upp = new + new_se * normal_quantiles['upp'])
      } else {
        new_stats <- new_data |>
          ungroup() |>
          mutate(DATA_TMP_VAR = !!sym(var_name)) |>
          summarize(new = mean(DATA_TMP_VAR, na.rm = TRUE),
                    sd_new = sd(DATA_TMP_VAR, na.rm = TRUE),
                    new_samp_size = sum(!is.na(DATA_TMP_VAR))) |>
          collect()

        if (statistic == "mean") {
          new_stats <- new_stats |>
            mutate(new_se = sd_new/sqrt(new_samp_size),
                   new_low = new + new_se * normal_quantiles['low'],
                   new_upp = new + new_se * normal_quantiles['upp'])
        }
        
        if (statistic == "sum") {
          new_stats <- new_stats |>
            mutate(new     = new_samp_size * new,
                   new_se  = sd_new,
                   new_low = new + new_se * normal_quantiles['low'],
                   new_upp = new + new_se * normal_quantiles['upp'])
        }
      }
      
      if (statistic == "prop") {
        result <- full_join(
          x = orig_stats, y = new_stats,
          by = intersect(colnames(orig_stats), colnames(new_stats))
        )
      } else {
        result <- cbind(orig_stats, new_stats)
      }
      
      combined_result <- bind_rows(
        result |> mutate(VARIABLE = var_name,
                         IMPLICATE = implicate),
        combined_result
      )
    }
  }
  
  combined_result <- combined_result |>
    mutate(new_est_in_orig_ci = ifelse(
      (new >= orig_low & new <= orig_upp), TRUE, FALSE
    )) |>
    mutate(overlap = compute_io(l1 = orig_low, u1 = orig_upp,
                                l2 = new_low,  u2 = new_upp))
  
  combined_result <- combined_result |>
    select(any_of(c("VARIABLE", "IMPLICATE", "CATEGORY")),
           overlap, new_est_in_orig_ci,
           orig, new, orig_low, new_low, orig_upp, new_upp) |>
    rename_with(toupper) |>
    arrange(VARIABLE, IMPLICATE) |>
    tibble::tibble()

  if (is.null(new_suffixes)) {
    combined_result[["IMPLICATE"]] <- NULL
  }
  
  return(combined_result)
}

#' @title Check confidence interval overlap for survey data
#' @description 
#' Compare overlap in confidence intervals
#' for a statistic computed using original survey data vs
#' imputed/synthetic survey data.
#' @param orig_svy A survey design object corresponding 
#' to the original data.
#' @param new_svy A survey design object containing the 
#' imputed or synthetic data.
#' @param vars A character vector of variables to be analyzed.
#' @param new_suffixes An optional character vector of suffixes
#' used to distinguish multiple implicates.
#' @param statistic Either \code{"mean"}, \code{"total"}, or \code{"prop"}.
#' @param level The confidence level for the intervals. Default is 0.95.
#' Must be at least 0.5 and less than 1.
#' @returns 
#' A data frame with one row per combination of variable and implicate.
#' The column \code{OVERLAP} is the confidence interval overlap statistic,
#' while \code{NEW_EST_IN_ORIG_CI} indicates whether the point estimate
#' for the new data is contained in the confidence interval from the original data.
#' The remaining columns are the point estimates and confidence intervals
#' from each source.
#' @details
#' This method was proposed by Karr et al. (2006); see section 2.2.1, particularly equation (5)
#' of that reference.
#' @references
#' Karr, A. F., Kohnen, C. N., Oganian, A., Reiter, J. P., & Sanil, A. P. (2006). 
#' "A Framework for Evaluating the Utility of Data Altered to Protect Confidentiality." 
#' The American Statistician, 60(3), 224–232. 
#' https://doi.org/10.1198/000313006x124640
#' @seealso [check_ci_overlap()] for use with non-survey data.
#' @export
#' @examples
#' library(srvyr)
#' 
#' # Prepare survey data for analysis
#' libraries_orig_svy <- as_survey_rep(
#'   .data      = libraries_orig,
#'   weights    = FULL_SAMPLE_WGT,
#'   repweights = num_range("REP_WGT_", 1:80),
#'   type       = "successive-difference", mse = TRUE
#' )
#' libraries_synth_svy <- as_survey_rep(
#'   .data      = libraries_synth,
#'   weights    = FULL_SAMPLE_WGT,
#'   repweights = num_range("REP_WGT_", 1:80),
#'   type       = "successive-difference", mse = TRUE
#' )
#' 
#' # Check CI overlap for estimated means
#' svy_check_ci_overlap(
#'   orig_svy  = libraries_orig_svy,
#'   new_svy   = libraries_synth_svy,
#'   vars      = c("TOTCIR", "ELMATCIR"),
#'   statistic = "mean"
#' )
svy_check_ci_overlap <- function(
    orig_svy,
    new_svy,
    vars,
    new_suffixes = NULL,
    statistic = "mean",
    level = 0.95
) {

  if (!requireNamespace("srvyr", quietly = TRUE)) {
    stop("The `srvyr` package must be installed to use this function.")
  }
  
  stopifnot(statistic %in% c("mean", "prop", "sum"))
  if (!any(inherits(orig_svy, c('svyrep.design', 'survey.design')))) {
    stop("`orig_svy` must be a survey object created with the 'survey' or 'srvyr' package.")
  }
  if (!any(inherits(new_svy, c('svyrep.design', 'survey.design')))) {
    stop("`orig_svy` must be a survey object created with the 'survey' or 'srvyr' package.")
  }
  
  orig_svy <- srvyr::as_survey(orig_svy)
  new_svy  <- srvyr::as_survey(new_svy)
  
  if (is.null(new_suffixes)) {
    new_suffixes <- ""
  }
  
  stat_fn <- switch(
    statistic, 
    "mean" = srvyr::survey_mean,
    "prop" = srvyr::survey_prop,
    "sum"  = srvyr::survey_total
  )
  
  combined_result <- NULL
  
  for (var_name in vars) {
    
    if (statistic == "prop") {
      orig_stats <- orig_svy |>
        mutate(CATEGORY = !!sym(var_name)) |>
        summarize(orig = survey_prop(vartype = "ci", level = level), .by = CATEGORY) |>
        mutate(CATEGORY = as.character(CATEGORY))
    } else {
      orig_stats <- orig_svy |>
        ungroup() |>
        mutate(DATA_TMP_VAR = !!sym(var_name)) |>
        summarize(orig = stat_fn(DATA_TMP_VAR, na.rm = TRUE,
                                 vartype = "ci", level = level))
    }
    
    for (suffix in new_suffixes) {
      
      new_var_name <- paste0(var_name, suffix)
      implicate <- str_extract(suffix, "\\d+")

      if (!new_var_name %in% colnames(new_svy)) {
        error_msg <- sprintf(
          fmt = "The variable `%s` is not in `new_svy`. Please check the arguments `vars` and `new_suffixes`.",
          new_var_name
        )
        stop(error_msg)
      }
      
      if (statistic == "prop") {
        new_stats <- new_svy |>
          mutate(CATEGORY = !!sym(new_var_name)) |>
          summarize(new = survey_prop(vartype = "ci", level = level), .by = CATEGORY) |>
          mutate(CATEGORY = as.character(CATEGORY))
        
      } else {
        new_stats <- new_svy |>
          ungroup() |>
          mutate(DATA_TMP_VAR = !!sym(new_var_name)) |>
          summarize(new = stat_fn(DATA_TMP_VAR, na.rm = TRUE,
                                  vartype = "ci", level = level))
      }
      
      if (statistic == "prop") {
        result <- full_join(
          x = orig_stats, y = new_stats,
          by = intersect(colnames(orig_stats), colnames(new_stats))
        )
      } else {
        result <- cbind(orig_stats, new_stats)
      }
      
      combined_result <- bind_rows(
        result |> mutate(VARIABLE = var_name,
                         IMPLICATE = implicate),
        combined_result
      )
    }
  }
  
  compute_io <- function(l1, u1, l2, u2) {
    numerator = pmin(u1, u2) - pmax(l1, l2)
    io = 0.5 * (numerator/(u1-l1) + numerator/(u2-l2))
    return(io)
  }
  
  combined_result <- combined_result |>
    mutate(new_est_in_orig_ci = ifelse(
      (new >= orig_low & new <= orig_upp), TRUE, FALSE
    )) |>
    mutate(overlap = compute_io(l1 = orig_low, u1 = orig_upp,
                                l2 = new_low,  u2 = new_upp))
  
  combined_result <- combined_result |>
    select(any_of(c("VARIABLE", "IMPLICATE", "CATEGORY")),
           overlap, new_est_in_orig_ci,
           orig, new, orig_low, new_low, orig_upp, new_upp) |>
    rename_with(toupper) |>
    arrange(VARIABLE, IMPLICATE) |>
    tibble::tibble()
  
  return(combined_result)
}
