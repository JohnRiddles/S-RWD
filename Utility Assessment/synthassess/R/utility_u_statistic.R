#' @title Compute U-statistic
#' @description Computes the U-statistic 
#' based on a logistic regression model with specific predictor variables.
#' See Snoke et al. (2018) for details.
#' @param orig_data A data frame (or tbl) corresponding 
#' to the original data.
#' @param new_data A data frame (or tbl) containing the 
#' imputed or synthetic data.
#' @param vars A character vector of variables
#' to use as predictors in the logistic regression model
#' @return A data frame with the U-statistic, its standard error, and p-value
#' @references
#' Joshua Snoke, Gillian M. Raab, Beata Nowok, Chris Dibben, Aleksandra Slavkovic.
#' "General and Specific Utility Measures for Synthetic Data." 
#' Journal of the Royal Statistical Society Series A: Statistics in Society, 
#' Volume 181, Issue 3, June 2018, Pages 663–688. 
#' https://doi.org/10.1111/rssa.12358
#' @export
#' @examples
#' compute_u_statistic(
#'   orig_data = libraries_orig,
#'   new_data  = libraries_synth,
#'   vars      = c("TOTCIR", "ELMATCIR")
#' )

compute_u_statistic <- function(
  orig_data,
  new_data,
  vars = NULL
) {

  if (is.null(vars)) {
    stop("`vars` must contain at least one variable name.")
  }
  
  stacked_data <- union_all(
    orig_data |> 
      select(all_of(vars)) |>
      mutate(TMP_WHICH_DATA = 0),
    new_data |> 
      select(all_of(vars)) |>
      mutate(TMP_WHICH_DATA = 1),
  )

  if (inherits(stacked_data, "tbl_spark")) {
    log_reg_model <- sparklyr::ml_logistic_regression(
      x = stacked_data,
      formula = reformulate(response = 'TMP_WHICH_DATA', termlabels = vars)
    )
    pred_model_probs <- log_reg_model |>
      sparklyr::ml_predict() |>
      pull("probability_1")
  } else {
    log_reg_model <- glm(
      formula = reformulate(response = 'TMP_WHICH_DATA', termlabels = vars),
      data    = stacked_data,
      family  = stats::binomial(link = 'logit')
    )
    pred_model_probs <- log_reg_model |> predict(type = "response")
  }

  N = stacked_data |> 
    summarize(N = n()) |> 
    pull("N")
  n2 = stacked_data |> 
    summarize(n2 = sum(TMP_WHICH_DATA, na.rm = TRUE)) |>
    pull("n2")
  n1 = N - n2
  c_value <- n2/N

  U_statistic = (1/N) * sum((pred_model_probs - 0.5)^2)

  k = length(coef(log_reg_model))

  EV = ((k - 1) * (1 - c_value)^2 * (c_value))/N
  SE = sqrt(EV)
  p_value = 1 - pchisq(U_statistic, df = EV)

  result <- data.frame(
    U = U_statistic,
    SE = SE,
    p_value = p_value
  )
  return(result)
}