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
        N_NEW  = ifelse(is.na(N_NEW),  0, N_NEW)
      )
    }
  )
  
}