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