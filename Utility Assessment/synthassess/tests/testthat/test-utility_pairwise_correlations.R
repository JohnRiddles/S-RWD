library(dplyr)
library(duckplyr)

origdata <- duckplyr::as_duckdb_tibble(libraries_orig)
newdata <- duckplyr::as_duckdb_tibble(libraries_synth)

test_that("`compare_pairwise_correlations()` works with `tbl_df` objects", {
  expect_equal(
    object = compare_pairwise_correlations(
      orig_data = origdata, 
      new_data = newdata, 
      vars = c("TOTCIR", "ELMATCIR", "LIBRARIA")
    ),
    expected = compare_pairwise_correlations(
      orig_data = libraries_orig, 
      new_data = libraries_synth, 
      vars = c("TOTCIR", "ELMATCIR", "LIBRARIA")
    )
  )
})
