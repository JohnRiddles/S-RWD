library(dplyr)

libraries_orig <- libraries_orig |> mutate(
  STAFF_SIZE_CATEGORY = TOTSTAFF |> cut(
    breaks = c(0, 1.262, 3.3, 7.34, 17.868, Inf),
    labels = c("Small", "Small-to-Medium", "Medium",
               "Medium-to-Large", "Large"),
  ) |> as.numeric(),
  HAS_ELECTRONIC_MATERIALS = ifelse(ELMATCIR > 0, "Yes", "No")
)

libraries_synth <- libraries_synth |> mutate(
  STAFF_SIZE_CATEGORY = TOTSTAFF |> cut(
    breaks = c(0, 1.262, 3.3, 7.34, 17.868, Inf),
    labels = c("Small", "Small-to-Medium", "Medium",
               "Medium-to-Large", "Large"),
  ) |> as.numeric(),
  HAS_ELECTRONIC_MATERIALS = ifelse(ELMATCIR > 0, "Yes", "No")
)

# Compare crosstabs in the two datasets
test_that("Expected list of crosstabs", {
  crosstab_output <- compare_crosstabs(
    orig_data = libraries_orig,
    new_data  = libraries_synth,
    vars      = c("STABR", "STAFF_SIZE_CATEGORY",
                  "HAS_ELECTRONIC_MATERIALS"),
    k         = 2 
  )
  expect_equal(length(crosstab_output), 3)
  crosstab_combos <- crosstab_output |> sapply(\(df) {
    setdiff(colnames(df), c("N_ORIG", "N_NEW"))
  }) 
  expected_combos <- combn(
    x = c("STABR", "STAFF_SIZE_CATEGORY",
          "HAS_ELECTRONIC_MATERIALS"),
    m = 2
  )
  expect_equal(object = crosstab_combos, expected = expected_combos)
})
