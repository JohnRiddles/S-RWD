library(dplyr)

test_that("Correct results for `dist_btwn_cat_vars()` in small example", {

  x1 <- data.frame(x = factor(c("A", "A", "A", "B", "B", "B")),
                   y = factor(c("a", "a", "a", "b", "b", "b")))                 
  x2 <- data.frame(x = factor(c("A", "A", "A", "B", "B")),
                   y = factor(c("c", "a", "b", "a", "b")))

  output <- dist_btwn_cat_vars(
    orig_data = x1,
    new_data  = x2,
    vars      = c("x", "y") 
  ) |> 
    arrange(VARIABLE)

  expect_equal(
    object   = round(output[['HD']], 5),
    expected = c(0.07116, 0.32492)
  )

})

test_that("Correct results for `compute_ks_distance()`", {
  suppressWarnings({
    results <- compute_ks_distance(libraries_orig, libraries_synth, c("LIBRARIA"))
    expected_results <- ks.test(
      libraries_orig[['LIBRARIA']], libraries_synth[['LIBRARIA']],
      exact = FALSE, simulate.p.value = FALSE, alternative = "two.sided"
    )
  })
  expect_equal(
    object   = unname(results[['KS_DIST']]),
    expected = unname(expected_results[['statistic']])
  )
  expect_equal(
    object   = unname(results[['P_VALUE']]),
    expected = unname(expected_results[['p.value']])
  )
})
