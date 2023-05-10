context("Bootstrapping")

test_that("Insufficient nboot triggers proper error message", {
  msg <- "Insufficient samples. Increase nboo."
  expect_error(
    gamma_coefficient_rxc_bca(table_7.8, nboot = 10), msg
  )
  expect_error(
    Kendalls_tau_b_rxc_bca(table_7.8, nboot = 10), msg
  )
  expect_error(
    Pearson_correlation_coefficient_rxc_bca(table_7.8, nboot = 10), msg
  )
  expect_error(
    Spearman_correlation_coefficient_rxc_bca(table_7.8, nboot = 10), msg
  )
})
