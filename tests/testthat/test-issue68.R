context("Issue 68")

test_that("zero-count cells are not treated as observations", {
  linear_by_linear <- linear_by_linear_test_rxc(ritland_2007)
  expect_equal(linear_by_linear$Z, -1.993139, tolerance = 5e-7)
  expect_equal(linear_by_linear$Pvalue, 0.04624624, tolerance = 5e-8)

  n3 <- floor(table_7.7 / 10)
  pearson <- Pearson_correlation_coefficient_rxc(n3)
  expect_equal(pearson$rP, -0.1565616, tolerance = 5e-7)
  expect_equal(c(pearson$lower, pearson$upper), c(-0.3776, 0.0814), tolerance = 5e-5)

  kendall <- Kendalls_tau_b_rxc(n3)
  expect_equal(kendall$tau_b, -0.1235, tolerance = 5e-5)
  expect_equal(c(kendall$lower, kendall$upper), c(-0.2762, 0.0354), tolerance = 5e-5)

  set.seed(1562)
  kendall_bca <- Kendalls_tau_b_rxc_bca(n3, nboot = 200)
  expect_equal(kendall_bca$tau_b, -0.1235, tolerance = 5e-5)
  expect_equal(c(kendall_bca$lower, kendall_bca$upper), c(-0.3415, 0.0690), tolerance = 5e-5)
})

test_that("linear-by-linear and Pearson statistics match the table 7.8 example", {
  linear_by_linear <- linear_by_linear_test_rxc(table_7.8, 1:3, 1:5)
  expect_equal(linear_by_linear$Z, 7.571748, tolerance = 5e-7)

  pearson <- Pearson_correlation_coefficient_rxc(table_7.8)
  expect_equal(pearson$rP, 0.6590361, tolerance = 5e-7)
})
