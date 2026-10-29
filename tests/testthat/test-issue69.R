context("Issue 69")

test_that("exact two-sided p-values are invariant to row order", {
  result <- Exact_cond_midP_tests_rxc(table_7.3)
  reversed <- Exact_cond_midP_tests_rxc(table_7.3[3:1, ])
  p_values <- c("P_lbl", "midP_lbl", "P_JT", "midP_JT")

  expect_equal(result$P_lbl, 0.0005126, tolerance = 5e-8)
  expect_equal(result$midP_lbl, 0.0004197, tolerance = 5e-8)
  expect_equal(result$P_JT, 0.0005177, tolerance = 5e-8)
  expect_equal(result$midP_JT, 0.0005140, tolerance = 5e-8)
  expect_equal(
    unlist(result[p_values], use.names = FALSE),
    unlist(reversed[p_values], use.names = FALSE)
  )
})

test_that("asymptotic ordered tests return two-sided p-values", {
  linear_by_linear <- linear_by_linear_test_rxc(table_7.3, 1:3, 1:2)
  jonckheere_terpstra <- JonckheereTerpstra_test_rxc(table_7.3)

  expect_equal(linear_by_linear$Pvalue, 0.000508, tolerance = 5e-7)
  expect_equal(jonckheere_terpstra$Pvalue, 0.000589, tolerance = 5e-7)
})
