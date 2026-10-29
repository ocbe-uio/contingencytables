context("Issue 71")

test_that("exact conditional tests count numerically tied tables", {
  x <- matrix(c(0, 2, 0, 2, 1, 1), 3, byrow = TRUE)
  y <- matrix(c(1, 0, 0, 1, 1, 0), 3, byrow = TRUE)
  tied_statistics <- c(
    "P_FFH", "midP_FFH",
    "P_Pearson", "midP_Pearson",
    "P_LR", "midP_LR",
    "P_KW", "midP_KW"
  )

  for (table in list(x, y)) {
    result <- Exact_cond_midP_tests_rxc(table)
    expect_equal(
      unlist(result[tied_statistics], use.names = FALSE),
      rep(c(1, 0.5), 4)
    )
  }

  result <- Exact_cond_midP_tests_rxc(floor(table_7.3 / 10))
  expect_equal(unname(result$P_LR), 0.5882353, tolerance = 5e-8)
  expect_equal(unname(result$midP_LR), 0.4595588, tolerance = 5e-8)
})
