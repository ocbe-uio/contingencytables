context("Argument validation")

test_that("Invalid arguments are picked up", {
  expect_error(Adjusted_inv_sinh_CI_OR_2x2(-ritland_2007), "counts")
  expect_error(Adjusted_inv_sinh_CI_OR_2x2(ritland_2007, -3), "positive")
  expect_error(Adjusted_inv_sinh_CI_OR_2x2(ritland_2007, alpha = 1.1), "prob")
})
