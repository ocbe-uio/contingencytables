context("print() method for contingencytables_outputs")

test_that("Printing method works for lower_upper_estimate_alpha", {
  expect_silent(Adjusted_inv_sinh_CI_OR_2x2(lampasona_2013))
  expect_invisible(tmp <- Adjusted_inv_sinh_CI_OR_2x2(lampasona_2013))
  expect_silent(tmp)
  expect_output(print(tmp), "The adjusted inverse sinh CI")
  expect_length(tmp$statistics, 5)
})

test_that("Printing method works for pvalue_df_estimate_statname", {
  expect_silent(Bhapkar_test_paired_cxc(peterson_2007))
  expect_invisible(tmp <- Bhapkar_test_paired_cxc(peterson_2007))
  expect_silent(tmp)
  expect_output(print(tmp), "The Bhapkar test for marginal homogenity")
  expect_length(tmp$statistics, 4)
})

test_that("Printing method works for pvalue", {
  expect_silent(Blaker_exact_test_1x2(13, 16, 0.5))
  expect_invisible(tmp <- Blaker_exact_test_1x2(13, 16, 0.5))
  expect_silent(tmp)
  expect_output(print(tmp), "The Blaker exact test")
  expect_length(tmp$statistics, 2)
})

# TODO: add other print methods for contingencytables_output