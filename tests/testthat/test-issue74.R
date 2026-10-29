context("Issue 74")

test_that("very small p-values are displayed without rounding to zero", {
  result_7_8 <- linear_by_linear_test_rxc(table_7.8)
  output_7_8 <- capture.output(print(result_7_8))

  expect_gt(result_7_8$Pvalue, 0)
  expect_match(output_7_8, "e-14")
  expect_false(grepl("P = 0.000000", output_7_8, fixed = TRUE))
  expect_match(contingencytables:::sprintf("P = %7.5f", 3.68594e-14), "e-14")
  expect_identical(sprintf("P = %7.5f", 0.000123456), "P = 0.00012")
})

test_that("normal upper-tail p-values do not underflow through subtraction", {
  result_7_9 <- linear_by_linear_test_rxc(table_7.9)

  expect_gt(result_7_9$Pvalue, 0)
  expect_equal(
    result_7_9$Pvalue,
    2 * pnorm(abs(result_7_9$Z), lower.tail = FALSE)
  )

  table_output <- capture.output(the_rxc_table(table_7.9, nboot = 0))
  expect_true(any(grepl("Linear-by-linear.*e-53", table_output)))
  expect_false(any(grepl("Linear-by-linear.*0.000000", table_output)))
})