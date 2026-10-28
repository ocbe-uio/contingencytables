context("Issue 72")

test_that("default scores match the row and column dimensions", {
	result_7_8 <- linear_by_linear_test_rxc(table_7.8)
	expect_equal(result_7_8$Z, 7.571748, tolerance = 5e-7)

	result_7_3 <- linear_by_linear_test_rxc(table_7.3)
	expect_equal(result_7_3$Z, -3.476703, tolerance = 5e-7)
	expect_equal(
		result_7_3$Z,
		linear_by_linear_test_rxc(table_7.3, 1:3, 1:2)$Z
	)
})
