context("Chapter 4")

test_that("Chapter 4 functions basically work", {
	load_chapter(4)
	n <- matrix(c(9, 4, 4, 10), nrow=2, byrow=T)
	expect_output(
		object = Adjusted_inv_sinh_CI_OR_2x2(n),
		regexp = "estimate = 5.6250 \\(95% CI 1.1414 to 21.7873\\)"
	)
	n <- matrix(c(0,16,15,57), nrow=2, byrow=T)
	expect_output(
		object = Adjusted_inv_sinh_CI_ratio_2x2(n),
		regexp = "estimate = 0.0000 \\(95% CI 0.0000 to 1.1524\\)"
	)
	n <- matrix(c(7,27,1,33), nrow=2, byrow=T)
	expect_output(
		object = Adjusted_log_CI_2x2(n),
		regexp = "estimate = 7.0000 \\(95% CI 0.9241 to 27.0523\\)"
	)
	expect_output(
		object = Wald_CI_2x2(n = matrix(c(7,27,1,33), nrow=2, byrow=T)),
		regexp = "estimate = 0.1765 \\(95% CI 0.0292 to 0.3238\\)"
	)
	expect_output(
		object = AgrestiCaffo_CI_2x2(n = matrix(c(7,27,1,33), nrow=2, byrow=T)),
		regexp = "estimate = 0.1765 \\(95% CI 0.0116 to 0.3217\\)"
	)
		expect_output(
		object = BaptistaPike_exact_conditional_CI_2x2(rbind(c(1, 7), c(5, 7))),
		regexp = "estimate = 0.2000 \\(95% CI 0.0075 to 2.2069\\)"
	)
})