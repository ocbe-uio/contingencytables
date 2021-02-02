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
		regexp = "estimate = 0.2000 \\(95% CI 0.0151 to 1.7523\\)"
	)
	expect_output(
		object = BaptistaPike_midP_CI_2x2(rbind(c(15, 30), c(2, 3))),
		regexp = "estimate = 0.7500 \\(95% CI 0.1415 to 4.6034\\)"
	)
	expect_output(
		object = Cornfield_exact_conditional_CI_2x2(rbind(c(9,4), c(4,10))),
		regexp = "estimate = 5.6250 \\(95% CI 1.0226 to 31.5025\\)"
	)
	expect_output(
		object = Cornfield_midP_CI_2x2(rbind(c(3,1), c(1,3))),
		regexp = "estimate = 9.0000 \\(95% CI 0.3101 to 308.5568\\)"
	)
	expect_output(
		object = Fisher_exact_test_2x2(rbind(c(0,16), c(15,57))),
		regexp = "P = 0.06287"
	)
})