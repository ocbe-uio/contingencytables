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
	expect_output(
		object = Exact_unconditional_test_2x2(rbind(c(3,1), c(1,3))),
		regexp = "The Suissa-Shuster exact unconditional test: P = 0.28916"
	)
	expect_output(
		object = Fisher_midP_test_2x2(rbind(c(0,16), c(15,57))),
		regexp = "The Fisher mid-P test \\(Fisher-Irwin\\): P = 0.04466"
	)
	n <- matrix(c(0,16,15,57), nrow=2, byrow=T)
	expect_output(
		object = Gart_adjusted_logit_CI_2x2(n),
		regexp = "estimate = 0.0000 \\(95% CI 0.0064 to 1.9804\\)"
	)
	n <- matrix(c(9,4,4,10), nrow=2, byrow=T)
	expect_output(
		object = Independence_smoothed_logit_CI_2x2(n),
		regexp = "estimate = 5.6250 \\(95% CI 1.0206 to 23.7777\\)"
	)
	n <- matrix(c(9,4,4,10), nrow=2, byrow=T)
	expect_output(
		object = Inv_sinh_CI_OR_2x2(n),
		regexp = "estimate = 5.6250 \\(95% CI 1.2472 to 25.3686\\)"
	)
	n <- matrix(c(7,27,1,33), nrow=2, byrow=TRUE)
	expect_output(
		object = Inv_sinh_CI_ratio_2x2(n),
		regexp = "estimate = 7.0000 \\(95% CI 1.1671 to 41.9827\\)"
	)
	n <- matrix(c(7,27,1,33), nrow=2, byrow=TRUE)
	expect_output(
		object = Katz_log_CI_2x2(n),
		regexp = "estimate = 7.0000 \\(95% CI 0.9096 to 53.8695\\)"
	)
	n <- matrix(c(7,27,1,33), nrow=2, byrow=TRUE)
	expect_output(
		object = Koopman_asymptotic_score_CI_2x2(n),
		regexp = "estimate = 7.0000 \\(95% CI 1.2209 to 42.5757\\)"
	)
	n <- rbind(c(3,1), c(1,3))
	expect_output(
		object = LR_test_2x2(n),
		regexp = "P = 0.14798, T = 2.093 \\(df = 1\\)"
	)
	n <- matrix(c(7,27,1,33), nrow=2, byrow=TRUE)
	expect_output(
		object =  Mee_asymptotic_score_CI_2x2(n),
		regexp = "estimate = 0.1765 \\(95% CI 0.0284 to 0.3439\\)"
	)
	n <- matrix(c(0,16,15,57), nrow=2, byrow=TRUE)
	expect_output(
		object =  MiettinenNurminen_asymptotic_score_CI_difference_2x2(n),
		regexp = "estimate = -0.2083 \\(95% CI -0.3164 to -0.0056\\)"
	)
	n <- matrix(c(9,4,4,10), nrow=2, byrow=TRUE)
	expect_output(
		object =  MiettinenNurminen_asymptotic_score_CI_OR_2x2(n),
		regexp = "5.6250 \\(95% CI 1.0934 to 28.9419\\)"
	)
	n <- matrix(c(7, 27, 1, 33), nrow=2, byrow=TRUE)
	expect_output(
		object =  MiettinenNurminen_asymptotic_score_CI_ratio_2x2(n),
		regexp = "estimate = 7.0000 \\(95% CI 1.2086 to 43.0330\\)"
	)
	n <- matrix(c(9,4,4,10), nrow=2, byrow=TRUE)
	expect_output(
		object =  MOVER_R_Wilson_CI_OR_2x2(n),
		regexp = "estimate = 5.6250 \\(95% CI 1.0433 to 20.5670\\)"
	)
})