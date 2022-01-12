context("Chapter 2")

test_that("Chapter 2 functions basically work", {
		expect_error(Wald_CI_1x2(100))
	expect_output(
		object = Wald_CI_1x2(1, 2),
		regexp = "estimate = 0.5000 \\(95% CI 0.0000 to 1.0000\\)"
	)
	expect_error(AgrestiCoull_CI_1x2(19))
	expect_output(
		object = AgrestiCoull_CI_1x2(19, 20, .15),
		regexp = "estimate = 0.8750 \\(85% CI 0.7778 to 0.9722\\)"
	)
	expect_error(Arcsine_CI_1x2(500))
	expect_output(
		object = Arcsine_CI_1x2(100, 5e3, .1),
		regexp = "estimate = 0.0200 \\(90% CI 0.0169 to 0.0235\\)"
	)
	expect_error(Blaker_exact_CI_1x2(1))
	expect_output(
		object = Blaker_exact_CI_1x2(1, 100),
		regexp = "estimate = 0.0100 \\(95% CI 0.0005 to 0.0513\\)"
	)
	expect_error(Blaker_exact_test_1x2(1))
	expect_output(
		object = Blaker_exact_test_1x2(1, 10, .5),
		regexp = "P = 0.02148"
	)
	expect_error(Blaker_midP_CI_1x2(100))
	expect_output(
		object = Blaker_midP_CI_1x2(100, 500, .5),
		regexp = "estimate = 0.2000 \\(50% CI 0.1881 to 0.2121\\)"
	)
	expect_error(Blaker_midP_test_1x2(100))
	expect_output(
		object = Blaker_midP_test_1x2(X=13, n=16, pi0=0.5),
		regexp = "P = 0.00845"
	)
	expect_error(ClopperPearson_exact_CI_1x2(100))
	expect_output(
		object = ClopperPearson_exact_CI_1x2(X=13, n=16),
		regexp = "estimate = 0.8125 \\(95% CI 0.5435 to 0.9595\\)"
	)
	expect_error(ClopperPearson_midP_CI_1x2(100))
	expect_output(
		object = ClopperPearson_midP_CI_1x2(X=13, n=16),
		regexp = "estimate = 0.8125 \\(95% CI 0.5699 to 0.9500\\)"
	)
	expect_error(Exact_binomial_test_1x2(100))
	expect_output(
		object = Exact_binomial_test_1x2(X = 13, n = 16, pi0 = 0.5),
		regexp = "P = 0.02127"
	)
	expect_error(Jeffreys_CI_1x2(100))
	expect_output(
		object = Jeffreys_CI_1x2(X = 13, n = 16),
		regexp = "estimate = 0.8125 \\(95% CI 0.5792 to 0.9442\\)"
	)
	expect_error(LR_CI_1x2(100))
	expect_output(
		object = LR_CI_1x2(X = 13, n = 16),
		regexp = "estimate = 0.8125 \\(95% CI 0.5828 to 0.9497\\)"
	)
	expect_error(LR_test_1x2(100))
	expect_output(
		object = LR_test_1x2(X = 13, n = 16, pi0 = .5),
		regexp = "P = 0.00944, T = 6.738 \\(df <- 1\\)"
	)
	expect_error(MidP_binomial_test_1x2(100))
	expect_output(
		object = MidP_binomial_test_1x2(X = 13, n = 16, pi0 = .5),
		regexp = "P = 0.01273"
	)
	expect_error(Score_test_1x2(100))
	expect_output(
		object = Score_test_1x2(X = 13, n = 16, pi0 = .5),
		regexp = "P = 0.01242, Z =  2.500"
	)
	expect_error(Score_test_CC_1x2(100))
	expect_output(
		object = Score_test_CC_1x2(X = 13, n = 16, pi0 = .5),
		regexp = "P = 0.02445, Z = 2.250"
	)
	expect_error(Wald_CI_CC_1x2(100))
	expect_output(
		object = Wald_CI_CC_1x2(X = 13, n = 16, alpha = .1),
		regexp = "estimate = 0.8125 \\(90% CI 0.6207 to 1.0000\\)"
	)
	expect_error(Wilson_score_CI_1x2(100))
	expect_output(
		object = Wilson_score_CI_1x2(X=13, n=16),
		regexp = "estimate = 0.8125 \\(95% CI 0.5699 to 0.9341\\)"
	)
	expect_error(Wilson_score_CI_CC_1x2(100))
	expect_output(
		object = Wilson_score_CI_CC_1x2(X=13, n=16),
		regexp = "estimate = 0.8125 \\(95% CI 0.5369 to 0.9503\\)"
	)
	expect_error(the_1x2_table_CIs(100))
	expect_output(
		object = the_1x2_table_CIs(X=13, n=16),
		regexp = "Estimate of pi: 13 / 16 = 0.812 "
	)
	expect_error(Wald_test_1x2(100))
	expect_output(
		object = Wald_test_1x2(X=13, n=16, pi0=0.1),
		regexp = "P = 0.00000, Z =  7.302"
	)
	expect_error(Wald_test_CC_1x2(100))
	expect_output(
		object = Wald_test_CC_1x2(X=13, n=16, pi0=0.1),
		regexp = "P = 0.00000, Z = 6.982"
	)
	expect_error(the_1x2_table_tests(100))
	expect_output(
		object = the_1x2_table_tests(X=13, n=16, pi0=0.5),
		regexp = "H_0: pi = 0.500  vs  H_A: pi ~= 0.500"
	)
	})
