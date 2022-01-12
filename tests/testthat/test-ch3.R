context("Chapter 3")

test_that("Chapter 3 functions basically work", {
		expect_output(
		object = Chacko_test_1xc(n=c(1, 4, 3, 11, 9)),
		regexp = "P = 0.00873, T = 9.482 \\(df = 2\\)"
	)
	expect_output(
		object = Exact_multinomial_test_1xc(
			n=c(6, 1, 3), pi0=c(0.402, 0.479, 0.119)
		),
		regexp = "P = 0.04792"
	)
	expect_output(
		object = Gold_Wald_CIs_1xc(n=c(276, 380, 118)),
		regexp = "pi_3: estimate = 0.1525 \\(0.1208 to 0.1841\\)"
	)
	expect_output(
		object = Goodman_Wald_CIs_1xc(n=c(276, 380, 118)),
		regexp = "pi_3: estimate = 0.1525 \\(0.1215 to 0.1834\\)"
	)
	expect_output(
		object = Goodman_Wald_CIs_for_diffs_1xc(n=c(276, 380, 118)),
		regexp = "pi_2 - pi_3: estimate = 0.3385 \\(0.2759 to 0.4011\\)"
	)
	expect_output(
		object = Goodman_Wilson_score_CIs_1xc(n=c(276, 380, 118)),
		regexp = "pi_3: estimate = 0.1525 \\(0.1241 to 0.1859\\)"
	)
	expect_output(
		object = LR_test_1xc(n=c(6, 1, 3), pi0=c(0.402, 0.479, 0.119)),
		regexp = "P = 0.02704, T = 7.221 \\(df = 2\\)"
	)
	expect_output(
		object = MidP_multinomial_test_1xc(
			n=c(6, 1, 3), pi0=c(0.402, 0.479, 0.119)
		),
		regexp = "P = 0.04649"
	)
	expect_output(
		object = Pearson_chi_squared_test_1xc(
			n=c(276, 380, 118), pi0=c(0.402, 0.479, 0.119)
		),
		regexp = "P = 0.00321, T = 11.481 \\(df = 2\\)"
	)
	expect_output(
		object = QuesenberryHurst_Wilson_score_CIs_1xc(n=c(276, 380, 118)),
		regexp = "pi_2: estimate = 0.4910 \\(0.4472 to 0.5348\\)"
	)
	expect_output(
		object = the_1xc_table_CIs(n=c(276, 380, 118)),
		regexp = "Gold Wald                       0.1208 to 0.1841    0.0633"
	)
	expect_output(
		object = the_1xc_table_tests(n=c(6, 1, 3), pi0=c(0.402, 0.479, 0.119)),
		regexp = "Pearson chi-squared    0.0346   \\(T = 6.727, df = 2\\)"
	)
	})
