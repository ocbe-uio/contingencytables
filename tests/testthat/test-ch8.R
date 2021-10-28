context("Chapter 8")

test_that("Chapter 8 functions basically work", {
	load_chapter(8)
	n <- matrix(c(59, 6, 16, 80), 2, byrow=TRUE)
	expect_output(
		object = BonettPrice_hybrid_Wilson_score_CI_CC_paired_2x2(n),
		regexp = "CI w / CC: estimate = 0.8667 \\(95% CI 0.7475 to 1.0058\\)"
	)
	expect_output(
		object = BonettPrice_hybrid_Wilson_score_CI_paired_2x2(n),
		regexp = "estimate = 0.8667 \\(95% CI 0.7579 to 0.9910\\)"
	)
	expect_output(
		object = ClopperPearson_exact_CI_1x2_beta_version(X = 13, n = 16),
		regexp = "exact CI: estimate = 0.8125 \\(95% CI 0.5435 to 0.9595\\)"
	)
	expect_output(
		object = McNemar_asymptotic_test_CC_paired_2x2(n),
		regexp = "McNemar test with continuity correction: P = 0.055009, Z =  1.919"
	)
	expect_output(
		object = McNemar_asymptotic_test_paired_2x2(n),
		regexp = "The McNemar asymptotic test: P = 0.033006, Z = -2.132"
	)
	expect_output(
		object = McNemar_exact_cond_test_paired_2x2(n),
		regexp = "The McNemar exact conditional test: P = 0.052479"
	)
	expect_output(
		object = McNemar_exact_unconditional_test_paired_2x2(n),
		regexp = "The McNemar exact unconditional test: P = 0.034053"
	)
	expect_output(
		object = McNemar_midP_test_paired_2x2(n),
		regexp = "The McNemar mid-P test: P = 0.034690"
	)
	expect_output(
		object =  Tang_asymptotic_score_CI_paired_2x2(n),
		regexp = "estimate = 0.8667 \\(95% CI 0.7476 to 0.9876\\)"
	)
	expect_output(
		object = Tango_asymptotic_score_CI_paired_2x2(n),
		regexp = "score CI: estimate = -0.0621 \\(95% CI -0.1240 to -0.0054\\)"
	)
	expect_output(
		object = Wald_CI_diff_paired_2x2(n),
		regexp = "The Wald CI: estimate = -0.0621 \\(95% CI -0.1184 to -0.0058\\)"
	)
	expect_output(
		object = Wald_CI_diff_CC_paired_2x2(n),
		regexp = "estimate = -0.0621 \\(95% CI -0.1186 to -0.0057\\)"
	)
	expect_output(
		object = Wald_CI_AgrestiMin_paired_2x2(n),
		regexp = "adjustment: estimate = -0.0621 \\(95% CI -0.1182 to -0.0045\\)"
	)
	expect_output(
		object = MOVER_Wilson_score_CI_paired_2x2(n),
		regexp = "estimate = 0.8667 \\(95% CI 0.7592 to 0.9866\\)"
	)
	expect_output(
		object = Newcombe_square_and_add_CI_paired_2x2(n),
		regexp = "estimate = -0.0621 \\(95% CI -0.1186 to -0.0046\\)"
	)
	expect_output(
		object = Transformed_Wilson_score_CI_paired_2x2(n),
		regexp = "0.3750 \\(95% CI 0.1514 to 0.9287\\)"
	)
	expect_output(
		object = Wald_CI_BonettPrice_paired_2x2(n),
		regexp = "adjustment: estimate = -0.0621 \\(95% CI -0.1195 to -0.0032\\)"
	)
	# expect_output(
	# 	object = the_paired_2x2_table_CIs_difference(n),
	# 	regexp = ""
	# )
	# expect_output(
	# 	object = (n),
	# 	regexp = ""
	# )
	# expect_output(
	# 	object = (n),
	# 	regexp = ""
	# )
	# expect_output(
	# 	object = (n),
	# 	regexp = ""
	# )
	# expect_output(
	# 	object = (n),
	# 	regexp = ""
	# )
	# expect_output(
	# 	object = (n),
	# 	regexp = ""
	# )

	unload_chapter(8)
})
