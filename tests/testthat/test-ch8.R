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
	# expect_output(
	# 	object = ,
	# 	regexp = ""
	# )
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
	unload_chapter(8)
})
