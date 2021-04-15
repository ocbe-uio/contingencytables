context("Chapter 5")

test_that("Chapter 8 functions basically work", {
	load_chapter(5)
	n <- rbind(c(8, 53), c(10, 48), c(11, 100), c(22, 102), c(6, 129))
	a <- c(1, 2, 3, 4, 5)
	expect_output(
		object = CochranArmitage_CI_rx2(n, a),
		regexp = "betahat = -0.0194 \\(95% CI -0.0395 to 0.0007\\)"
	)
	expect_output(
		object = CochranArmitage_exact_cond_midP_tests_rx2(n, a),
		regexp = "Cochran-Armitage exact cond. test: P = 0.08579"
	)
	#
	# expect_output(
	# 	object = ,
	# 	regexp = ""
	# )
	unload_chapter(5)
})