context("Chapter 5")

test_that("Chapter 5 functions basically work", {
	# TODO: #18 speed up tests
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
	expect_output(
		object = CochranArmitage_MH_tests_rx2(n, a),
		regexp = "Mantel-Haenszel test:           T = -1.790, P = 0.07351"
	)
	m <- matrix(
		c(48, 17066, 38, 14464, 5, 788, 1, 126, 1, 37), byrow=TRUE, ncol=2
	)
	d <- 'decreasing'
	expect_output(
		object = Exact_cond_midP_unspecific_ordering_rx2(m, d), #TODO: n faster?
		regexp = "Mid-P test:              0.02292"
	)
	# expect_output(
	# 	object = ,
	# 	regexp = ""
	# )
	unload_chapter(5)
})
