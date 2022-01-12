context("Chapter 5")

test_that("Chapter 5 functions basically work", {
		n <- rbind(c(8, 53), c(10, 48), c(11, 100), c(22, 102), c(6, 129))
	a <- c(1, 2, 3, 4, 5)
	expect_output(
		object = CochranArmitage_CI_rx2(n, a),
		regexp = "betahat = -0.0194 \\(95% CI -0.0395 to 0.0007\\)"
	)
	expect_output(
		object = CochranArmitage_MH_tests_rx2(n, a),
		regexp = "Mantel-Haenszel test:           T = -1.790, P = 0.07351"
	)
	n <- rbind(c(8, 53), c(10, 48), c(11, 100), c(22, 102))
	a <- c(1, 2, 3, 4)
	expect_output(
		object = CochranArmitage_exact_cond_midP_tests_rx2(n, a),
		regexp = "Cochran-Armitage exact cond. test: P = 0.62494"
	)
	m <- matrix(c(48, 170, 38, 144, 5, 7, 1, 1), byrow=TRUE, ncol=2)
	d <- 'decreasing'
	expect_output(
		object = Exact_cond_midP_unspecific_ordering_rx2(m, d),
		regexp = "Mid-P test:              0.12889"
	)
	n <- matrix(c(8, 53, 10, 48, 11, 100, 22, 102, 6, 129), byrow=TRUE, ncol=2)
	expect_output(
		object = Pearson_LR_tests_unspecific_ordering_rx2(n, 'decreasing'),
		regexp = "Likelihood ratio test:    T = 11.192, P = 0.00252"
	)
	n <- rbind(c(8, 53), c(10, 48), c(11, 100), c(22, 102), c(6, 129))
	a <- c(1, 2, 3, 4, 5)
	expect_output(
		object = Trend_estimate_CI_tests_rx2(n, a),
		regexp = "betahat = -0.1828 \\(95% CI -0.3844 to 0.0188\\)"
	)
	n.0 <- rbind(c(48, 1706), c(38, 1446), c(5, 78), c(1, 12))
	direction <- 'decreasing'
	expect_output(
		object = the_rx2_table(n.0, direction=direction, skip_exact=TRUE),
		regexp = "Cochran-Armitage             0.817           0.41391"
	)
	})
