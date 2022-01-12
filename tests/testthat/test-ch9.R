context("Chapter 9")

test_that("Chapter 9 functions basically work", {
		n <- rbind(c(596, 18, 6, 5), c(0, 2, 0, 0), c(0, 0, 42, 0), c(11, 0, 0, 0))
	expect_output(
		object = Bhapkar_test_paired_cxc(n),
		regexp = "marginal homogenity: P = 0.000005, T = 27.304 \\(df=3\\)"
	)
	expect_output(
		object = Bonferroni_type_CIs_paired_cxc(n),
		regexp = "pi_4\\+ vs pi_ \\+ 4: delta =  0.0088 \\(-0.0059 to  0.0233\\)"
	)
	n2 <- rbind(c(35, 5, 0), c(15, 20, 5), c(10, 5, 5))
	expect_output(
		object = FleissEveritt_test_paired_cxc(n2),
		regexp = "version of the Stuart test: P = 0.000912, T = 14.000 \\(df=2\\)"
	)
	n3 <- rbind(c(596, 18, 6), c(0, 2, 0), c(0, 0, 42))
	expect_output(
		object = FleissLevinPaik_test_paired_cxc(n3),
		regexp = "The Fleiss-Levin-Paik test: P = 0.000004, T = 21.429 \\(df=1\\)"
	)
	expect_output(
		object = McNemarBowker_test_paired_cxc(n),
		regexp = "test for symmetry: P = 0.000200, T0 = 26.250 \\(df=6\\)"
	)
	expect_output(
		object = Scheffe_type_CIs_paired_cxc(n),
		regexp = "pi_4\\+ vs pi_ \\+ 4: delta =  0.0088 \\(-0.0076 to  0.0250\\)"
	)
	n4 <- rbind(
		c(1, 0, 1, 0, 0),
		c(0, 2, 8, 4, 4),
		c(1, 1, 31, 14, 11),
		c(1, 0, 15, 9, 12),
		c(0, 0, 2, 1, 3)
	)
	a <- c(8, 3.5, 0, -3.5, -8)
	expect_output(
		object = Score_test_and_CI_marginal_mean_scores_paired_cxc(n4, a),
		regexp = "The score CI: estimate = 1.6942 \\(95% CI 0.8769 to 2.5115\\)"
	)
	expect_output(
		object = Stuart_test_paired_cxc(n),
		regexp = "marginal homogenity: P = 0.000008, T0 = 26.250 \\(df=3\\)"
	)
	expect_output(
		object = Wald_test_and_CI_marginal_mean_ranks_paired_cxc(n4),
		regexp = "0.6196 \\(95% CI 0.5591 to 0.6800\\); P = 0.00011, Z =  3.877"
	)
	expect_output(
		object = Wald_test_and_CI_marginal_mean_scores_paired_cxc(n4, a),
		regexp = "The Wald CI: estimate = 1.6942 \\(95% CI 0.9347 to 2.4537\\)"
	)
	expect_output(
		object = the_paired_cxc_table_nominal(n),
		regexp = "pi_3\\+ - pi_\\+3:  -0.0088  \\(-0.0187 to  0.0012\\)"
	)
	expect_output(
		object = the_paired_cxc_table_ordinal(n4, a),
		regexp = "Wald logit: estimate = 0.2391 \\(95% CI 0.1151 to 0.3558\\)"
	)
	})
