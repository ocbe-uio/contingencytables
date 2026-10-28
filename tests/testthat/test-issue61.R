context("Issue 61")

test_that("exact unconditional McNemar test works with no discordant pairs", {
	# a = 3, b = 0, c = 0, d = 3
	tab <- matrix(c(3, 0, 0, 3), nrow = 2)
	result <- McNemar_exact_unconditional_test_paired_2x2(tab)
	expect_equal(result$Pvalue, 1)

	# Symmetric layout (b and c swapped); should be irrelevant
	result_t <- McNemar_exact_unconditional_test_paired_2x2(t(tab))
	expect_equal(result_t$Pvalue, 1)
})
