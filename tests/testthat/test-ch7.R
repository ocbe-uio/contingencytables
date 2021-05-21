context("Chapter 7")

test_that("Chapter 7 functions basically work", {
	load_chapter(7)
	n <- matrix(c(40, 25, 54, 7, 63, 10), byrow=TRUE, ncol=2)
	expect_output(
		object = Pearson_LR_tests_rxc(n),
		regexp = "Pearson chi-squared test: T = 17.562, df = 2, P = 0.00015"
	)
	unload_chapter(7)
})