context("Chapter 4")

test_that("Chapter 4 functions basically work", {
	load_chapter(4)
	n <- matrix(c(9,4,4,10), nrow=2, byrow=T)
	expect_output(
		object = Adjusted_inv_sinh_CI_OR_2x2(n),
		regexp = " estimate = 5.6250 \\(95% CI 1.1414 to 21.7873\\)"
	)
})