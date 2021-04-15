context("Chapter 8")

test_that("Chapter 8 functions basically work", {
	load_chapter(8)
	n <- matrix(c(59, 6, 16, 80), 2, byrow=TRUE)
	expect_output(
		object = MOVER_Wilson_score_CI_paired_2x2(n),
		regexp = "estimate = 0.8667 \\(95% CI 0.7592 to 0.9866\\)"
	)
	unload_chapter(8)
})