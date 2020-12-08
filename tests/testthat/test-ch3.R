context("Chapter 3")

test_that("Chapter 3 functions basically work", {
	load_chapter(3)
	expect_output(
		object = Chacko_test_1xc(n=c(1, 4, 3, 11, 9)),
		expected = "The Chacko test: P = 0.00873, T = 9.482 (df = 2)"
	)
	expect_output(
		object = Exact_multinomial_test_1xc(
			n=c(6, 1, 3), pi0=c(0.402, 0.479, 0.119)
		),
		regexp = "P = 0.04792"
	)
})