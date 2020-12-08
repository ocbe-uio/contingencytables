context("Chapter 3")

test_that("Chapter 3 functions basically work", {
	load_chapter(3)
	expect_output(
		object = Chacko_test_1xc(n=c(1, 4, 3, 11, 9)),
		expected = "The Chacko test: P = 0.00873, T = 9.482 (df = 2)"
	)
})