context("Chapter 10")

test_that("Chapter 10 functions basically work", {
	load_chapter(10)
	n <- array(dim = c(2, 2, 2))
  n[, , 1] <- matrix(c(647, 622, 2, 27), 2, byrow = TRUE)
  n[, , 2] <- matrix(c(41, 28, 19, 32), 2, byrow = TRUE)
	expect_output(
		object = MantelHaenszel_estimate_stratified_2x2(n),
		regexp = "The Mantel-Haenszel estimate =  4.5239"
	)
	unload_chapter(10)
})
