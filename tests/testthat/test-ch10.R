context("Chapter 10")

test_that("Chapter 10 functions basically work", {
	load_chapter(10)
	n <- array(dim = c(2, 2, 2))
  n[, , 1] <- matrix(c(647, 622, 2, 27), 2, byrow = TRUE)
  n[, , 2] <- matrix(c(41, 28, 19, 32), 2, byrow = TRUE)
	expect_output(
		object = BreslowDay_homogeneity_test_stratified_2x2(n),
		regexp = "The Breslow-Day test: P = 0.02292, T0 = 5.175 \\(df = 1\\)"
	)
  expect_output(
		object = MantelHaenszel_estimate_stratified_2x2(n),
		regexp = "The Mantel-Haenszel estimate =  4.5239"
	)
  expect_output(
		object = CochranMantelHaenszel_test_stratified_2x2(n),
		regexp = "Cochran-Mantel-Haenszel test: P = 0.00000, T0 = 24.920 \\(df = 1"
	)
  expect_output(
		object = InverseVariance_estimate_stratified_2x2(n),
		regexp = "The inverse variance estimate =  3.5563"
	)
  # Cochran_Q_test_stratified_2x2
	unload_chapter(10)
})
