context("Chapter 10")

test_that("Chapter 10 functions basically work", {
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
  expect_output(
    object = Cochran_Q_test_stratified_2x2(n),
    regexp = "The Cochran Q test \\(MH\\): P = 0.01025, Q = 6.591 \\(df = 1\\)"
  )
  expect_output(
    object = ML_estimates_and_CIs_stratified_2x2(n),
    regexp = "gammahat_2 =  0.2122 \\(95% CI 0.0276 to 0.3967\\)"
  )
  expect_output(
    object = Pearson_LR_homogeneity_test_stratified_2x2(n),
    regexp = "Pearson chi-squared test: P = 0.02471, T0 = 5.044 \\(df = 1\\)"
  )
  expect_output(
    object = Pearson_LR_test_common_effect_stratified_2x2(n),
    regexp = "Pearson chi-squared test: P = 0.00000, T0 = 25.101 \\(df = 1\\)"
  )
  expect_output(
		object = Peto_homogeneity_test_stratified_2x2(n),
		regexp = "The Peto test: P = 0.09297, T0 = 2.822 \\(df = 1\\)"
	)
  expect_output(
		object = Peto_OR_estimate_stratified_2x2(n),
		regexp = "The Peto OR estimate =  3.7120"
	)
  expect_output(
		object = RBG_test_and_CI_stratified_2x2(n),
		regexp = "The RBG CI: thetahatMH = 4.5239 \\(95% CI 2.3556 to 8.6880\\)"
	)
  expect_output(
		object = Wald_test_and_CI_common_diff_stratified_2x2(n),
		regexp = "Wald CI \\(MH\\): deltahat = 0.3294 \\(95% CI 0.2281 to 0.4307\\)"
	)
  expect_output(
		object = Wald_test_and_CI_common_ratio_stratified_2x2(n),
		regexp = "Wald CI \\(MH\\): phihat = 2.4751 \\(95% CI 1.5831 to 3.8698\\)"
	)
  expect_output(
		object = Woolf_test_and_CI_stratified_2x2(n),
		regexp = "The Woolf CI: thetahatIV = 3.5563 \\(95% CI 1.8365 to 6.8866\\)"
	)
  expect_output(
		object = stratified_2x2_tables(n),
		regexp = "Woolf \\(IV\\)           3.5563     1.8365 to  6.8866"
	)
  })
