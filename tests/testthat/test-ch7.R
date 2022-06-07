context("Chapter 7")

test_that("Chapter 7 functions basically work", {
  n <- matrix(c(40, 25, 54, 7, 63, 10), byrow = TRUE, ncol = 2)
  n_short <- floor(n / 10)
  expect_output(
    object = Bonferroni_type_CIs_rxc(n),
    regexp = "pi_1|1 - pi_1|3: estimate = -0.2476 \\(-0.4213 to -0.0740\\)"
  )
  n2 <- rbind(c(51, 7, 6), c(22, 4, 12), c(24, 9, 10))
  expect_output(
    object = Cumulative_models_for_rxc(n2),
    regexp = "Likelihood ratio\\s+P =  0.00832, T =  9.579 \\(df=2\\)"
  )
  expect_output(
    object = Exact_cond_midP_tests_rxc(n_short),
    regexp = "Exact linear-by-linear:      P = 0.9754902"
  )
  expect_output(
    object = FisherFreemanHalton_asymptotic_test_rxc(n),
    regexp = "Fisher-Freeman-Halton asymptotic test: P = 0.0003, T = 16.260"
  )
  n3 <- matrix(
    c(2, 4, 29, 19, 7, 6, 116, 51, 19, 27, 201, 76, 18, 22, 133, 54),
    ncol = 4, byrow = TRUE
  )
  n3_short <- floor(n3 / 10)
  expect_output(
    object = {
      set.seed(1562)
      gamma_coefficient_rxc_bca(n3_short, nboot = 200)
    },
    regexp = "-0.2137 \\(95% CI -0.5268 to  0.1931\\)"
  )
  expect_output(
    object = gamma_coefficient_rxc(n3_short),
    regexp = "The proportion of discordant pairs:  0.606838"
  )
  expect_output(
    object = JonckheereTerpstra_test_rxc(n3),
    regexp = "Terpstra test for association: P = 0.006720, Z = -2.710"
  )
  expect_output(
    regexp = "Fieller CI: tau-b = -0.1235 \\(95% CI -0.2762 to  0.0354\\)",
    object = Kendalls_tau_b_rxc(n3_short),
  )
  expect_output(
    object = {
      set.seed(1562)
      Kendalls_tau_b_rxc_bca(n3_short, nboot = 200)
    },
    regexp = "bootstrap CI: tau-b = -0.1235 \\(95% CI -0.0863 to  0.3904\\)"
  )
  n4 <- rbind(c(22, 4, 12), c(24, 9, 10), c(51, 7, 6))
  expect_output(
    object = KruskalWallis_asymptotic_test_rxc(n4),
    regexp = "Asymptotic Kruskal-Wallis test: T =  9.162, df = 2, P = 0.010"
  )
  expect_output(
    object = linear_by_linear_test_rxc(n3),
    regexp = "linear test for association: P = 0.004321, Z = -2.854",
  )
  expect_output(
    object = Pearson_correlation_coefficient_rxc(n3_short),
    regexp = "coefficient: r =  0.2019 \\(95% CI -0.0347 to  0.4171\\)"
  )
  expect_output(
    object = {
      set.seed(1562)
      Pearson_correlation_coefficient_rxc_bca(n3_short, nboot = 2000)
    },
    regexp = "bootstrap CI: r =  0.2019 \\(95% CI  0.0862 to  0.3876\\)"
  )
  expect_output(
    object = Pearson_LR_tests_rxc(n),
    regexp = "Pearson chi-squared test: T = 17.562, df = 2, P = 0.00015"
  )
  expect_equal(
    object = dim(Pearson_residuals_rxc(n, printresults = FALSE)$residuals),
    expected = c(3, 2)
  )
  expect_output(
    object = Scheffe_type_CIs_rxc(n),
    regexp = "pi_1|2 - pi_1|3: estimate = 0.0222 \\(-0.1181 to 0.1625\\)"
  )
  expect_output(
    object = Spearman_correlation_coefficient_rxc(n3_short),
    regexp = "Wright CI: rho = -0.1358 \\(95% CI -0.3603 to  0.1035\\)"
  )
  expect_output(
    object = {
      set.seed(562)
      Spearman_correlation_coefficient_rxc_bca(n3_short, nboot = 200)
    },
    regexp = "bootstrap CI: rho = -0.1358 \\(95% CI -0.3636 to  0.1118\\)"
  )
  expect_output(
    object = the_rxc_table(n3_short, nboot = 3000),
    regexp = "Kruskal-Wallis asymptotic\\s+1.561 \\(df=3\\)   0.668229"
  )
})
