context("Chapter 7")

test_that("Chapter 7 functions basically work", {
  n_short <- floor(table_7.3 / 10)
  expect_output(
    object = print(Bonferroni_type_CIs_rxc(table_7.3)),
    regexp = "pi_1|1 - pi_1|3: estimate = -0.2476 \\(-0.4213 to -0.0740\\)"
  )
  n2 <- rbind(c(51, 7, 6), c(22, 4, 12), c(24, 9, 10))
  expect_output(
    object = print(Cumulative_models_for_rxc(n2)),
    regexp = "Likelihood ratio\\s+P =  0.00832, T =  9.579 \\(df=2\\)"
  )
  expect_output(
    object = print(Cumulative_models_for_rxc(table_7.5, "probit")),
    regexp = "Wald \\(Z-statistic\\) row 5 vs row 1    -0.509     0.611037"
  )
  expect_error(Cumulative_models_for_rxc(n2[, 1, drop = FALSE]), "must have at least 3")
  expect_error(Cumulative_models_for_rxc(n2[, 1:2]), "must have at least 3")
  expect_output(
    object = print(Exact_cond_midP_tests_rxc(n_short)),
    regexp = "Exact linear-by-linear:      P = 0.9754902"
  )
  expect_output(
    object = print(Exact_cond_midP_tests_rxc(ceiling(table_7.6 / 10))),
    regexp = "Mid-P LR statistic:          P = 0.9237914"
  )
  expect_error(
    object = Exact_cond_midP_tests_rxc(matrix(1:16, 4)),
    regexp = "Too elaborate..."
  )
  expect_output(
    object = print(FisherFreemanHalton_asymptotic_test_rxc(table_7.3)),
    regexp = "Fisher-Freeman-Halton asymptotic test: P = 0.0003, T = 16.260"
  )
  expect_output(
    object = print(FisherFreemanHalton_asymptotic_test_rxc(matrix(c(4, 5, 0, 0), 2))),
    regexp = "Halton asymptotic test: P = 1.0000, T = -0.000 \\(df=1\\)"
  )
  n3_short <- floor(table_7.7 / 10)
  expect_output(
    object = {
      set.seed(1562)
      print(gamma_coefficient_rxc_bca(n3_short, nboot = 200))
    },
    regexp = "-0.2137 \\(95% CI -0.5268 to  0.1931\\)"
  )
  expect_output(
    object = print(gamma_coefficient_rxc(n3_short)),
    regexp = "The proportion of discordant pairs:  0.606838"
  )
  expect_output(
    object = print(JonckheereTerpstra_test_rxc(table_7.7)),
    regexp = "Terpstra test for association: P = 0.006720, Z = -2.710"
  )
  expect_output(
    regexp = "Fieller CI: tau-b = -0.1235 \\(95% CI -0.2762 to  0.0354\\)",
    object = print(Kendalls_tau_b_rxc(n3_short)),
  )
  expect_output(
    object = {
      set.seed(1562)
      print(Kendalls_tau_b_rxc_bca(n3_short, nboot = 200))
    },
    regexp = "bootstrap CI: tau-b = -0.1235 \\(95% CI -0.0863 to  0.3904\\)"
  )
  expect_output(
    object = print(KruskalWallis_asymptotic_test_rxc(table_7.6)),
    regexp = "Asymptotic Kruskal-Wallis test: T =  9.162, df = 2, P = 0.010"
  )
  expect_output(
    object = print(linear_by_linear_test_rxc(table_7.7)),
    regexp = "linear test for association: P = 0.004321, Z = -2.854",
  )
  expect_output(
    object = print(Pearson_correlation_coefficient_rxc(n3_short)),
    regexp = "coefficient: r =  0.2019 \\(95% CI -0.0347 to  0.4171\\)"
  )
  expect_output(
    object = {
      set.seed(1562)
      print(
        Pearson_correlation_coefficient_rxc_bca(n3_short, nboot = 200, alpha = .2)
      )
    },
    regexp = "bootstrap CI: r =  0.2019 \\(80% CI  0.1028 to  0.3698\\)"
  )
  expect_output(
    object = print(Pearson_LR_tests_rxc(table_7.3)),
    regexp = "Pearson chi-squared test: T = 17.562, df = 2, P = 0.00015"
  )
  expect_output(
    object = print(Pearson_residuals_rxc(table_7.3)),
    regexp = "Pearson residuals:"
  )
  expect_equal(
    object = dim(Pearson_residuals_rxc(table_7.3)$residuals),
    expected = c(3, 2)
  )
  expect_output(
    object = print(Scheffe_type_CIs_rxc(table_7.3)),
    regexp = "pi_1|2 - pi_1|3: estimate = 0.0222 \\(-0.1181 to 0.1625\\)"
  )
  expect_output(
    object = print(Spearman_correlation_coefficient_rxc(n3_short)),
    regexp = "Wright CI: rho = -0.1358 \\(95% CI -0.3603 to  0.1035\\)"
  )
  expect_output(
    object = {
      set.seed(562)
      print(Spearman_correlation_coefficient_rxc_bca(n3_short, nboot = 200))
    },
    regexp = "bootstrap CI: rho = -0.1358 \\(95% CI -0.3636 to  0.1118\\)"
  )
  expect_output(
    object = {
      set.seed(7494)
      the_rxc_table(n3_short, nboot = 250, alpha = 0.2)
    },
    regexp = "Kruskal-Wallis asymptotic\\s+1.561 \\(df=3\\)   0.668229"
  )
  expect_output(
    object = the_rxc_table(matrix(1:6, 3)),
    regexp = "  Fisher-Freeman-Halton asymptotic\\s+0.428 \\(df=2\\)   0.807209"
  )
  n4_short <- floor(table_7.4 / 10)
  expect_output(
    object = the_rxc_table(n4_short, nboot = 0),
    regexp = "  pi_1|6 - pi_1|6: estimate = 0.0000 \\(-0.8987 to 0.8987\\)"
  )
})
