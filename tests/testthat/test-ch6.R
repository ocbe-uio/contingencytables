context("Chapter 6")

test_that("Chapter 6 functions basically work", {
  n_short <- floor(lydersen_2012a / 2)
  expect_output(
    object = print(Brant_test_2xc(lydersen_2012a)),
    regexp = "P = 0.434217, T = 1.668 \\(df = 2\\)"
  )
  expect_output(
    object = print(Cumulative_models_for_2xc(lydersen_2012a)),
    regexp = "Wald \\(Z-statistic\\):          P =  0.14097, Z = -1.472"
  )
  expect_output(
    object = print(Exact_cond_midP_linear_rank_tests_2xc(n_short)),
    regexp = "Exact cond. linear rank test: P = 0.23854"
  )
  dir <- "decreasing"
  expect_output(
    object = print(Exact_cond_midP_unspecific_ordering_rx2(t(n_short), dir)),
    regexp = "Exact conditional test\\s*:\\s*P =\\s*0.23094"
  )
  stat <- "PearsonCumOR"
  expect_output(
    object = print(Exact_cond_midP_unspecific_ordering_rx2(t(n_short), dir, stat)),
    regexp = "Exact conditional test\\s*:\\s*P =\\s*0.08012"
  )
  expect_output(
    object = print(MantelHaenszel_test_2xc(lydersen_2012a)),
    regexp = "test of association: P = 0.1442, T = 2.132 \\(df=1\\)"
  )
  expect_output(
    object = print(Pearson_LR_tests_cum_OR_2xc(lydersen_2012a)),
    regexp = "Pearson chi-squared test: T =  3.813,  P = 0.07223"
  )
  alphahat0 <- c(-0.1923633, 0.5588396, 1.271953)
  expect_output(
    object = print(
        Score_test_for_effect_in_the_probit_model_2xc(
        lydersen_2012a, alphahat0
      )
    ),
    regexp = "Score test for effect: P = 0.1431, T = 2.145 \\(df=1\\)"
  )
  expect_output(
    object = print(the_2xc_table(n_short, direction = "decreasing")),
    regexp = "Wald \\(OR\\)       2.420     0.598 to  9.788"
  )
})
