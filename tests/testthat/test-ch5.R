context("Chapter 5")

test_that("Chapter 5 functions basically work", {
  a <- c(1, 2, 3, 4, 5)
  expect_output(
    object = print(CochranArmitage_CI_rx2(indredavik_2008, a)),
    regexp = "betahat = -0.0194 \\(95% CI -0.0395 to 0.0007\\)"
  )
  expect_output(
    object = print(CochranArmitage_MH_tests_rx2(indredavik_2008, a)),
    regexp = "Mantel-Haenszel test\\s*:\\s*T = -1.790, P = 0.07351"
  )
  a <- 1:4
  expect_output(
    object = print(
      CochranArmitage_exact_cond_midP_tests_rx2(indredavik_2008[-5, ], a)
    ),
    regexp = "Cochran-Armitage exact cond. test: P = 0.62494"
  )
  expect_output(
    object = print(
      CochranArmitage_exact_cond_midP_tests_rx2(
        matrix(c(1, 2, 3, 0, 0, 0, 0, 0, 5, 1), 5), 1:5
      )
    ),
    regexp = "Cochran-Armitage exact cond. test: P = 0.00216"
  )
  a5 <- 1:5
  expect_output(
    object = print(
      CochranArmitage_exact_cond_midP_tests_rx2(floor(indredavik_2008 / 10), a5)
    ),
    regexp = "Cochran-Armitage mid-P test:   mid-P = 0.71754"
  )
  m <- matrix(c(48, 170, 38, 144, 5, 7, 1, 1), byrow = TRUE, ncol = 2)
  d <- "decreasing"
  expect_output(
    object = print(Exact_cond_midP_unspecific_ordering_rx2(ceiling(m / 10), d)),
    regexp = "Mid-P test\\s*:\\s*midP =\\s*0.43131"
  )
  expect_output(
    object = print(
      Exact_cond_midP_unspecific_ordering_rx2(
        matrix(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 0), 5),
        "decreasing"
      )
    ),
    regexp = "Mid-P test\\s*:\\s*midP =\\s*0.18978"
  )
  expect_output(
    object = print(Pearson_LR_tests_unspecific_ordering_rx2(indredavik_2008, d)),
    regexp = "Likelihood ratio test:    T = 11.192, P = 0.00252"
  )
  expect_output(
    object = print(Trend_estimate_CI_tests_rx2(indredavik_2008, a5)),
    regexp = "betahat = -0.1828 \\(95% CI -0.3844 to 0.0188\\)"
  )
  m2 <- rbind(c(48, 1706), c(38, 1446), c(5, 78), c(1, 12))
  expect_output(
    object = print(the_rx2_table(m2, direction = d, skip_exact = TRUE)),
    regexp = "Cochran-Armitage             0.817           0.41391"
  )
  expect_output(
    object = print(the_rx2_table(ceiling(m2 / 10), direction = d)),
    regexp = "Mid-P \\(LR\\)                                   0.07092"
  )
})
