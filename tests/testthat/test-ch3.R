context("Chapter 3")

test_that("Chapter 3 functions basically work", {
  expect_output(
    object = print(Chacko_test_1xc(n = c(1, 4, 3, 11, 9))),
    regexp = "P = 0.002168, T = 12.268 \\(df = 2\\)"
  )
  expect_output(
    object = print(Exact_multinomial_test_1xc(
      n = snp6498169$subset$n, pi0 = snp6498169$subset$pi0
    )),
    regexp = "P = 0.04792"
  )
  expect_error(Exact_multinomial_test_1xc(1:2, runif(2)), "X must be >= 3")
  expect_output(
    object = print(Gold_Wald_CIs_1xc(n = snp6498169$complete$n)),
    regexp = "pi_3: estimate = 0.1525 \\(0.1208 to 0.1841\\)"
  )
  expect_output(
    object = print(Goodman_Wald_CIs_1xc(n = snp6498169$complete$n)),
    regexp = "pi_3: estimate = 0.1525 \\(0.1215 to 0.1834\\)"
  )
  expect_output(
    object = print(Goodman_Wald_CIs_for_diffs_1xc(n = snp6498169$complete$n)),
    regexp = "pi_2 - pi_3: estimate = 0.3385 \\(0.2759 to 0.4011\\)"
  )
  expect_output(
    object = print(Goodman_Wald_CIs_for_diffs_1xc(n = snp6498169$complete$n, .1, "Scheffe")),
    regexp = "pi_2 - pi_3: estimate = 0.3385 \\(0.2824 to 0.3946\\)"
  )
  expect_output(
    object = print(Goodman_Wilson_score_CIs_1xc(n = snp6498169$complete$n)),
    regexp = "pi_3: estimate = 0.1525 \\(0.1241 to 0.1859\\)"
  )
  expect_output(
    object = print(LR_test_1xc(n = snp6498169$subset$n, pi0 = snp6498169$subset$pi0)),
    regexp = "P = 0.02704, T = 7.221 \\(df = 2\\)"
  )
  expect_output(
    object = print(
        MidP_multinomial_test_1xc(
        n = snp6498169$subset$n, pi0 = snp6498169$subset$pi0
      )
    ),
    regexp = "P = 0.04649"
  )
  expect_output(
    object = print(Pearson_chi_squared_test_1xc(
      n = snp6498169$complete$n, pi0 = snp6498169$complete$pi0
    )),
    regexp = "P = 0.00321, T = 11.481 \\(df = 2\\)"
  )
  expect_output(
    object = print(QuesenberryHurst_Wilson_score_CIs_1xc(n = snp6498169$complete$n)),
    regexp = "pi_2: estimate = 0.4910 \\(0.4472 to 0.5348\\)"
  )
  expect_output(
    object = print(the_1xc_table_CIs(n = snp6498169$complete$n)),
    regexp = "Gold Wald                       0.1208 to 0.1841    0.0633"
  )
  expect_output(
    object = print(the_1xc_table_tests(snp6498169$subset$n, snp6498169$subset$pi0)),
    regexp = "Pearson chi-squared    0.0346   \\(T = 6.727, df = 2\\)"
  )
  expect_output(
    object = print(
      the_1xc_table_tests(c(1, 4, 6, 3, 2), rep(1 / 5, 5), chacko.test = TRUE)
    ),
    regexp = "The Chacko test: P = 0.189546, T = 1.721 \\(df = 1\\)"
  )
})
