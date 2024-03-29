context("Chapter 2")

test_that("Chapter 2 functions basically work", {
  expect_error(Wald_CI_1x2(100))
  expect_output(
    object = print(Wald_CI_1x2(1, 2)),
    regexp = "estimate = 0.5000 \\(95% CI 0.0000 to 1.0000\\)"
  )
  expect_error(AgrestiCoull_CI_1x2(19))
  expect_output(
    object = print(AgrestiCoull_CI_1x2(19, 20, .15)),
    regexp = "estimate = 0.8750 \\(85% CI 0.7778 to 0.9722\\)"
  )
  expect_error(Arcsine_CI_1x2(500))
  expect_output(
    object = print(Arcsine_CI_1x2(100, 5e3, .1)),
    regexp = "estimate = 0.0200 \\(90% CI 0.0169 to 0.0235\\)"
  )
  expect_error(Blaker_exact_CI_1x2(1))
  expect_output(
    object = print(Blaker_exact_CI_1x2(1, 100)),
    regexp = "estimate = 0.0100 \\(95% CI 0.0005 to 0.0513\\)"
  )
  expect_error(Blaker_exact_test_1x2(1))
  expect_output(
    object = print(Blaker_exact_test_1x2(1, 10, .5)),
    regexp = "P = 0.02148"
  )
  expect_output(
    object = print(Blaker_exact_CI_1x2(0, 9)),
    regexp = "Blaker exact CI: estimate = 0.0000 \\(95% CI 0.0000 to 0.3161\\)"
  )
  expect_output(
    object = print(Blaker_exact_CI_1x2(9, 9)),
    regexp = "Blaker exact CI: estimate = 1.0000 \\(95% CI 0.6839 to 1.0000\\)"
  )
  expect_error(Blaker_midP_CI_1x2(100))
  expect_output(
    object = print(Blaker_midP_CI_1x2(100, 500, .5)),
    regexp = "estimate = 0.2000 \\(50% CI 0.1881 to 0.2121\\)"
  )
  expect_output(
    object = print(Blaker_midP_CI_1x2(1, 1)),
    regexp = "Blaker mid-P CI: estimate = 1.0000 \\(95% CI 0.1000 to 1.0000\\)"
  )
  expect_output(
    object = print(Blaker_midP_CI_1x2(0, 1)),
    regexp = "Blaker mid-P CI: estimate = 0.0000 \\(95% CI 0.0000 to 0.9000\\)"
  )
  expect_error(Blaker_midP_test_1x2(100))
  expect_output(
    object = print(Blaker_midP_test_1x2(
      ligarden_2010["X"], ligarden_2010["n"], pi0 = 0.5
    )),
    regexp = "P = 0.00845"
  )
  expect_error(ClopperPearson_exact_CI_1x2(100))
  expect_output(
    object = print(ClopperPearson_exact_CI_1x2(
      ligarden_2010["X"], ligarden_2010["n"]
    )),
    regexp = "estimate = 0.8125 \\(95% CI 0.5435 to 0.9595\\)"
  )
  expect_output(
    object = print(ClopperPearson_exact_CI_1x2(0, 1)),
    regexp = "exact CI: estimate = 0.0000 \\(95% CI 0.0000 to 0.9750\\)"
  )
  expect_output(
    object = print(ClopperPearson_exact_CI_1x2(1, 1)),
    regexp = "exact CI: estimate = 1.0000 \\(95% CI 0.0250 to 1.0000\\)"
  )
  expect_error(ClopperPearson_midP_CI_1x2(100))
  expect_output(
    object = print(ClopperPearson_midP_CI_1x2(ligarden_2010["X"], ligarden_2010["n"])),
    regexp = "estimate = 0.8125 \\(95% CI 0.5699 to 0.9500\\)"
  )
  expect_output(
    object = print(ClopperPearson_midP_CI_1x2(0, 9)),
    regexp = "Pearson mid-P CI: estimate = 0.0000 \\(95% CI 0.0000 to 0.2831\\)"
  )
  expect_output(
    object = print(ClopperPearson_midP_CI_1x2(9, 9)),
    regexp = "Pearson mid-P CI: estimate = 1.0000 \\(95% CI 0.7169 to 1.0000\\)"
  )
  expect_error(Exact_binomial_test_1x2(100))
  expect_output(
    object = print(
      Exact_binomial_test_1x2(
        ligarden_2010["X"], ligarden_2010["n"], pi0 = 0.5
      )
    ),
    regexp = "P = 0.02127"
  )
  expect_error(Jeffreys_CI_1x2(100))
  expect_output(
    object = print(Jeffreys_CI_1x2(ligarden_2010["X"], ligarden_2010["n"])),
    regexp = "estimate = 0.8125 \\(95% CI 0.5792 to 0.9442\\)"
  )
  expect_error(LR_CI_1x2(100))
  expect_output(
    object = print(LR_CI_1x2(ligarden_2010["X"], ligarden_2010["n"])),
    regexp = "estimate = 0.8125 \\(95% CI 0.5828 to 0.9497\\)"
  )
  expect_output(
    object = print(LR_CI_1x2(0, 10)),
    regexp = "estimate = 0.0000 \\(95% CI 0.0000 to 0.1748\\)"
  )
  expect_output(
    object = print(LR_CI_1x2(10, 10)),
    regexp = "estimate = 1.0000 \\(95% CI 0.8252 to 1.0000\\)"
  )
  expect_error(LR_test_1x2(100))
  expect_output(
    object = print(LR_test_1x2(ligarden_2010["X"], ligarden_2010["n"], pi0 = .5)),
    regexp = "P = 0.00944, T = 6.738 \\(df = 1\\)"
  )
    expect_output(
    object = print(LR_test_1x2(ligarden_2010["X"], ligarden_2010["X"], pi0 = .5)),
    regexp = "The likelihood ratio test: P = 1.00000, T = 0.000 \\(df = 1\\)"
  )
  expect_error(MidP_binomial_test_1x2(100))
  expect_output(
    object = print(MidP_binomial_test_1x2(
      ligarden_2010["X"], ligarden_2010["n"], pi0 = .5
    )),
    regexp = "P = 0.01273"
  )
  expect_error(Score_test_1x2(100))
  expect_output(
    object = print(Score_test_1x2(ligarden_2010["X"], ligarden_2010["n"], pi0 = .5)),
    regexp = "P = 0.01242, Z =  2.500"
  )
  expect_error(Score_test_CC_1x2(100))
  expect_output(
    object = print(
      Score_test_CC_1x2(
        ligarden_2010["X"], ligarden_2010["n"], pi0 = .5
      )
    ),
    regexp = "P = 0.02445, Z = 2.250"
  )
  expect_error(Wald_CI_CC_1x2(100))
  expect_output(
    object = print(Wald_CI_CC_1x2(ligarden_2010["X"], ligarden_2010["n"], alpha = .1)),
    regexp = "estimate = 0.8125 \\(90% CI 0.6207 to 1.0000\\)"
  )
  expect_error(Wilson_score_CI_1x2(100))
  expect_output(
    object = print(Wilson_score_CI_1x2(ligarden_2010["X"], ligarden_2010["n"])),
    regexp = "estimate = 0.8125 \\(95% CI 0.5699 to 0.9341\\)"
  )
  expect_error(Wilson_score_CI_CC_1x2(100))
  expect_output(
    object = print(Wilson_score_CI_CC_1x2(ligarden_2010["X"], ligarden_2010["n"])),
    regexp = "estimate = 0.8125 \\(95% CI 0.5369 to 0.9503\\)"
  )
  expect_error(the_1x2_table_CIs(100))
  expect_output(
    object = print(the_1x2_table_CIs(ligarden_2010["X"], ligarden_2010["n"])),
    regexp = "Estimate of pi: 13 / 16 = 0.812"
  )
  expect_error(Wald_test_1x2(100))
  expect_output(
    object = print(Wald_test_1x2(ligarden_2010["X"], ligarden_2010["n"], pi0 = 0.1)),
    regexp = "P = 0.00000, Z =  7.302"
  )
  expect_output(
    object = print(Wald_test_1x2(1, 1, pi0 = 0.1)),
    regexp = "The Wald test: P = 1.00000, Z =  0.000"
  )
  expect_error(Wald_test_CC_1x2(100))
  expect_output(
    object = print(
      Wald_test_CC_1x2(
        ligarden_2010["X"], ligarden_2010["n"], pi0 = 0.1
      )
    ),
    regexp = "P = 0.00000, Z = 6.982"
  )
  expect_output(
    object = print(Wald_test_CC_1x2(8, 8, 1)),
    regexp = "The Wald test with continuity correction: P = 1.00000, Z = 0.000"
  )
  expect_error(the_1x2_table_tests(100))
  expect_output(
    object = the_1x2_table_tests(
      ligarden_2010["X"], ligarden_2010["n"], pi0 = 0.5
    ),
    regexp = "H_0: pi = 0.500  vs  H_A: pi ~= 0.500"
  )
})
