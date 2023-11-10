context("Chapter 8")

test_that("Chapter 8 functions basically work", {
  n_short <- floor(cavo_2012 / 10)
  expect_output(
    object = print(BonettPrice_hybrid_Wilson_score_CI_CC_paired_2x2(cavo_2012)),
    regexp = "CI w / CC: estimate = 0.8667 \\(95% CI 0.7475 to 1.0058\\)"
  )
  expect_output(
    object = print(BonettPrice_hybrid_Wilson_score_CI_CC_paired_2x2(matrix(c(0, 1, 0, 2), 2))),
    regexp = "CI w / CC: estimate = 0.0000 \\(95% CI 0.0000 to 17.3081\\)"
  )
  expect_output(
    object = print(BonettPrice_hybrid_Wilson_score_CI_CC_paired_2x2(matrix(c(0, 0, 1, 2), 2))),
    regexp = "CI w / CC: estimate =    Inf \\(95% CI 0.0578 to    Inf\\)"
  )
  expect_output(
    object = print(BonettPrice_hybrid_Wilson_score_CI_CC_paired_2x2(matrix(c(9, 7, 0, 2), 2))),
    regexp = "CI w / CC: estimate = 0.5625 \\(95% CI 0.3330 to 0.9625\\)"
  )
  expect_output(
    object = print(BonettPrice_hybrid_Wilson_score_CI_CC_paired_2x2(matrix(c(9, 0, 8, 2), 2))),
    regexp = "CI w / CC: estimate = 1.8889 \\(95% CI 1.0926 to 3.2321\\)"
  )
  expect_output(
    object = print(BonettPrice_hybrid_Wilson_score_CI_paired_2x2(cavo_2012)),
    regexp = "estimate = 0.8667 \\(95% CI 0.7579 to 0.9910\\)"
  )
  expect_output(
    object = print(BonettPrice_hybrid_Wilson_score_CI_CC_paired_2x2(matrix(c(0, 0, 0, 2), 2))),
    regexp = "CI w / CC: estimate =    NaN \\(95% CI 0.0000 to    Inf\\)"
  )
  expect_output(
    object = print(ClopperPearson_exact_CI_1x2_beta_version(
      ligarden_2010["X"], ligarden_2010["n"]
    )),
    regexp = "exact CI: estimate = 0.8125 \\(95% CI 0.5435 to 0.9595\\)"
  )
  expect_output(
    object = print(ClopperPearson_exact_CI_1x2_beta_version(10, 10)),
    regexp = "Pearson exact CI: estimate = 1.0000 \\(95% CI 0.6915 to 1.0000\\)"
  )
  expect_output(
    object = print(ClopperPearson_exact_CI_1x2_beta_version(0, 10)),
    regexp = "Pearson exact CI: estimate = 0.0000 \\(95% CI 0.0000 to 0.3085\\)"
  )
  expect_output(
    object = print(McNemar_asymptotic_test_CC_paired_2x2(cavo_2012)),
    regexp = "McNemar test with continuity correction: P = 0.055009, Z =  1.919"
  )
  expect_output(
    object = print(McNemar_asymptotic_test_CC_paired_2x2(matrix(c(10, 0, 0, 1), 2))),
    regexp = "No discordant pairs"
  )
  expect_output(
    object = print(McNemar_asymptotic_test_paired_2x2(cavo_2012)),
    regexp = "The McNemar asymptotic test: P = 0.033006, Z = -2.132"
  )
  expect_output(
    object = print(McNemar_asymptotic_test_paired_2x2(matrix(c(10, 0, 0, 1), 2))),
    regexp = "No discordant pairs"
  )
  expect_output(
    object = print(McNemar_exact_cond_test_paired_2x2(cavo_2012)),
    regexp = "The McNemar exact conditional test: P = 0.052479"
  )
  expect_output(
    object = print(McNemar_exact_unconditional_test_paired_2x2(n_short)),
    regexp = "The McNemar exact unconditional test: P = 0.542071"
  )
  expect_output(
    object = print(McNemar_midP_test_paired_2x2(cavo_2012)),
    regexp = "The McNemar mid-P test: P = 0.034690"
  )
  expect_output(
    object = print(Tang_asymptotic_score_CI_paired_2x2(cavo_2012)),
    regexp = "estimate = 0.8667 \\(95% CI 0.7476 to 0.9876\\)"
  )
  expect_output(
    object = print(Tango_asymptotic_score_CI_paired_2x2(cavo_2012)),
    regexp = "score CI: estimate = -0.0621 \\(95% CI -0.1240 to -0.0054\\)"
  )
  expect_output(
    object = print(Wald_CI_diff_paired_2x2(cavo_2012)),
    regexp = "The Wald CI: estimate = -0.0621 \\(95% CI -0.1184 to -0.0058\\)"
  )
  expect_output(
    object = print(Wald_CI_diff_CC_paired_2x2(cavo_2012)),
    regexp = "estimate = -0.0621 \\(95% CI -0.1186 to -0.0057\\)"
  )
  expect_output(
    object = print(Wald_CI_AgrestiMin_paired_2x2(cavo_2012)),
    regexp = "adjustment: estimate = -0.0621 \\(95% CI -0.1182 to -0.0045\\)"
  )
  expect_output(
    object = print(MOVER_Wilson_score_CI_paired_2x2(cavo_2012)),
    regexp = "estimate = 0.8667 \\(95% CI 0.7592 to 0.9866\\)"
  )
  expect_output(
    object = print(Newcombe_square_and_add_CI_paired_2x2(cavo_2012)),
    regexp = "estimate = -0.0621 \\(95% CI -0.1186 to -0.0046\\)"
  )
  expect_output(
    object = print(Transformed_Wilson_score_CI_paired_2x2(cavo_2012)),
    regexp = "0.3750 \\(95% CI 0.1514 to 0.9287\\)"
  )
  expect_output(
    object = print(Wald_CI_BonettPrice_paired_2x2(cavo_2012)),
    regexp = "adjustment: estimate = -0.0621 \\(95% CI -0.1195 to -0.0032\\)"
  )
  expect_output(
    object = print(the_paired_2x2_table_CIs_difference(cavo_2012)),
    regexp = "Estimate of delta = pi_1\\+ - pi_\\+1: -0.062"
  )
  expect_output(
    object = print(the_paired_2x2_table_CIs_OR(ezra_2010)),
    regexp = "Estimate of theta_cond = n_12/n_21: 12.500"
  )
  expect_output(
    object = print(Wald_CI_OR_paired_2x2(ezra_2010)),
    regexp = "The Wald CI: estimate = 12.5000 \\(95% CI 2.9608 to 52.7731\\)"
  )
  expect_output(
    object = print(Wald_CI_OR_paired_2x2(matrix(c(1, 0, 3, 2), 2))),
    regexp = "The Wald CI: estimate =    Inf \\(95% CI 0.0000 to    Inf\\)"
  )
  expect_output(
    object = print(Wald_CI_OR_Laplace_paired_2x2(ezra_2010)),
    regexp = "adjustment: estimate = 12.5000 \\(95% CI 2.6232 to 28.6333\\)"
  )
  expect_output(
    object = print(Transformed_Clopper_Pearson_exact_CI_paired_2x2(ezra_2010)),
    regexp = "estimate = 12.5000 \\(95% CI 3.1169 to 108.8892\\)"
  )
  expect_output(
    object = print(Transformed_Clopper_Pearson_midP_CI_paired_2x2(ezra_2010)),
    regexp = "estimate = 12.5000 \\(95% CI 3.4681 to 78.2972\\)"
  )
  expect_output(
    object = print(Transformed_Blaker_exact_CI_paired_2x2(ezra_2010)),
    regexp = "estimate = 12.5000 \\(95% CI 3.2994 to 74.0582\\)"
  )
  expect_output(
    object = print(Wald_CI_ratio_paired_2x2(cavo_2012)),
    regexp = "The Wald CI: estimate = 0.8667 \\(95% CI 0.7597 to 0.9886\\)"
  )
  expect_output(
    object = print(Wald_CI_ratio_paired_2x2(matrix(c(0, 0, 10, 20), 2))),
    regexp = "The Wald CI: estimate =    Inf \\(95% CI 0.0000 to    Inf\\)"
  )
  expect_output(
    object = print(the_paired_2x2_table_CIs_ratio(cavo_2012)),
    regexp = "Estimate of phi = pi_1\\+/pi_\\+1: 0.867"
  )
  expect_output(
    object = print(the_paired_2x2_table_tests(n_short)),
    regexp = "Estimate of pi_\\+1: 6/14 = 0.429"
  )
})
