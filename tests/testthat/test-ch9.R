context("Chapter 9")

test_that("Chapter 9 functions basically work", {
  expect_output(
    object = print(Bhapkar_test_paired_cxc(peterson_2007)),
    regexp = "marginal homogenity: P = 0.000005, T = 27.304 \\(df = 3\\)"
  )
  expect_output(
    object = print(Bhapkar_test_paired_cxc(matrix(c(1, 2, 3, NA), 2))),
    "The Bhapkar test statistic is not computable"
  )
  expect_output(
    object = print(Bonferroni_type_CIs_paired_cxc(peterson_2007)),
    regexp = "pi_4\\+ vs pi_\\+4: delta =  0.0088 \\(-0.0059 to  0.0233\\)"
  )
  expect_output(
    object = print(Bonferroni_type_CIs_paired_cxc(matrix(c(1, 0, 0, 1), 2))),
    regexp = "pi_2\\+ vs pi_\\+2: delta =  0.0000 \\(-1.0000 to  1.0000\\)"
  )
  expect_output(
    object = print(FleissEveritt_test_paired_cxc(fleiss_2003)),
    regexp = "version of the Stuart test: P = 0.000912, T = 14.000 \\(df=2\\)"
  )
  expect_error(
    object = FleissEveritt_test_paired_cxc(matrix(1:4, 2)),
    regexp = "This method can only be used for c=3 categories"
  )
  expect_error(
    object = FleissEveritt_test_paired_cxc(matrix(rep(1, 9), 3)),
    regexp = "No differences between the marginal sums"
  )
  expect_output(
    object = print(FleissLevinPaik_test_paired_cxc(peterson_2007[-4, -4])),
    regexp = "The Fleiss-Levin-Paik test: P = 0.000004, T = 21.429 \\(df=1\\)"
  )
  expect_error(
    object = FleissLevinPaik_test_paired_cxc(matrix(1:4, 2)),
    regexp = "This method can only be used for c=3 categories"
  )
  expect_error(
    object = FleissLevinPaik_test_paired_cxc(matrix(rep(1, 9), 3)),
    regexp = "No differences between the marginal sums"
  )
  expect_output(
    object = print(McNemarBowker_test_paired_cxc(peterson_2007)),
    regexp = "test for symmetry: P = 0.000200, T0 = 26.250 \\(df=6\\)"
  )
  expect_output(
    object = print(Scheffe_type_CIs_paired_cxc(peterson_2007)),
    regexp = "pi_4\\+ vs pi_\\+4: delta =  0.0088 \\(-0.0076 to  0.0250\\)"
  )
  expect_output(
    object = print(Scheffe_type_CIs_paired_cxc(matrix(c(1, 0, 0, 1), 2))),
    regexp = "  pi_1\\+ vs pi_\\+1: delta =  0.0000 \\(-1.0000 to  1.0000\\)"
  )
  expect_output(
    object = print(Scheffe_type_CIs_paired_cxc(matrix(c(1, 0, 0, 1), 2))),
    regexp = "  pi_1\\+ vs pi_\\+1: delta =  0.0000 \\(-1.0000 to  1.0000\\)"
  )
  a <- c(8, 3.5, 0, -3.5, -8)
  expect_output(
    object = print(Score_test_and_CI_marginal_mean_scores_paired_cxc(fischer_1999, a)),
    regexp = "The score CI: estimate = 1.6942 \\(95% CI 0.8769 to 2.5115\\)"
  )
  expect_output(
    object = print(Stuart_test_paired_cxc(peterson_2007)),
    regexp = "marginal homogenity: P = 0.000008, T0 = 26.250 \\(df=3\\)"
  )
  expect_output(
    object = print(Stuart_test_paired_cxc(matrix(c(1, 0, 0, 1), 2))),
    regexp = "No differences between the marginal sums"
  )
  expect_output(
    object = print(Stuart_test_paired_cxc(matrix(c(NA, 2, 3, 4), 2))),
    regexp = "The Stuart test statistic is not computable"
  )
  expect_output(
    object = print(Wald_test_and_CI_marginal_mean_ranks_paired_cxc(fischer_1999)),
    regexp = "0.6196 \\(95% CI 0.5591 to 0.6800\\); P = 0.00011, Z =  3.877"
  )
  expect_output(
    object = print(Wald_test_and_CI_marginal_mean_scores_paired_cxc(fischer_1999, a)),
    regexp = "The Wald CI: estimate = 1.6942 \\(95% CI 0.9347 to 2.4537\\)"
  )
  expect_output(
    object = print(the_paired_cxc_table_nominal(peterson_2007)),
    regexp = "pi_3\\+ - pi_\\+3:  -0.0088  \\(-0.0187 to  0.0012\\)"
  )
  expect_output(
    object = print(the_paired_cxc_table_nominal(matrix(1:9, 3))),
    regexp = "Fleiss-Everitt version of the Stuart test\\s+2.535 \\(df=2\\)\\s+0.281505"
  )
  expect_output(
    object = print(the_paired_cxc_table_ordinal(fischer_1999, a)),
    regexp = "Wald logit: estimate = 0.2391 \\(95% CI 0.1151 to 0.3558\\)"
  )
  expect_output(
    object = print(the_paired_cxc_table_ordinal(table_7.6)),
    regexp = "Fleiss-Levin-Paik test              30.387 \\(df=1\\)   0.000000"
  )

})
