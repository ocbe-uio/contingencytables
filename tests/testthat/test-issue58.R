context("Issue 58")

test_that("Miettinen-Nurminen asymptotic score CIs match issue 58", {
  tab2x2 <- matrix(c(2, 1, 8, 9), nrow = 2)
  the_2x2_table_CIs_difference(tab2x2)
  difference <- MiettinenNurminen_asymptotic_score_CI_difference_2x2(tab2x2)
  expect_equal(
    c(difference$lower, difference$upper, difference$estimate),
    c(-0.2595, 0.4454, 0.1),
    tolerance = 5e-5
  )

  ratio <- MiettinenNurminen_asymptotic_score_CI_ratio_2x2(tab2x2)
  expect_equal(
    c(ratio$lower, ratio$upper, ratio$estimate),
    c(0.2888, 14.6551, 2),
    tolerance = 5e-5
  )

  odds_ratio <- MiettinenNurminen_asymptotic_score_CI_OR_2x2(tab2x2)
  expect_equal(
    c(odds_ratio$lower, odds_ratio$upper, odds_ratio$estimate),
    c(0.2248, 21.0309, 2.25),
    tolerance = 5e-5
  )
})
