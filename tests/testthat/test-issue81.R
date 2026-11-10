context("Issue 81")

test_that("exact unconditional McNemar test handles large tables", {
  tab <- matrix(c(155, 5, 7, 14), 2)
  result <- McNemar_exact_unconditional_test_paired_2x2(tab)
  expect_equal(result$Pvalue, 0.6823931, tolerance = 1e-6)
  expect_true(all(is.finite(result$Pvalues)))
  expect_equal(result$Pvalues, McNemar_exact_unconditional_test_paired_2x2(t(tab))$Pvalues)

  pi <- result$pi_values[which.max(result$Pvalues)]
  direct <- 0
  for (x12 in 0:181) {
    for (x21 in 0:(181 - x12)) {
      if (x12 + x21 > 0 && (x12 - x21)^2 / (x12 + x21) >= 1 / 3) {
        direct <- direct + dmultinom(c(x12, x21, 181 - x12 - x21), prob = c(pi / 2, pi / 2, 1 - pi))
      }
    }
  }
  expect_equal(result$Pvalue, min(direct + 0.0001, 1))

  unbalanced <- matrix(c(150, 0, 20, 11), 2)
  result <- McNemar_exact_unconditional_test_paired_2x2(unbalanced, gamma = 0, num_pi_values = 11)
  expect_true(all(is.finite(result$Pvalues)))
  expect_equal(result$Pvalues[1], 0)
  expect_equal(result$Pvalues[11], 2 * pbinom(60, 181, 0.5))
  expect_lt(result$Pvalue, 1)
})

test_that("exact unconditional McNemar probabilities match trinomial enumeration", {
  tab <- matrix(c(3, 1, 3, 1), 2)
  result <- McNemar_exact_unconditional_test_paired_2x2(tab, gamma = 0, num_pi_values = 5)
  N <- sum(tab)
  observed <- (tab[1, 2] - tab[2, 1])^2 / (tab[1, 2] + tab[2, 1])
  expected <- vapply(result$pi_values, function(pi) {
    probability <- 0
    for (x12 in 0:N) {
      for (x21 in 0:(N - x12)) {
        if (x12 + x21 > 0 && (x12 - x21)^2 / (x12 + x21) >= observed) {
          probability <- probability + dmultinom(c(x12, x21, N - x12 - x21), prob = c(pi / 2, pi / 2, 1 - pi))
        }
      }
    }
    probability
  }, numeric(1))
  expect_equal(result$Pvalues, expected)
  expect_equal(result$Pvalue, max(expected))
})

test_that("large tables produce finite probabilities", {
  tab <- matrix(c(674, 5, 7, 14), 2)
  result <- McNemar_exact_unconditional_test_paired_2x2(
    tab, gamma = 0, num_pi_values = 3
  )

  expect_equal(
    result$Pvalues,
    c(0, 0.569552958899, 2 * pbinom(342, 700, 0.5)),
    tolerance = 1e-8
  )
})
