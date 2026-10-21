context("Issue 62")

test_that("A table and its row swap get the same mid-P value", {
  x <- matrix(c(2, 36, 8, 30), 2, byrow = TRUE)
  # Probabilities of x11 = 0, ..., 10 under the margins of x: the observed
  # x11 = 2 is tied with x11 = 8, and x11 = 0, 1, 9, 10 are less probable
  f <- dhyper(0:10, 10, 66, 38)
  midp <- sum(f[c(1, 2, 10, 11)]) + 0.5 * (f[3] + f[9]) # 0.04998656
  expect_equal(Fisher_midP_test_2x2(x)$Pvalue, midp)
  expect_equal(Fisher_midP_test_2x2(x[2:1, ])$Pvalue, midp)
})

test_that("A table with a zero row and its row swap give the same p-values", {
  x <- matrix(c(0, 0, 1, 1), 2, byrow = TRUE)
  x_swapped <- x[2:1, ]
  for (statistic in c("hypergeometric", "LR", "Pearson")) {
    for (table in list(x, x_swapped)) {
      expect_equal(Fisher_exact_test_2x2(table, statistic)$Pvalue, 1)
      expect_equal(Fisher_midP_test_2x2(table, statistic)$Pvalue, 0.5)
    }
  }
})

test_that("Fisher_exact_test_2x2() agrees with fisher.test() on tied tables", {
  y <- matrix(c(8, 2, 30, 36), 2, byrow = TRUE)
  expect_equal(
    Fisher_exact_test_2x2(y, "hypergeometric")$Pvalue,
    fisher.test(y)$p.value # 0.08600352
  )
  z <- matrix(c(2, 0, 1, 1), 2, byrow = TRUE)
  expect_equal(
    Fisher_exact_test_2x2(z, "hypergeometric")$Pvalue,
    fisher.test(z)$p.value
  )
})

test_that("Ties are found without symmetric margins", {
  z <- matrix(c(2, 1, 12, 3), 2, byrow = TRUE)
  # The observed table (x11 = 2) and x11 = 3 are the two modes
  f <- dhyper(0:3, 14, 4, 3)
  midp <- sum(f[1:2]) + 0.5 * sum(f[3:4]) # 0.55392
  expect_equal(Fisher_midP_test_2x2(z)$Pvalue, midp)
})

test_that("P-values agree with the definition and with fisher.test()", {
  # Mid-P value as defined in Section 4.4.6, from the table weights
  # choose(n1+, x11) * choose(n2+, n+1 - x11): integers, exact for these tables
  midp_definition <- function(n) {
    n1p <- sum(n[1, ])
    n2p <- sum(n[2, ])
    np1 <- sum(n[, 1])
    x11 <- max(0, np1 - n2p):min(n1p, np1)
    w <- choose(n1p, x11) * choose(n2p, np1 - x11)
    wobs <- choose(n1p, n[1, 1]) * choose(n2p, n[2, 1])
    (sum(w[w < wobs]) + 0.5 * sum(w[w == wobs])) / sum(w)
  }
  tables <- list(
    # Equal row totals
    matrix(c(0, 3, 2, 1), 2, byrow = TRUE),
    matrix(c(3, 3, 1, 5), 2, byrow = TRUE),
    matrix(c(1, 14, 6, 9), 2, byrow = TRUE),
    # Equal column totals
    matrix(c(0, 2, 3, 1), 2, byrow = TRUE),
    matrix(c(1, 5, 6, 2), 2, byrow = TRUE),
    matrix(c(0, 4, 15, 11), 2, byrow = TRUE)
  )
  for (n in tables) {
    expect_equal(Fisher_midP_test_2x2(n)$Pvalue, midp_definition(n))
    expect_equal(
      Fisher_exact_test_2x2(n, "hypergeometric")$Pvalue, fisher.test(n)$p.value
    )
  }
})
