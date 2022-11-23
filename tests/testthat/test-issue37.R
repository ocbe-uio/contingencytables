context("Issue 37")

n0 <- n1 <- 400
e0 <- e1 <- 25
t2x2 <- matrix(c(e0, n0 - e0, e1, n1 - e1), nrow = 2, byrow = TRUE)

test_that("Output is as expected", {
  expect_output(
    BaptistaPike_midP_CI_2x2(t2x2, alpha = 0.05, printresults = TRUE),
    "Baptista-Pike mid-P CI: estimate = 1.0000 \\(95% CI 0.5660 to 1.7667\\)"
  )
})
