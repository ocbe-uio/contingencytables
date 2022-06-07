context("Chi-squared test with continuity correction")
x <- matrix(c(1, 2, 19, 33), ncol = 2)
n1 <- rbind(c(3, 1), c(1, 3))
n2 <- rbind(c(7, 27), c(1, 33))
n3 <- rbind(c(9, 4), c(4, 10))
n4 <- rbind(c(0, 16), c(15, 57))
test_that("Pearson_chi_squared_test_CC_2x2 equals chisq.test", {
  expect_equal(
    Pearson_chi_squared_test_CC_2x2(n1, printresults = FALSE)$p.value,
    suppressWarnings(chisq.test(n1, correct = TRUE)$p.value)
  )
  expect_equal(
    Pearson_chi_squared_test_CC_2x2(n2, printresults = FALSE)$p.value,
    suppressWarnings(chisq.test(n2, correct = TRUE)$p.value)
  )
  expect_equal(
    Pearson_chi_squared_test_CC_2x2(n3, printresults = FALSE)$p.value,
    suppressWarnings(chisq.test(n3, correct = TRUE)$p.value)
  )
  expect_equal(
    Pearson_chi_squared_test_CC_2x2(n4, printresults = FALSE)$p.value,
    suppressWarnings(chisq.test(n4, correct = TRUE)$p.value)
  )
  expect_equal(
    Pearson_chi_squared_test_CC_2x2(x, printresults = FALSE)$p.value,
    suppressWarnings(chisq.test(x, correct = TRUE)$p.value)
  )
})
