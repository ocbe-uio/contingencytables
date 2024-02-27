context("Issue 36")

fagerland <- c(1, 4, 3, 11, 9) # Fagerland et al. (2017)
chacko_1 <- c(10, 16, 14, 12, 18) # Chacko (1966), section 3
chacko_2 <- c(12, 14, 18, 16, 22, 20, 18, 24, 26, 30) # Chacko (1966), section 5
ruxton <- c(6, 8, 4, 7, 3) # Ruxton

test_that("Expected output is obtained", {
  f <- Chacko_test_1xc(fagerland)
  expect_equal(f$Pvalue, 0.002, tol = 1e-2)
  expect_equal(f$T, 12.27, tol = 1e-2)
  expect_equal(f$df, 2)

  c1 <- Chacko_test_1xc(chacko_1)
  expect_equal(c1$df, 2)

  c2 <- Chacko_test_1xc(chacko_2)
  expect_equal(c2$T, 13.5)
  expect_equal(c2$df, 6)

  expect_warning(Chacko_test_1xc(ruxton))
})
