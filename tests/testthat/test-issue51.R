context("Issue 51")

test_that("Fisher_midP_test_2x2() examples produce expected p-values", {
  tol <- 1e-5
  expect_equal(Fisher_midP_test_2x2(tea)$P, 0.25714, tolerance = tol)
  expect_equal(Fisher_midP_test_2x2(perondi_2004)$P, 0.02966, tolerance = tol)
  expect_equal(Fisher_midP_test_2x2(lampasona_2013)$P, 0.03914, tolerance = tol)
  expect_equal(Fisher_midP_test_2x2(ritland_2007)$P, 0.04466, tolerance = tol)
  x <- matrix(c(745, 127, 3, 2), ncol = 2)
  expect_equal(Fisher_midP_test_2x2(x)$P, 0.09193139, tolerance = tol)
})
