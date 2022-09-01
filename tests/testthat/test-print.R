context("print() method")
test_that("Printing method works as expected", {
  expect_silent(Adjusted_inv_sinh_CI_OR_2x2(lampasona_2013))
  expect_invisible(tmp <- Adjusted_inv_sinh_CI_OR_2x2(lampasona_2013))
  expect_silent(tmp)
  expect_output(print(tmp), "The adjusted inverse sinh CI")
})
