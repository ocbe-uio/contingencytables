context("Plotting")

test_that("Plotting fails when it should", {
  expect_error(
    plot(Arcsine_CI_1x2(13, 20)),
    "Plotting is only supported for the following functions"
  )
})

test_that("Plotting works when it should", {
  expect_invisible(plot(Exact_unconditional_test_2x2(tea)))
  expect_invisible(
    plot(McNemar_exact_unconditional_test_paired_2x2(bentur_2009))
  )
})
