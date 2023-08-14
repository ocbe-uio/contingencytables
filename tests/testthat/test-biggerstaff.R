context("Fixing multiple_hypergeometric_pdf")

test_that("Brad's version of multiple_hypergeometric_pdf() solves NA issues", {
  mat <- matrix(c(196, 398, 287, 3, 10, 14), 3)
  set.seed(6473562)
  met <- matrix(rpois(6, lambda = 200), 2)
  mit <- matrix(rpois(8, lambda = 80), 2)
  mot <- matrix(rpois(15, lambda = 80), 3)
  mut <- matrix(rpois(20, lambda = 50), 5)
  expect_output(
    print(Exact_cond_midP_tests_rxc(mat)),
    "Exact Fisher-Freeman-Halton: P = 0.1100976"
  )
  expect_output(
    print(FisherFreemanHalton_asymptotic_test_rxc(mat)),
    "Fisher-Freeman-Halton asymptotic test: P = 0.1117, T = 4.383"
  )
  expect_output(
    print(FisherFreemanHalton_asymptotic_test_rxc(met)),
    "Fisher-Freeman-Halton asymptotic test: P = 0.9317, T = 0.142 \\(df=2\\)"
  )
  expect_output(
    print(FisherFreemanHalton_asymptotic_test_rxc(mit)),
    "Fisher-Freeman-Halton asymptotic test: P = 0.0047, T = 12.991 \\(df=3\\)"
  )
  expect_output(
    print(FisherFreemanHalton_asymptotic_test_rxc(mot)),
    "Fisher-Freeman-Halton asymptotic test: P = 0.9920, T = 1.544 \\(df=8\\)"
  )
  expect_output(
    print(FisherFreemanHalton_asymptotic_test_rxc(mut)),
    "Fisher-Freeman-Halton asymptotic test: P = 0.7912, T = 7.922 \\(df=12\\)"
  )
})
