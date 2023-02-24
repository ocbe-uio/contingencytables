context("Class instantiation")

X <- list(
  Adjusted_inv_sinh_CI_OR_2x2(lampasona_2013),
  BaptistaPike_exact_conditional_CI_2x2(tea),
  CochranArmitage_MH_tests_rx2(indredavik_2008, c(1, 2, 3, 4, 5)),
  with(snp6498169$subset, Exact_multinomial_test_1xc(n, pi0))
)

test_that("Objects have correct class elements", {
  for (x in X) {
    expect_named(attributes(x), c("names", "class", "print_structure"))
    expect_s3_class(x, "contingencytables_result")
  }
})
