context("Optimization of Exact_cond_midP_linear_rank_tests_2xc()")

fill_nchoosek_old <- function(c0, npj) {
  npj_choose_x1j <- matrix(0, c0, max(npj) + 1)
    for (j in 1:c0) {
      for (x1j in 0:npj[j]) {
        npj_choose_x1j[j, x1j + 1] <- choose(npj[j], x1j)
      }
    }
  return(npj_choose_x1j)
}

test_that("New fill_nchoosek produces the same output as the old one", {
  for (iter in seq_len(10)) {
    c0 <- sample(1:5, 1L)
    npj <- rpois(c0, lambda = 10)
    expect_equal(fill_nchoosek(c0, npj), fill_nchoosek_old(c0, npj))
  }
})
