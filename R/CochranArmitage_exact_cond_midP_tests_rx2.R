#' @title The Cochran-Armitage exact conditional and mid-P tests
#' @description The Cochran-Armitage exact conditional and mid-P tests
#' @description Described in Chapter 5 "The Ordered rx2 Table"
#' @param n the observed counts (an rx2 matrix)
#' @param a scores assigned to the rows
#' @examples
#' \dontrun{
#' CochranArmitage_exact_cond_midP_tests_rx2(mills_graubard_1987, c(1, 2, 3, 4, 5))
#' }
#' CochranArmitage_exact_cond_midP_tests_rx2(indredavik_2008, c(1, 2, 3, 4, 5))
#' @export
#' @return An object of the [contingencytables_result] class,
#' basically a subclass of [base::list()]. Use the [utils::str()] function
#' to see the specific elements returned.
CochranArmitage_exact_cond_midP_tests_rx2 <- function(n, a) {
  validateArguments(mget(ls()))
  r <- nrow(n)
  nip <- apply(n, 1, sum)
  N <- sum(n)
  np1 <- sum(n[, 1])

  # Calculate all nchoosek beforehand
  nip_choose_xi1 <- matrix(0, r, max(nip) + 1)
  for (i in 1:r) {
    for (xi1 in 0:nip[i]) {
      nip_choose_xi1[i, xi1 + 1] <- choose(nip[i], xi1)
    }
  }
  N_choose_np1 <- choose(N, np1)

  # The observed value of the test statistic
  Tobs <- linear_rank_test_statistic(n[, 1], a)

  # Calculate the smallest one-sided P-value and the point probability
  # Need separate functions for different values of r (the number of rows)
  if (r == 4) {
    res <- calc_Pvalue_4x2(Tobs, nip, np1, N_choose_np1, nip_choose_xi1, a)
    one_sided_P <- res$one_sided_P
    point_prob <- res$point_prob
  } else if (r == 5) {
    res <- calc_Pvalue_5x2(Tobs, nip, np1, N_choose_np1, nip_choose_xi1, a)
    one_sided_P <- res$one_sided_P
    point_prob <- res$point_prob
  }

  # Two-sided twice-the-smallest tail P-value and mid-P value
  P <- 2 * one_sided_P
  midP <- 2 * (one_sided_P - 0.5 * point_prob)

  # Output
  printresults <- function() {
    my_sprintf_cat("Cochran-Armitage exact cond. test: P = %7.5f", P)
    my_sprintf_cat("Cochran-Armitage mid-P test:   mid-P = %7.5f", midP)
  }
  return(contingencytables_result(list(P = P, midP = midP), printresults))
}
