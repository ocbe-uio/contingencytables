#' @title The Cochran-Armitage exact conditional and mid-P tests
#' @description The Cochran-Armitage exact conditional and mid-P tests
#' @description Described in Chapter 5 "The Ordered rx2 Table"
#' @param n the observed counts (an rx2 matrix)
#' @param a scores assigned to the rows
#' @param printresults display results
#' @examples
#' \dontrun{
#' # Alcohol consumption and malformations (Mills and Graubard, 1987)
#' n <- rbind(c(48, 17066), c(38, 14464), c(5, 788), c(1, 126), c(1, 37))
#' a <- c(1, 2, 3, 4, 5)
#' CochranArmitage_exact_cond_midP_tests_rx2(n, a)
#' }
#'
#' # Elevated troponin T levels in stroke patients (Indredavik et al., 2008)
#' n <- rbind(c(8, 53), c(10, 48), c(11, 100), c(22, 102), c(6, 129))
#' a <- c(1, 2, 3, 4, 5)
#' CochranArmitage_exact_cond_midP_tests_rx2(n, a)
#'
#' @export
#' @return A data frame containing the two-sided, twice-the-smallest tail P-value and the mid-P value
CochranArmitage_exact_cond_midP_tests_rx2 <- function(n, a, printresults = TRUE) {
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

  if (printresults) {
    print(sprintf("Cochran-Armitage exact cond. test: P = %7.5f", P))
    print(sprintf("Cochran-Armitage mid-P test:   mid-P = %7.5f", midP))
  }

  res <- data.frame(P = P, midP = midP)
  invisible(res)
}
