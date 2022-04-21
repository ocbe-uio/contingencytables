#' @title The Pearson chi-squared and likelihood ratio tests for unspecific ordering in rx2 tables
#' @description The Pearson chi-squared and likelihood ratio tests for unspecific ordering in rx2 tables.
#' Described in Chapter 5 "The Ordered rx2 Table".
#' May also be used for 2xc tables, after flipping rows and columns (i.e. if
#' n is a 2xc table, call this function with n' (the transpose of n) as the
#' first argument).
#' @param n the observed counts (an rx2 matrix)
#' @param direction the direction of the success probabilities ("increasing" or "decreasing")
#' @param printresults display results
#' @examples
#' # Chapter 5: Alcohol consumption and malformations (Mills and Graubard, 1987)
#' n <- matrix(
#'   c(48, 17066, 38, 14464, 5, 788, 1, 126, 1, 37),
#'   byrow = TRUE, ncol = 2
#' )
#' Pearson_LR_tests_unspecific_ordering_rx2(n, "increasing")
#'
#' # Chapter 5: Elevated troponin T levels in stroke patients (Indredavik et al., 2008)
#' n <- matrix(c(8, 53, 10, 48, 11, 100, 22, 102, 6, 129), byrow = TRUE, ncol = 2)
#' Pearson_LR_tests_unspecific_ordering_rx2(n, "decreasing")
#'
#' # Chapter 6: Postoperative nausea (Lydersen et al., 2012a)
#' n <- t(matrix(c(14, 10, 3, 2, 11, 7, 8, 4), byrow = TRUE, ncol = 4))
#' Pearson_LR_tests_unspecific_ordering_rx2(n, "decreasing")
#' @export
#' @return A list containing the two-sided p-value and the test statistic for the likelihood ratio and the Pearson chi-squared tests
Pearson_LR_tests_unspecific_ordering_rx2 <- function(n, direction, printresults = TRUE) {
  r <- nrow(n)
  nip <- apply(n, 1, sum)
  npj <- apply(n, 2, sum)
  N <- sum(n)
  nhat <- n[, 1] / apply(n, 1, sum)

  # Calculate the probabilities for the chi-bar-squared distribution
  rho <- calc_rho(r)

  # Pool out-of-order proportions and pooled estimates
  nhatstar <- nhat
  for (i in 1:(r - 1)) {
    if ((direction == "increasing" && nhatstar[i] > nhatstar[i + 1]) ||
      (direction == "decreasing" && nhatstar[i] < nhatstar[i + 1])) {
      pooled.proportion <- (n[i, 1] + n[i + 1, 1]) / (n[i, 1] + n[i, 2] + n[i + 1, 1] + n[i + 1, 2])
      nhatstar[i] <- pooled.proportion
      nhatstar[i + 1] <- pooled.proportion
    }
  }
  nstar <- matrix(0, r, 2)
  nstar[, 1] <- apply(n, 1, sum) * nhatstar
  nstar[, 2] <- apply(n, 1, sum) * (1 - nhatstar)

  # The Pearson chi-squared and likelihood ratio statistics
  m <- matrix(0, r, 2)
  T_Pearson <- 0
  T_LR <- 0
  for (i in 1:r) {
    for (j in 1:2) {
      m[i, j] <- nip[i] * npj[j] / N
      T_Pearson <- T_Pearson + ((nstar[i, j] - m[i, j])^2) / m[i, j]
      T_LR <- T_LR + nstar[i, j] * log(nstar[i, j] / m[i, j])
    }
  }
  T_LR <- 2 * T_LR

  # The two-sided P-values (reference distribution: chi-bar-squared)
  P_Pearson <- 0
  P_LR <- 0
  for (i in 1:r) {
    prob_Pearson <- 1 - pchisq(T_Pearson, i - 1)
    P_Pearson <- P_Pearson + rho[i] * prob_Pearson
    prob_LR <- 1 - pchisq(T_LR, i - 1)
    P_LR <- P_LR + rho[i] * prob_LR
  }

  # Output arguments (observed statistics and P-values)
  results <- list()
  results$T_Pearson <- T_Pearson
  results$P_Pearson <- P_Pearson
  results$T_LR <- T_LR
  results$P_LR <- P_LR

  if (printresults) {
    print(sprintf("Pearson chi-squared test: T = %6.3f, P = %7.5f", T_Pearson, P_Pearson))
    print(sprintf("Likelihood ratio test:    T = %6.3f, P = %7.5f", T_LR, P_LR))
  }

  invisible(results)
}

# ========================
calc_rho <- function(r) {
  # The probabilities for the chi-bar-squared distribution
  # Iterative algorithm by Barlow et al. (1972)
  rho <- matrix(0, 1, r)
  rho[1] <- 1 / r
  rho[r] <- 1 / factorial(r)
  if (r >= 3) {
    for (i in 2:(r - 1)) {
      rho_minus1 <- calc_rho(r - 1)
      rho[i] <- (1 / r) * rho_minus1[i - 1] + ((r - 1) / r) * rho_minus1[i]
    }
  }
  return(rho)
}
