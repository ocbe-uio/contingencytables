#' @title The Pearson chi-squared and likelihood ratio tests for unspecific ordering in rx2 tables
#' @description The Pearson chi-squared and likelihood ratio tests for unspecific ordering in rx2 tables.
#' Described in Chapter 5 "The Ordered rx2 Table".
#' May also be used for 2xc tables, after flipping rows and columns (i.e. if
#' n is a 2xc table, call this function with n' (the transpose of n) as the
#' first argument).
#' @param n the observed counts (an rx2 matrix)
#' @param direction the direction of the success probabilities ("increasing" or "decreasing")
#' @examples
#' # Chapter 5: Alcohol consumption and malformations (Mills and Graubard, 1987)
#' Pearson_LR_tests_unspecific_ordering_rx2(mills_graubard_1987, "increasing")
#'
#' # Chapter 5: Elevated troponin T levels in stroke patients (Indredavik et al., 2008)
#' Pearson_LR_tests_unspecific_ordering_rx2(indredavik_2008, "decreasing")
#'
#' # Chapter 6: Postoperative nausea (Lydersen et al., 2012a)
#' Pearson_LR_tests_unspecific_ordering_rx2(t(lydersen_2012a), "decreasing")
#' @export
#' @return An object of the [contingencytables_result] class,
#' basically a subclass of [base::list()]. Use the [utils::str()] function
#' to see the specific elements returned.
Pearson_LR_tests_unspecific_ordering_rx2 <- function(n, direction) {
  validateArguments(mget(ls()))

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

  printresults <- function() {
    my_sprintf_cat("Pearson chi-squared test: T = %6.3f, P = %7.5f\n", T_Pearson, P_Pearson)
    my_sprintf_cat("Likelihood ratio test:    T = %6.3f, P = %7.5f", T_LR, P_LR)
  }

  return(contingencytables_result(results, printresults))
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
