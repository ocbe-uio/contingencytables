#' @title The Mee asymptotic score confidence interval for the difference between probabilities
#' @description The Mee asymptotic score confidence interval for the difference between probabilities
#' @description Described in Chapter 4 "The 2x2 Table"
#' @param n the observed counts (a 2x2 matrix)
#' @param alpha the nominal level, e.g. 0.05 for 95% CIs
#' @param printresults display results (FALSE = no, TRUE = yes)
#' @examples
#' # An RCT of high vs standard dose of epinephrine (Perondi et al., 2004):
#' n <- matrix(c(7, 27, 1, 33), nrow = 2, byrow = TRUE)
#' Mee_asymptotic_score_CI_2x2(n)
#' # The association between CHRNA4 genotype and XFS (Ritland et al., 2007):
#' n <- matrix(c(0, 16, 15, 57), nrow = 2, byrow = TRUE)
#' Mee_asymptotic_score_CI_2x2(n)
#' @export
#' @return A data frame containing lower, upper and point estimates of the statistic
Mee_asymptotic_score_CI_2x2 <- function(n, alpha = 0.05, printresults = TRUE) {
  n11 <- n[1, 1]
  n21 <- n[2, 1]
  n1p <- n[1, 1] + n[1, 2]
  n2p <- n[2, 1] + n[2, 2]

  # Estimates of the two probabilities of success
  pi1hat <- n[1, 1] / n1p
  pi2hat <- n[2, 1] / n2p

  # Estimate of the difference between probabilities (deltahat)
  estimate <- pi1hat - pi2hat

  # Options for Matlab's fzero command
  tol <- 0.0000001
  delta0 <- -0.99999
  delta1 <- 0.99999

  # Lower CI limit
  if (estimate == -1) {
    L <- -1
  } else {
    L <- uniroot(
      calculate_limit_lower.Mee, c(delta0, estimate),
      n11 = n11, n21 = n21,
      n1p = n1p, n2p = n2p, pi1hat = pi1hat, pi2hat = pi2hat, alpha = alpha,
      tol = tol
    )$root
  }

  # Upper CI limit
  if (estimate == 1) {
    U <- 1
  } else {
    U <- uniroot(
      calculate_limit_upper.Mee, c(estimate, delta1),
      n11 = n11, n21 = n21,
      n1p = n1p, n2p = n2p, pi1hat = pi1hat, pi2hat = pi2hat, alpha = alpha, tol = tol
    )$root
  }

  if (printresults) {
    print(
      sprintf(
        "Mee asymptotic score CI: estimate = %6.4f (%g%% CI %6.4f to %6.4f)",
        estimate, 100 * (1 - alpha), L, U
      ),
      quote = FALSE
    )
  }

  res <- data.frame(lower = L, upper = U, estimate = estimate)
  invisible(res)
}
