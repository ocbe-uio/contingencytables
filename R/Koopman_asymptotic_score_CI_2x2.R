#' @title The Koopman asymptotic score confidence interval for the ratio of probabilities
#' @description The Koopman asymptotic score confidence interval for the ratio of probabilities
#' @note This versions uses the score test statistic of the Miettinen-Nurminen
#' interval without the variance correction term.
#' @description Described in Chapter 4 "The 2x2 Table"
#' @param n the observed counts (a 2x2 matrix)
#' @param alpha the nominal level, e.g. 0.05 for 95% CIs
#' @param printresults display results (FALSE = no, TRUE = yes)
#' @examples
#' # An RCT of high vs standard dose of epinephrine (Perondi et al., 2004):
#' n <- matrix(c(7, 27, 1, 33), nrow = 2, byrow = TRUE)
#' Koopman_asymptotic_score_CI_2x2(n)
#' # The association between CHRNA4 genotype and XFS (Ritland et al., 2007):
#' n <- matrix(c(0, 16, 15, 57), nrow = 2, byrow = TRUE)
#' Koopman_asymptotic_score_CI_2x2(n)
#' @export
#' @return A data frame containing lower, upper and point estimates of the statistic
Koopman_asymptotic_score_CI_2x2 <- function(n, alpha = 0.05, printresults = TRUE) {
  n11 <- n[1, 1]
  n21 <- n[2, 1]
  n1p <- n[1, 1] + n[1, 2]
  n2p <- n[2, 1] + n[2, 2]

  # Estimates of the two probabilities of success
  pi1hat <- n[1, 1] / n1p
  pi2hat <- n[2, 1] / n2p

  # Estimate of the ratio of probabilities (phihat)
  estimate <- pi1hat / pi2hat

  # Options for Matlab's fzero command
  tol <- 0.0000001
  phi0 <- 0.00001
  phi1 <- 100000

  # Lower CI limit
  if (n[1, 1] == 0 && n[2, 1] == 0) {
    L <- 0
  } else if (is.na(estimate) || estimate == Inf) {
    L <- uniroot(
      calculate_limit_lower.Koopman, c(phi0, phi1),
      n11 = n11, n21 = n21,
      n1p = n1p, n2p = n2p, pi1hat = pi1hat, pi2hat = pi2hat, alpha = alpha, tol = tol
    )$root
  } else if (estimate == 0) {
    L <- 0
  } else {
    L <- uniroot(
      calculate_limit_lower.Koopman, c(phi0, estimate),
      n11 = n11, n21 = n21,
      n1p = n1p, n2p = n2p, pi1hat = pi1hat, pi2hat = pi2hat, alpha = alpha, tol = tol
    )$root
  }

  # Upper CI limit
  if (is.na(estimate) || estimate == Inf) {
    U <- Inf
  } else if (estimate == 0) {
    U <- uniroot(
      calculate_limit_upper.Koopman, c(phi0, phi1),
      n11 = n11, n21 = n21,
      n1p = n1p, n2p = n2p, pi1hat = pi1hat, pi2hat = pi2hat, alpha = alpha, tol = tol
    )$root
  } else {
    U <- uniroot(
      calculate_limit_upper.Koopman, c(estimate, phi1),
      n11 = n11, n21 = n21,
      n1p = n1p, n2p = n2p, pi1hat = pi1hat, pi2hat = pi2hat, alpha = alpha, tol = tol
    )$root
  }

  if (printresults) {
    print(
      sprintf(
        "Koopman asymptotic score CI: estimate = %6.4f (%g%% CI %6.4f to %6.4f)",
        estimate, 100 * (1 - alpha), L, U
      ),
      quote = FALSE
    )
  }

  res <- data.frame(lower = L, upper = U, estimate = estimate)
  invisible(res)
}
