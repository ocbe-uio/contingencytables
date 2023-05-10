#' @title The Miettinen-Nurminen asymptotic score confidence interval for the ratio of probabilities
#' @description The Miettinen-Nurminen asymptotic score confidence interval for the ratio of probabilities
#' @description Described in Chapter 4 "The 2x2 Table"
#' @param n the observed counts (a 2x2 matrix)
#' @param alpha the nominal level, e.g. 0.05 for 95% CIs
#' @examples
#' # An RCT of high vs standard dose of epinephrine (Perondi et al., 2004)
#' n <- perondi_2004
#' MiettinenNurminen_asymptotic_score_CI_ratio_2x2(n)
#' # The association between CHRNA4 genotype and XFS (Ritland et al., 2007)
#' n <- ritland_2007
#' MiettinenNurminen_asymptotic_score_CI_ratio_2x2(n)
#' @export
#' @return An object of the [contingencytables_result] class,
#' basically a subclass of [base::list()]. Use the [utils::str()] function
#' to see the specific elements returned.
MiettinenNurminen_asymptotic_score_CI_ratio_2x2 <- function(n, alpha = 0.05) {
  validateArguments(mget(ls()))

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
      calculate_limit_lower.Miettinen_ratio,
      c(phi0, phi1),
      n11 = n11, n21 = n21, n1p = n1p,
      n2p = n2p, pi1hat = pi1hat, pi2hat = pi2hat, alpha = alpha, tol = tol
    )$root
  } else if (estimate == 0) {
    L <- 0
  } else {
    L <- uniroot(
      calculate_limit_lower.Miettinen_ratio, c(phi0, estimate),
      n11 = n11, n21 = n21, n1p = n1p,
      n2p = n2p, pi1hat = pi1hat, pi2hat = pi2hat, alpha = alpha, tol = tol
    )$root
  }

  # Upper CI limit
  if (is.na(estimate) || estimate == Inf) {
    U <- Inf
  } else if (estimate == 0) {
    U <- uniroot(
      calculate_limit_upper.Miettinen_ratio, c(phi0, phi1),
      n11 = n11, n21 = n21, n1p = n1p,
      n2p = n2p, pi1hat = pi1hat, pi2hat = pi2hat, alpha = alpha, tol = tol
    )$root
  } else {
    U <- uniroot(
      calculate_limit_upper.Miettinen_ratio, c(estimate, phi1),
      n11 = n11, n21 = n21, n1p = n1p,
      n2p = n2p, pi1hat = pi1hat, pi2hat = pi2hat, alpha = alpha, tol = tol
    )$root
  }

  return(
    contingencytables_result(
      list(lower = L, upper = U, estimate = estimate),
      sprintf(
        "Mietinen-Nurminen asymptotic score CI: estimate = %6.4f (%g%% CI %6.4f to %6.4f)",
        estimate, 100 * (1 - alpha), L, U
      )
    )
  )
}
