#' @title The Miettinen-Nurminen asymptotic score CI for the odds ratio
#' @description The Miettinen-Nurminen asymptotic score confidence interval
#' for the odds ratio
#' @description Described in Chapter 4 "The 2x2 Table"
#' @param n the observed counts (a 2x2 matrix)
#' @param alpha the nominal level, e.g. 0.05 for 95% CIs
#' @examples
#' # A case-control study of GADA exposure on IPEX syndrome (Lampasona et al., 2013)
#' MiettinenNurminen_asymptotic_score_CI_OR_2x2(lampasona_2013)
#' # The association between CHRNA4 genotype and XFS (Ritland et al., 2007)
#' MiettinenNurminen_asymptotic_score_CI_OR_2x2(ritland_2007)
#' @export
#' @return An object of the [contingencytables_result] class,
#' basically a subclass of [base::list()]. Use the [utils::str()] function
#' to see the specific elements returned.
MiettinenNurminen_asymptotic_score_CI_OR_2x2 <- function(n, alpha = 0.05) {
  validateArguments(mget(ls()))

  n11 <- n[1, 1]
  n21 <- n[2, 1]
  n1p <- n[1, 1] + n[1, 2]
  n2p <- n[2, 1] + n[2, 2]

  # Estimate of the odds ratio (thetahat)
  estimate <- n[1, 1] * n[2, 2] / (n[1, 2] * n[2, 1])

  # Options for Matlab's fzero command
  tol <- 0.0000001
  theta0 <- 0.00001
  theta1 <- 100000

  # Lower CI limit
  if (is.na(estimate) || estimate == Inf) {
    L <- uniroot(
      calculate_limit_lower, c(theta0, theta1),
      n11 = n11, n21 = n21, n1p = n1p,
      n2p = n2p, alpha = alpha, tol = tol
    )$root
  } else if (estimate == 0) {
    L <- 0
  } else {
    L <- uniroot(
      calculate_limit_lower, c(theta0, estimate),
      n11 = n11, n21 = n21,
      n1p = n1p, n2p = n2p, alpha = alpha, tol = tol
    )$root
  }

  # Upper CI limit
  if (n[2, 1] == 0 || n[1, 2] == 0) {
    U <- Inf
  } else if (estimate == 0) {
    U <- uniroot(
      calculate_limit_upper, c(theta0, theta1),
      n11 = n11, n21 = n21, n1p = n1p,
      n2p = n2p, alpha = alpha, tol = tol
    )$root
  } else {
    U <- uniroot(
      calculate_limit_upper, c(estimate, theta1),
      n11 = n11, n21 = n21,
      n1p = n1p, n2p = n2p, alpha = alpha, tol = tol
    )$root
  }

  return(
    contingencytables_result(
      list("lower" = L, "upper" = U, "estimate" = estimate),
      sprintf(
        "Miettinen-Nurminen asymptotic score CI: estimate = %6.4f (%g%% CI %6.4f to %6.4f)",
        estimate, 100 * (1 - alpha), L, U
      )
    )
  )
}
