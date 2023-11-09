#' @title The adjusted inverse hyperbolic sine confidence interval for the
#' ratio of probabilities
#' @description The adjusted inverse hyperbolic sine confidence interval for
#' the ratio of probabilities
#' @description Described in Chapter 4 "The 2x2 Table"
#' @param n the observed counts (a 2x2 matrix)
#' @param psi1 pseudo-frequency
#' @param psi2 pseudo-frequency
#' @param psi3 pseudo-frequency
#' @param psi4 pseudo-frequency
#' @param alpha the nominal level, e.g. 0.05 for 95% CIs
#' @return An object of the [contingencytables_result] class,
#' basically a subclass of [base::list()]. Use the [utils::str()] function
#' to see the specific elements returned.
#' @examples
#' Adjusted_inv_sinh_CI_ratio_2x2(perondi_2004)
#' Adjusted_inv_sinh_CI_ratio_2x2(ritland_2007)
#' @export
Adjusted_inv_sinh_CI_ratio_2x2 <- function(
  n, psi1 = 0, psi2 = 0, psi3 = 0, psi4 = 1, alpha = 0.05
) {
  validateArguments(
    x = mget(ls()),
    types = c(
      n = "counts", psi1 = "skip", psi2 = "skip", psi3 = "skip", psi4 = "skip",
      alpha = "probability"
    )
  )
  n1p <- n[1, 1] + n[1, 2]
  n2p <- n[2, 1] + n[2, 2]

  # Estimates of the two probabilities of success
  pi1hat <- n[1, 1] / n1p
  pi2hat <- n[2, 1] / n2p

  # Estimate of the ratio of probabilities (phihat)
  estimate <- pi1hat / pi2hat

  # Adjusted estimate
  phitilde <- ((n[1, 1] + psi1) / (n1p + psi1 + psi2)) / ((n[2, 1] + psi1) / (n2p + psi1 + psi2))

  # The upper alpha / 2 percentile of the standard normal distribution
  z <- qnorm(1 - alpha / 2, 0, 1)

  # Standard error estimate
  tmp <- asinh(0.5 * z * sqrt(1 / (n[1, 1] + psi3) + 1 / (n[2, 1] + psi3) - 1 / (n1p + psi3 + psi4) - 1 / (n2p + psi3 + psi4)))

  # Calculate the confidence limits
  L <- exp(log(phitilde) - 2 * tmp)
  U <- exp(log(phitilde) + 2 * tmp)

  # Fix zero cell cases
  if (n[1, 1] == 0 && psi3 == 0) {
    U <- ((z^2) / n1p) / (n[2, 1] / n2p)
  }
  if (n[2, 1] == 0 && psi3 == 0) {
    L <- (n[1, 1] / n1p) / ((z^2) / n2p)
  }

  # Output
  printresults <- function() {
    cat_sprintf(
      "The adjusted inverse sinh CI: estimate = %6.4f (%g%% CI %6.4f to %6.4f)",
      estimate, 100 * (1 - alpha), L, U
    )
  }
  return(
    contingencytables_result(
      list(lower = L, upper = U, estimate = estimate), printresults
    )
  )
}
