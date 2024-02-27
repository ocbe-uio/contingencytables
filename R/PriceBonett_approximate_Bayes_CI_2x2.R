#' @title The Price-Bonett approximate Bayes confidence interval for the ratio of probabilities
#' @description The Price-Bonett approximate Bayes confidence interval for the ratio of probabilities
#' @description Described in Chapter 4 "The 2x2 Table"
#' @param n the observed counts (a 2x2 matrix)
#' @param a,b parameters of the beta distribution
#' @param alpha the nominal level, e.g. 0.05 for 95# CIs
#' @examples
#' # An RCT of high vs standard dose of epinephrine (Perondi et al., 2004)
#' PriceBonett_approximate_Bayes_CI_2x2(perondi_2004)
#'
#' # The association between CHRNA4 genotype and XFS (Ritland et al., 2007)
#' PriceBonett_approximate_Bayes_CI_2x2(ritland_2007)
#'
#' @export
#' @return An object of the [contingencytables_result] class,
#' basically a subclass of [base::list()]. Use the [utils::str()] function
#' to see the specific elements returned.
PriceBonett_approximate_Bayes_CI_2x2 <- function(n, a = 1.25, b = 2.5, alpha = 0.05) {
  validateArguments(mget(ls()))
  n1p <- n[1, 1] + n[1, 2]
  n2p <- n[2, 1] + n[2, 2]

  # Estimates of the two probabilities of success
  pi1hat <- n[1, 1] / n1p
  pi2hat <- n[2, 1] / n2p

  # Estimate of the ratio of probabilities (phihat)
  estimate <- pi1hat / pi2hat

  # Approximation of log(pi_1 / pi_2) has mean log_phi_tilde...
  pi1tilde <- (n[1, 1] + a - 1) / (n1p + a + b - 2)
  pi2tilde <- (n[2, 1] + a - 1) / (n2p + a + b - 2)
  log_phi_tilde <- log(pi1tilde) - log(pi2tilde)

  # ... and variance var_log_phi_tilde
  tmp1 <- n[1, 1] + a - 1 + ((n[1, 1] + a - 1)^2) / (n1p - n[1, 1] + b - 1)
  tmp2 <- n[2, 1] + a - 1 + ((n[2, 1] + a - 1)^2) / (n2p - n[2, 1] + b - 1)
  var_log_phi_tilde <- 1 / tmp1 + 1 / tmp2

  # The upper alpha / 2 percentile of the standard normal distribution
  z <- qnorm(1 - alpha / 2, 0, 1)

  # Calculate the confidence limits
  L <- exp(log_phi_tilde - z * sqrt(var_log_phi_tilde))
  U <- exp(log_phi_tilde + z * sqrt(var_log_phi_tilde))

  return(
    contingencytables_result(
      list("lower" = L, "upper" = U, "estimate" = estimate),
      sprintf(
        "The Price-Bonett approximate Bayes CI: estimate = %6.4f (%g%% CI %6.4f to %6.4f)",
        estimate, 100 * (1 - alpha), L, U
      )
    )
  )
}
