#' @title The adjusted log confidence interval for the ratio of probabilities
#' @description The adjusted log confidence interval for the ratio of probabilities
#' @description Described in Chapter 4 "The 2x2 Table"
#' @param n the observed counts (a 2x2 matrix)
#' @param alpha the nominal level, e.g. 0.05 for 95% CIs
#' @return An object of the [contingencytables_result] class,
#' basically a subclass of [base::list()]. Use the [utils::str()] function
#' to see the specific elements returned.
#' @examples
#' Adjusted_log_CI_2x2(perondi_2004)
#' Adjusted_log_CI_2x2(ritland_2007)
#' @export
Adjusted_log_CI_2x2 <- function(n, alpha = 0.05) {
  validateArguments(mget(ls()))
  n1p <- n[1, 1] + n[1, 2]
  n2p <- n[2, 1] + n[2, 2]

  # Estimate of the ratio of probabilities (phihat)
  estimate <- (n[1, 1] / n1p) / (n[2, 1] / n2p)

  # Adjusted estimates of the two probabilities of success (add 1 / 2 success in each group)
  pi1hat <- (n[1, 1] + 0.5) / (n1p + 0.5)
  pi2hat <- (n[2, 1] + 0.5) / (n2p + 0.5)

  # Adjusted estimate of the ratio of probabilities (phihat_1 / 2)
  adj.estimate <- pi1hat / pi2hat

  # Standard error of the log of the adjusted estimate
  SE <- sqrt(1 / (n[1, 1] + 0.5) + 1 / (n[2, 1] + 0.5) - 1 / (n1p + 0.5) - 1 / (n2p + 0.5))

  # The upper alpha / 2 percentile of the standard normal distribution
  z <- qnorm(1 - alpha / 2, 0, 1)

  # Calculate the confidence limits
  L <- exp(log(adj.estimate) - z * SE)
  U <- exp(log(adj.estimate) + z * SE)

  # Output
  printresults <- function() {
    cat_sprintf(
      "The adjusted log CI: estimate = %6.4f (%g%% CI %6.4f to %6.4f)",
      estimate, 100 * (1 - alpha), L, U
    )
  }
  return(
    contingencytables_result(
      list(lower = L, upper = U, estimate = estimate), printresults
    )
  )
}
