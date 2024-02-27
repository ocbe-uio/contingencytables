#' @title The Wald confidence interval for the ratio of paired probabilities
#' @description The Wald confidence interval for the ratio of paired
#' probabilities
#' @description Described in Chapter 8 "The Paired 2x2 Table"
#' @param n the observed counts (a 2x2 matrix)
#' @param alpha the nominal level, e.g. 0.05 for 95% CIs
#' @examples
#' # Airway hyper-responsiveness before and after stem cell transplantation
#' # (Bentur et al., 2009)
#' Wald_CI_ratio_paired_2x2(bentur_2009)
#'
#' # Complete response before and after consolidation therapy
#' # (Cavo et al., 2012)
#' Wald_CI_ratio_paired_2x2(cavo_2012)
#'
#' @export
#' @return An object of the [contingencytables_result] class,
#' basically a subclass of [base::list()]. Use the [utils::str()] function
#' to see the specific elements returned.
Wald_CI_ratio_paired_2x2 <- function(n, alpha = 0.05) {
  validateArguments(mget(ls()))

  # Estimate of the ratio of probabilities (phihat)
  estimate <- (n[1, 1] + n[1, 2]) / (n[1, 1] + n[2, 1])

  # Standard error of the estimate
  SE <- sqrt((n[1, 2] + n[2, 1]) / ((n[1, 1] + n[1, 2]) * (n[1, 1] + n[2, 1])))

  # The upper alpha / 2 percentile of the standard normal distribution
  z <- qnorm(1 - alpha / 2, 0, 1)

  # Calculate the confidence limits
  L <- exp(log(estimate) - z * SE)
  U <- exp(log(estimate) + z * SE)

  # Handle zero-count cases
  if (is.na(estimate) || estimate == 0 || abs(estimate) == Inf) {
    L <- 0
    U <- Inf
  }

  printresults <- function() {
    sprintf(
      "The Wald CI: estimate = %6.4f (%g%% CI %6.4f to %6.4f)", estimate,
      100 * (1 - alpha), L, U
    )
  }

  return(
    contingencytables_result(
      list("lower" = L, "upper" = U, "estimate" = estimate), printresults
    )
  )
}
