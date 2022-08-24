#' @title The Wald confidence interval for the difference between paired
#' probabilities
#' @description The Wald confidence interval for the difference between paired
#' probabilities
#' @description Described in Chapter 8 "The Paired 2x2 Table"
#' @param n the observed counts (a 2x2 matrix)
#' @param alpha the nominal level, e.g. 0.05 for 95% CIs
#' @param printresults display results (FALSE = no, TRUE = yes)
#' @examples
#' # Airway hyper-responsiveness before and after stem cell transplantation
#' # (Bentur et al., 2009)
#' Wald_CI_diff_paired_2x2(bentur_2009)
#'
#' # Complete response before and after consolidation therapy
#' # (Cavo et al., 2012)
#' Wald_CI_diff_paired_2x2(cavo_2012)
#' @export
#' @return A list containing lower, upper and point estimates of the statistic
Wald_CI_diff_paired_2x2 <- function(n, alpha = 0.05, printresults = TRUE) {
  validateArguments(mget(ls()))

  N <- sum(n)

  # Estimate of the difference between probabilities (deltahat)
  estimate <- (n[1, 2] - n[2, 1]) / N

  # Standard error of the estimate
  SE <- sqrt((n[1, 2] + n[2, 1]) - ((n[1, 2] - n[2, 1])^2) / N) / N

  # The upper alpha / 2 percentile of the standard normal distribution
  z <- qnorm(1 - alpha / 2, 0, 1)

  # Calculate the confidence limits
  L <- estimate - z * SE
  U <- estimate + z * SE

  # Fix overshoot by truncation
  L <- max(-1, L)
  U <- min(U, 1)

  if (printresults) {
    my_sprintf(
      "The Wald CI: estimate = %6.4f (%g%% CI %6.4f to %6.4f)\n", estimate,
      100 * (1 - alpha), L, U
    )
  }

  invisible(list(L = L, U = U, estimate = estimate))
}
