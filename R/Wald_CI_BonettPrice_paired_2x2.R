#' @title The Wald confidence interval for the difference between paired
#' probabilities
#' @description The Wald confidence interval for the difference between paired
#' probabilities
#' @description with the pseudo-frequency adjustment suggested by
#' Bonett and Price(2012)
#' @description Described in Chapter 8 "The Paired 2x2 Table"
#' @param n the observed counts (a 2x2 matrix)
#' @param alpha the nominal level, e.g. 0.05 for 95% CIs
#' @examples
#' # Airway hyper-responsiveness before and after stem cell transplantation
#' # (Bentur et al., 2009)
#' Wald_CI_BonettPrice_paired_2x2(bentur_2009)
#'
#' # Complete response before and after consolidation therapy
#' # (Cavo et al., 2012)
#' Wald_CI_BonettPrice_paired_2x2(cavo_2012)
#' @export
#' @return An object of the [contingencytables_result] class,
#' basically a subclass of [base::list()]. Use the [utils::str()] function
#' to see the specific elements returned.
Wald_CI_BonettPrice_paired_2x2 <- function(n, alpha = 0.05) {
  validateArguments(mget(ls()))

  # Estimate of the difference between probabilities (deltahat)
  N <- sum(n)
  estimate <- (n[1, 2] - n[2, 1]) / N

  # Calculate the Laplace estimates
  pi12tilde <- (n[1, 2] + 1) / (N + 2)
  pi21tilde <- (n[2, 1] + 1) / (N + 2)

  # Standard error of the estimate
  SE <- sqrt((pi12tilde + pi21tilde - (pi12tilde - pi21tilde)^2) / (N + 2))

  # The upper alpha / 2 percentile of the standard normal distribution
  z <- qnorm(1 - alpha / 2, 0, 1)

  # Calculate the confidence limits
  L <- pi12tilde - pi21tilde - z * SE
  U <- pi12tilde - pi21tilde + z * SE

  # Fix overshoot by truncation
  L <- max(-1, L)
  U <- min(U, 1)

  printresults <- function() {
    sprintf(
      paste(
        "The Wald CI with Bonett-Price adjustment: estimate =",
        "%6.4f (%g%% CI %6.4f to %6.4f)"
      ),
      estimate, 100 * (1 - alpha), L, U
    )
  }

  return(
    contingencytables_result(
      list(L = L, U = U, estimate = estimate), printresults
    )
  )
}
