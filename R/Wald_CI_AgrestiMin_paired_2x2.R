#' @title The Wald confidence interval for the difference between paired
#' probabilities
#' @description The Wald confidence interval for the difference between paired
#' probabilities
#' @description with the pseudo-frequency adjustment suggested by
#' Agresti and Min (2005)
#' @description Described in Chapter 8 "The Paired 2x2 Table"
#' @param n the observed counts (a 2x2 matrix)
#' @param alpha the nominal level, e.g. 0.05 for 95% CIs
#' @examples
#' # Airway hyper-responsiveness before and after stem cell transplantation
#' # (Bentur et al., 2009)
#' Wald_CI_AgrestiMin_paired_2x2(bentur_2009)
#'
#' # Complete response before and after consolidation therapy
#' # (Cavo et al., 2012)
#' Wald_CI_AgrestiMin_paired_2x2(cavo_2012)
#'
#' @export
#' @return An object of the [contingencytables_result] class,
#' basically a subclass of [base::list()]. Use the [utils::str()] function
#' to see the specific elements returned.
Wald_CI_AgrestiMin_paired_2x2 <- function(n, alpha = 0.05) {
  validateArguments(mget(ls()))

  # Estimate of the difference between probabilities (deltahat)
  N <- sum(n)
  estimate <- (n[1, 2] - n[2, 1]) / N

  # Add 1 / 2 pseudo-observations to each cell
  ntilde <- n + 0.5
  Ntilde <- sum(ntilde)

  # Standard error of the estimate
  SE <- sqrt(
    (ntilde[1, 2] + ntilde[2, 1]) - ((ntilde[1, 2] - ntilde[2, 1])^2) / Ntilde
  ) / Ntilde

  # The upper alpha / 2 percentile of the standard normal distribution
  z <- qnorm(1 - alpha / 2, 0, 1)

  # Calculate the confidence limits
  L <- (ntilde[1, 2] - ntilde[2, 1]) / Ntilde - z * SE
  U <- (ntilde[1, 2] - ntilde[2, 1]) / Ntilde + z * SE

  # Fix overshoot by truncation
  L <- max(-1, L)
  U <- min(U, 1)

  printresults <- function() {
    sprintf(
      paste(
        "The Wald CI with Agresti-Min adjustment: estimate =",
        "%6.4f (%g%% CI %6.4f to %6.4f)"
      ),
      estimate, 100 * (1 - alpha), L, U
    )
  }

  invisible(list("lower" = L, "upper" = U, "estimate" = estimate))
  return(
    contingencytables_result(
      list("lower" = L, "upper" = U, "estimate" = estimate),
      printresults
    )
  )

}
