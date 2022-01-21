#' @title The Wald confidence interval for the conditional odds ratio
#' @description The Wald confidence interval for the conditional odds ratio
#' @description Described in Chapter 8 "The Paired 2x2 Table"
#' @param n the observed counts (a 2x2 matrix)
#' @param alpha the nominal level, e.g. 0.05 for 95% CIs
#' @param printresults display results (FALSE = no, TRUE = yes)
#' @examples
#'
#' # Floppy eyelid syndrome vs obstructive sleep apnea (Ezra et al., 2010)
#' n <- rbind(c(7, 25), c(2, 68))
#' Wald_CI_OR_paired_2x2(n)
#'
#' @export
#' @return A list containing lower, upper and point estimates of the statistic
Wald_CI_OR_paired_2x2 <- function(n, alpha = 0.05, printresults = TRUE) {
  # Estimate of the conditional odds ratio (thetacondhat)
  estimate <- n[1, 2] / n[2, 1]

  # Standard error of the estimate
  SE <- sqrt(1 / n[1, 2] + 1 / n[2, 1])

  # The upper alpha / 2 percentile of the standard normal distribution
  z <- qnorm(1 - alpha / 2, 0, 1)

  # Calculate the confidence limits
  L <- exp(log(estimate) - z * SE)
  U <- exp(log(estimate) + z * SE)

  if (is.na(L)) {
    L <- 0
  }
  if (is.na(U)) {
    U <- Inf
  }

  if (printresults) {
    .print("The Wald CI: estimate = %6.4f (%g%% CI %6.4f to %6.4f)\n", estimate, 100 * (1 - alpha), L, U)
  }

  invisible(list(L = L, U = U, estimate = estimate))
}


.print <- function(s, ...) {
  print(sprintf(gsub("\n", "", s), ...), quote = FALSE)
}
