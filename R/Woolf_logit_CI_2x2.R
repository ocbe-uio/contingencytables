#' @title The Woolf logit confidence interval for the odds ratio
#' @description The Woolf logit confidence interval for the odds ratio
#' @description Described in Chapter 4 "The 2x2 Table"
#' @param n the observed table (a 2x2 matrix)
#' @param alpha the nominal level, e.g. 0.05 for 95% CIs
#' @param printresults display results (FALSE = no, TRUE = yes)
#' @examples
#' # A case-control study of GADA exposure on IPEX syndrome (Lampasona et al., 2013):
#' n <- matrix(c(9, 4, 4, 10), nrow = 2, byrow = TRUE)
#' Woolf_logit_CI_2x2(n)
#'
#' # The association between CHRNA4 genotype and XFS (Ritland et al., 2007):
#' n <- matrix(c(0, 16, 15, 57), nrow = 2, byrow = TRUE)
#' Woolf_logit_CI_2x2(n)
#'
#' @export
#' @return A vector containing lower, upper and point estimates of the statistic
Woolf_logit_CI_2x2 <- function(n, alpha = 0.05, printresults = TRUE) {
  # Estimate of the odds ratio (thetahat)
  estimate <- n[1, 1] * n[2, 2] / (n[1, 2] * n[2, 1])

  # Standard error of log(estimate)
  SE <- sqrt(1 / n[1, 1] + 1 / n[1, 2] + 1 / n[2, 1] + 1 / n[2, 2])

  # The upper alpha / 2 percentile of the standard normal distribution
  z <- qnorm(1 - alpha / 2, 0, 1)

  # Calculate the confidence limits
  L <- exp(log(estimate) - z * SE)
  U <- exp(log(estimate) + z * SE)

  # Fix zero cell cases
  if (n[1, 1] == 0 || n[2, 1] == 0 || n[1, 2] == 0 || n[2, 2] == 0) {
    L <- 0
    U <- Inf
  }

  if (printresults) {
    print(sprintf(
      "The Woolf logit CI: estimate = %6.4f (%g%% CI %6.4f to %6.4f)",
      estimate, 100 * (1 - alpha), L, U
    ), quote = FALSE)
  }

  res <- data.frame(lower = L, upper = U, estimate = estimate)
  invisible(res)
}
