#' @title The Woolf logit confidence interval for the odds ratio
#' @description The Woolf logit confidence interval for the odds ratio
#' @description Described in Chapter 4 "The 2x2 Table"
#' @param n the observed table (a 2x2 matrix)
#' @param alpha the nominal level, e.g. 0.05 for 95% CIs
#' @examples
#' # A case-control study of GADA exposure on IPEX syndrome
#' # (Lampasona et al., 2013):
#' Woolf_logit_CI_2x2(lampasona_2013)
#'
#' # The association between CHRNA4 genotype and XFS (Ritland et al., 2007):
#' Woolf_logit_CI_2x2(ritland_2007)
#'
#' @export
#' @return An object of the [contingencytables_result] class,
#' basically a subclass of [base::list()]. Use the [utils::str()] function
#' to see the specific elements returned.
Woolf_logit_CI_2x2 <- function(n, alpha = 0.05) {
  validateArguments(mget(ls()))

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

  printresults <- function() {
    sprintf(
      "The Woolf logit CI: estimate = %6.4f (%g%% CI %6.4f to %6.4f)",
      estimate, 100 * (1 - alpha), L, U
    )
  }

  res <- list("lower" = L, "upper" = U, "estimate" = estimate)
  return(contingencytables_result(res, printresults))
}
