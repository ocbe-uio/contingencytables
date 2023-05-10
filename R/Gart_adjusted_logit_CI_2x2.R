#' @title The Gart adjusted logit confidence interval for the odds ratio
#' @description The Gart adjusted logit confidence interval for the odds ratio
#' @description Described in Chapter 4 "The 2x2 Table"
#' @param n the observed table (a 2x2 matrix)
#' @param alpha the nominal level, e.g. 0.05 for 95% CIs
#' @examples
#' Gart_adjusted_logit_CI_2x2(lampasona_2013)
#' Gart_adjusted_logit_CI_2x2(ritland_2007)
#' @export
#' @return An object of the [contingencytables_result] class,
#' basically a subclass of [base::list()]. Use the [utils::str()] function
#' to see the specific elements returned.
Gart_adjusted_logit_CI_2x2 <- function(n, alpha = 0.05) {
  validateArguments(mget(ls()))

  # Estimate of the odds ratio (thetahat)
  estimate <- n[1, 1] * n[2, 2] / (n[1, 2] * n[2, 1])

  # Add 1 / 2 to all cells
  n11tilde <- n[1, 1] + 0.5
  n12tilde <- n[1, 2] + 0.5
  n21tilde <- n[2, 1] + 0.5
  n22tilde <- n[2, 2] + 0.5

  # Adjusted estimate of the odds ratio (thetahattilde)
  estimate_adj <- n11tilde * n22tilde / (n12tilde * n21tilde)

  # Standard error of the log of the adjusted estimate
  SE <- sqrt(1 / n11tilde + 1 / n12tilde + 1 / n21tilde + 1 / n22tilde)

  # The upper alpha / 2 percentile of the standard normal distribution
  z <- qnorm(1 - alpha / 2, 0, 1)

  # Calculate the confidence limits
  L <- exp(log(estimate_adj) - z * SE)
  U <- exp(log(estimate_adj) + z * SE)

  res <- list("lower" = L, "upper" = U, "estimate" = estimate)
  return(
    contingencytables_result(
      res,
      sprintf(
        "The Gart adjusted logit CI: estimate = %6.4f (%g%% CI %6.4f to %6.4f)",
        estimate, 100 * (1 - alpha), L, U
      )
    )
  )
}
