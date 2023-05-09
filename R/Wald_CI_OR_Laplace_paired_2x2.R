#' @title The Wald confidence interval for the conditional odds ratio with
#' Laplace adjustment
#' @description The Wald confidence interval for the conditional odds ratio with
#' Laplace adjustment
#' @description Described in Chapter 8 "The Paired 2x2 Table"
#' @param n the observed counts (a 2x2 matrix)
#' @param alpha the nominal level, e.g. 0.05 for 95% CIs
#' @examples
#' Wald_CI_OR_Laplace_paired_2x2(ezra_2010)
#' @export
#' @return An object of the [contingencytables_result] class,
#' basically a subclass of [base::list()]. Use the [utils::str()] function
#' to see the specific elements returned.
Wald_CI_OR_Laplace_paired_2x2 <- function(n, alpha = 0.05) {
  validateArguments(mget(ls()))

  # Estimate of the conditional odds ratio (thetacondhat)
  estimate <- n[1, 2] / n[2, 1]

  # Add 1 pseudo-observation to n_12 and n_21
  n12tilde <- n[1, 2] + 1
  n21tilde <- n[2, 1] + 1
  thetacondtilde <- n12tilde / n21tilde

  # Standard error of the estimate
  SE <- sqrt(1 / n12tilde + 1 / n21tilde)

  # The upper alpha / 2 percentile of the standard normal distribution
  z <- qnorm(1 - alpha / 2, 0, 1)

  # Calculate the confidence limits
  L <- exp(log(thetacondtilde) - z * SE)
  U <- exp(log(thetacondtilde) + z * SE)

  if (is.na(L)) {
    L <- 0
  }
  if (is.na(U)) {
    U <- Inf
  }

  printresults <- function() {
    sprintf(
      paste(
        "The Wald CI w / Laplace adjustment: estimate =",
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
