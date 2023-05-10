# function [L, U, estimate] = Wald_CI_1x2(X, n, alpha, printresults)

#' @title The Wald confidence interval for the binomial probability
#' @description Described in Chapter 2 "The 1x2 Table and the Binomial
#' Distribution"
#' @param X the number of successes
#' @param n the total number of observations
#' @param alpha the nominal level, e.g. 0.05 for 95% CIs
#' @importFrom stats qnorm
#' @examples
#' Wald_CI_1x2(singh_2010["1st", "X"], singh_2010["1st", "n"])
#' Wald_CI_1x2(singh_2010["2nd", "X"], singh_2010["2nd", "n"])
#' Wald_CI_1x2(singh_2010["3rd", "X"], singh_2010["3rd", "n"])
#' with(singh_2010["4th", ], Wald_CI_1x2(X, n)) # alternative syntax
#' Wald_CI_1x2(ligarden_2010["X"], ligarden_2010["n"]) # Ligarden et al. (2010)
#' @export
#' @return An object of the [contingencytables_result] class,
#' basically a subclass of [base::list()]. Use the [utils::str()] function
#' to see the specific elements returned.
Wald_CI_1x2 <- function(X, n, alpha = 0.05) {
  validateArguments(mget(ls()))

  # Estimate of the binomial probability (pihat)
  estimate <- X / n

  # The standard error of the estimate
  SE <- sqrt(estimate * (1 - estimate) / n)

  # The upper alpha/2 percentile of the standard normal distribution
  z <- qnorm(1 - alpha / 2, 0, 1)

  # Calculate the confidence limits
  L <- estimate - z * SE
  U <- estimate + z * SE

  # Overshoot can happen: truncate the results
  L <- max(0, L)
  U <- min(U, 1)

  printresults <- function() {
    my_sprintf_cat(
      "The Wald CI: estimate = %6.4f (%g%% CI %6.4f to %6.4f)",
      estimate, 100 * (1 - alpha), L, U
    )
  }

  res <- list(L, U, estimate)
  names(res) <- c("lower", "upper", "estimate")
  return(contingencytables_result(res, printresults))
}
