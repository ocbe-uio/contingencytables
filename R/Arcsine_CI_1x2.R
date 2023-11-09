#' @title Arcsine confidence interval
#' @description The Arcsine confidence interval for the binomial probability
#' (with Anscombe variance stabilizing transformation)
#' Described in Chapter 2 "The 1x2 Table and the Binomial Distribution"
#' @references Anscombe FJ (1948) The transformation of Poisson, binomial and
#' negative binomial data. Biometrika; 35:246-254
#'
#' @param X the number of successes
#' @param n the total number of observations
#' @param alpha the nominal level, e.g. 0.05 for 95% CIs
#' @return An object of the [contingencytables_result] class,
#' basically a subclass of [base::list()]. Use the [utils::str()] function
#' to see the specific elements returned.
#' @examples
#' Arcsine_CI_1x2(singh_2010["1st", "X"], singh_2010["1st", "n"])
#' Arcsine_CI_1x2(singh_2010["2nd", "X"], singh_2010["2nd", "n"])
#' Arcsine_CI_1x2(singh_2010["3rd", "X"], singh_2010["3rd", "n"])
#' with(singh_2010["4th", ], Arcsine_CI_1x2(X, n)) # alternative syntax
#' Arcsine_CI_1x2(ligarden_2010["X"], ligarden_2010["n"])
#' @export
Arcsine_CI_1x2 <- function(X, n, alpha = 0.05) {
  validateArguments(mget(ls()))
  # Estimate of the binomial probability (pihat)
  estimate <- X / n

  # Anscombe variance stabilizing transformation
  ptilde <- (X + 3 / 8) / (n + 3 / 4)

  # The upper alpha/2 percentile of the standard normal distribution
  z <- qnorm(1 - alpha / 2, 0, 1)

  # Calculate the confidence limits
  L <- sin(asin(sqrt(ptilde)) - z / (2 * sqrt(n)))^2
  U <- sin(asin(sqrt(ptilde)) + z / (2 * sqrt(n)))^2

  # Output
  printresults <- function() {
    cat_sprintf(
      "The arcsine CI: estimate = %6.4f (%g%% CI %6.4f to %6.4f)",
      estimate, 100 * (1 - alpha), L, U
    )
  }

  return(
    contingencytables_result(
      list(lower = L, upper = U, estimate = estimate),
      printresults
    )
  )
}
