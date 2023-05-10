#' @title The Wilson score confidence interval with continuity correction for
#' the binomial probability
#' @description Described in Chapter 2 "The 1x2 Table and the Binomial
#' Distribution"
#' @references Reference Wilson EB (1927) Probable inference, the law of
#' succession, and statistical inference. Journal of the American Statistical
#' Association; 22209-212
#' @param X the number of successes
#' @param n the total number of observations
#' @param alpha the nominal level, e.g. 0.05 for 95% CIs
#' @examples
#' # The number of 1st order male births (Singh et al. 2010)
#' Wilson_score_CI_CC_1x2(singh_2010["1st", "X"], singh_2010["1st", "n"])
#' # The number of 2nd order male births (Singh et al. 2010)
#' Wilson_score_CI_CC_1x2(singh_2010["2nd", "X"], singh_2010["2nd", "n"])
#' # The number of 3rd order male births (Singh et al. 2010)
#' Wilson_score_CI_CC_1x2(singh_2010["3rd", "X"], singh_2010["3rd", "n"])
#' # The number of 4th order male births (Singh et al. 2010)
#' with(singh_2010["4th", ], Wilson_score_CI_CC_1x2(X, n)) # alternative syntax
#' # Ligarden et al. (2010)
#' Wilson_score_CI_CC_1x2(ligarden_2010["X"], ligarden_2010["n"])
#' @export
#' @return An object of the [contingencytables_result] class,
#' basically a subclass of [base::list()]. Use the [utils::str()] function
#' to see the specific elements returned.
Wilson_score_CI_CC_1x2 <- function(X, n, alpha = 0.05) {
  validateArguments(mget(ls()))

  # Estimate of the binomial probability (pihat)
  estimate <- X / n

  # The upper alpha / 2 percentile of the standard normal distribution
  z <- qnorm(1 - alpha / 2, 0, 1)

  # Calculate the confidence limits
  L <- (2 * n * estimate + z^2 - 1 - z * sqrt(z^2 - 2 - 1 / n +
    4 * estimate * n * (1 - estimate) + 4 * estimate)) / (2 * (n + z^2))
  U <- (2 * n * estimate + z^2 + 1 + z * sqrt(z^2 + 2 - 1 / n +
    4 * estimate * n * (1 - estimate) - 4 * estimate)) / (2 * (n + z^2))

  printresults <- function() {
    sprintf(
      "The Wilson score CI: estimate = %6.4f (%g%% CI %6.4f to %6.4f)",
      estimate, 100 * (1 - alpha), L, U
    )
  }

  res <- list(L, U, estimate)
  names(res) <- c("lower", "upper", "estimate")
  return(contingencytables_result(res, printresults))
}
