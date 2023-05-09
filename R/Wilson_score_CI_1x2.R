#' @title The Wilson score confidence interval
#' @description The Wilson score confidence interval
# Described in Chapter 2 "The 1x2 Table and the Binomial Distribution"
#' @references Reference Wilson EB (1927) Probable inference, the law of
#' succession, and statistical inference. Journal of the American Statistical
#' Association 22209-212
#' @param X the number of successes
#' @param n the total number of observations
#' @param alpha the nominal level, e.g. 0.05 for 95% CIs
#' @examples
#' # birth order 1, Singh et al. (2010)
#' Wilson_score_CI_1x2(singh_2010["1st", "X"], singh_2010["1st", "n"])
#' # birth order 2, Singh et al. (2010)
#' Wilson_score_CI_1x2(singh_2010["2nd", "X"], singh_2010["2nd", "n"])
#' # birth order 3, Singh et al. (2010)
#' Wilson_score_CI_1x2(singh_2010["3rd", "X"], singh_2010["3rd", "n"])
#' # birth order 4, Singh et al. (2010)
#' with(singh_2010["4th", ], Wilson_score_CI_1x2(X, n)) # alternative syntax
#' # Ligarden (2010)
#' Wilson_score_CI_1x2(ligarden_2010["X"], ligarden_2010["n"])
#' @export
#' @return An object of the [contingencytables_result] class,
#' basically a subclass of [base::list()]. Use the [utils::str()] function
#' to see the specific elements returned.
Wilson_score_CI_1x2 <- function(X, n, alpha = 0.05) {
  validateArguments(mget(ls()))

  # Estimate of the binomial parameter
  estimate <- X / n

  # The upper alpha / 2 percentile of the standard normal distribution
  z <- qnorm(1 - alpha / 2, 0, 1)

  # Calculate the confidence limits
  A <- (2 * n * estimate + z^2) / (2 * n + 2 * z^2)
  B <- (z * sqrt(z^2 + 4 * n * estimate * (1 - estimate))) / (2 * n + 2 * z^2)
  L <- A - B
  U <- A + B

  printresults <- function() {
    paste(
      "The Wilson score CI: estimate = ", round(estimate, 4), " (",
      100 * (1 - alpha), "% CI ", round(L, 4), " to ", round(U, 4), ")",
      sep = ""
    )
  }

  res <- list(L, U, estimate)
  names(res) <- c("lower", "upper", "estimate")
  return(contingencytables_result(res, printresults))
}
