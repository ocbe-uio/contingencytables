#' @title The Wilson score confidence interval
#' @description The Wilson score confidence interval
# Described in Chapter 2 "The 1x2 Table and the Binomial Distribution"
#' @references Reference Wilson EB (1927) Probable inference, the law of succession, and statistical inference. Journal of the American Statistical Association 22209-212
#' @param X the number of successes
#' @param n the total number of observations
#' @param alpha the nominal level, e.g. 0.05 for 95% CIs
#' @param printresults display results (F = no, T = yes)
#' @examples
#' # birth order 1, Singh et al. (2010)
#' Wilson_score_CI_1x2(X = 250, n = 533)
#' # birth order 2, Singh et al. (2010)
#' Wilson_score_CI_1x2(X = 204, n = 412)
#' # birth order 3, Singh et al. (2010)
#' Wilson_score_CI_1x2(X = 103, n = 167)
#' # birth order 4, Singh et al. (2010)
#' Wilson_score_CI_1x2(X = 33, n = 45)
#' # Ligarden (2010)
#' Wilson_score_CI_1x2(X = 13, n = 16)
#' @export
#' @return A vector containing lower, upper and point estimates of the statistic
Wilson_score_CI_1x2 <- function(X, n, alpha = 0.05, printresults = TRUE) {

  # Estimate of the binomial parameter
  estimate <- X / n

  # The upper alpha / 2 percentile of the standard normal distribution
  z <- qnorm(1 - alpha / 2, 0, 1)

  # Calculate the confidence limits
  A <- (2 * n * estimate + z^2) / (2 * n + 2 * z^2)
  B <- (z * sqrt(z^2 + 4 * n * estimate * (1 - estimate))) / (2 * n + 2 * z^2)
  L <- A - B
  U <- A + B

  if (printresults) {
    print(
      paste(
        "The Wilson score CI: estimate = ", round(estimate, 4), " (",
        100 * (1 - alpha), "% CI ", round(L, 4), " to ", round(U, 4), ")",
        sep = ""
      ),
      quote = FALSE
    )
  }

  res <- c(L, U, estimate)
  names(res) <- c("lower", "upper", "estimate")
  invisible(res)
}
