# function [L, U, estimate] = Wald_CI_1x2(X, n, alpha, printresults)

#' @title The Wald confidence interval for the binomial probability
#' @description Described in Chapter 2 "The 1x2 Table and the Binomial Distribution"
#' @param X the number of successes
#' @param n the total number of observations
#' @param alpha the nominal level, e.g. 0.05 for 95% CIs
#' @param printresults display results (`FALSE` = no, `TRUE` = yes)
#' @importFrom stats qnorm
#' @examples
#'
#' Wald_CI_1x2(X = 250, n = 533) # The number of 1st order male births (Singh et al. 2010)
#' Wald_CI_1x2(X = 204, n = 412) # The number of 2nd order male births (Singh et al. 2010)
#' Wald_CI_1x2(X = 103, n = 167) # The number of 3rd order male births (Singh et al. 2010)
#' Wald_CI_1x2(X = 33, n = 45) # The number of 4th order male births (Singh et al. 2010)
#' Wald_CI_1x2(X = 13, n = 16) # Ligarden et al. (2010)
#' @export
#' @return A vector containing lower, upper and point estimates of the statistic
Wald_CI_1x2 <- function(X, n, alpha = 0.05, printresults = TRUE) {
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

  if (printresults) {
    print(
      sprintf(
        "The Wald CI: estimate = %6.4f (%g%% CI %6.4f to %6.4f)",
        estimate, 100 * (1 - alpha), L, U
      ),
      quote = FALSE
    )
  }

  res <- c(L, U, estimate)
  names(res) <- c("lower", "upper", "estimate")
  invisible(res)
}
