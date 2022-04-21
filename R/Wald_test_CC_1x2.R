#' @title The Wald test with continuity correction for the binomial probability (pi)
#' @description The Wald test with continuity correction for the binomial probability (pi)
#' @description H_0: pi = pi0  vs  H_A: pi ~= pi0 (two-sided)
# Described in Chapter 2 "The 1x2 Table and the Binomial Distribution"
#' @param X the number of successes
#' @param n the total number of observations
#' @param pi0 a given probability
#' @param printresults display results (0 = no, 1 = yes)
#' @examples
#' # The number of 1st order male births (adapted from Singh et al. 2010)
#' Wald_test_CC_1x2(X = 250, n = 533, pi0 = 0.1)
#' # The number of 2nd order male births (adapted from Singh et al. 2010)
#' Wald_test_CC_1x2(X = 204, n = 412, pi0 = 0.1)
#' # The number of 3rd order male births (adapted from Singh et al. 2010)
#' Wald_test_CC_1x2(X = 103, n = 167, pi0 = 0.1)
#' # The number of 4th order male births (adapted from Singh et al. 2010)
#' Wald_test_CC_1x2(X = 33, n = 45, pi0 = 0.1)
#' # Ligarden et al. (2010)
#' Wald_test_CC_1x2(X = 13, n = 16, pi0 = 0.1)
#' @export
#' @return A vector containing the two-sided p-value and the Wald test statistic
Wald_test_CC_1x2 <- function(X, n, pi0, printresults = TRUE) {
  # Estimate of the binomial probability (pihat)
  estimate <- X / n

  # The standard error of the estimate
  SE <- sqrt(estimate * (1 - estimate) / n)

  # The Wald test statistic with continuity correction
  Z <- (abs(estimate - pi0) - 1 / (2 * n)) / SE

  # When the SE is zero, set Z = 0
  if (X == 0 || X == n) {
    Z <- 0
  }

  # The two-sided P-value (reference distribution: standard normal)
  P <- 2 * (1 - pnorm(abs(Z), 0, 1))

  if (printresults) {
    print(
      sprintf(
        "The Wald test with continuity correction: P = %7.5f, Z = %5.3f", P, Z
      ),
      quote = FALSE
    )
  }

  res <- c(P, Z)
  names(res) <- c("p.value", "statistic")
  invisible(res)
}
