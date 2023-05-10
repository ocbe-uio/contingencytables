#' @title The score test with continuity correction for the
#' @description The score test with continuity correction for the
#' binomial probability (pi).
#' H_0: pi = pi0  vs  H_A: pi ~= pi0 (two-sided).
#' Described in Chapter 2 "The 1x2 Table and the Binomial Distribution"
#' @param X the number of successes
#' @param n the total number of observations
#' @param pi0 a given probability
#' @examples
#' # The number of 1st order male births (Singh et al. 2010, adapted)
#' Score_test_CC_1x2(singh_2010["1st", "X"], singh_2010["1st", "n"], pi0 = .5)
#' # The number of 2nd order male births (Singh et al. 2010, adapted)
#' Score_test_CC_1x2(singh_2010["2nd", "X"], singh_2010["2nd", "n"], pi0 = .5)
#' # The number of 3rd order male births (Singh et al. 2010, adapted)
#' Score_test_CC_1x2(singh_2010["3rd", "X"], singh_2010["3rd", "n"], pi0 = .5)
#' # The number of 4th order male births (Singh et al. 2010, adapted)
#' Score_test_CC_1x2(singh_2010["4th", "X"], singh_2010["4th", "n"], pi0 = .5)
#' # Ligarden et al. (2010, adapted)
#' Score_test_CC_1x2(ligarden_2010["X"], ligarden_2010["n"], pi0 = .5)
#' @export
#' @return An object of the [contingencytables_result] class,
#' basically a subclass of [base::list()]. Use the [utils::str()] function
#' to see the specific elements returned.
Score_test_CC_1x2 <- function(X, n, pi0) {
  validateArguments(mget(ls()))

  # Estimate of the binomial probability (pihat)
  estimate <- X / n

  # The standard error under the null hypothesis
  SE <- sqrt(pi0 * (1 - pi0) / n)

  # The score test statistic with continuity correction
  Z <- (abs(estimate - pi0) - 1 / (2 * n)) / SE

  # The two-sided P-value (reference distribution: standard normal)
  P <- 2 * (1 - pnorm(abs(Z), 0, 1))

  return(
    contingencytables_result(
      list("p.value" = P, "statistic" = Z),
      sprintf("The score test with continuity correction: P = %7.5f, Z = %5.3f", P, Z)
    )
  )
}
