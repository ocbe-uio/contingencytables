#' @title The likelihood ratio test for the binomial probability (pi)
#' @description The likelihood ratio test for the binomial probability (pi)
#' H_0: pi = pi0  vs  H_A: pi ~= pi0 (two-sided).
#' Described in Chapter 2 "The 1x2 Table and the Binomial Distribution".
#' @param X the number of successes
#' @param n the total number of observations
#' @param pi0 a given probability
#' @importFrom stats pchisq
#' @examples
#' LR_test_1x2(singh_2010["1st", "X"], singh_2010["1st", "n"], pi0 = .5)
#' LR_test_1x2(singh_2010["2nd", "X"], singh_2010["2nd", "n"], pi0 = .5)
#' LR_test_1x2(singh_2010["3rd", "X"], singh_2010["3rd", "n"], pi0 = .5)
#' LR_test_1x2(singh_2010["4th", "X"], singh_2010["4th", "n"], pi0 = .5)
#' LR_test_1x2(ligarden_2010["X"], ligarden_2010["n"], pi0 = .5)
#' @export
#' @return An object of the [contingencytables_result] class,
#' basically a subclass of [base::list()]. Use the [utils::str()] function
#' to see the specific elements returned.
LR_test_1x2 <- function(X, n, pi0) {
  validateArguments(mget(ls()))

  # Estimate of the binomial probability (pihat)
  estimate <- X / n

  # The likelihood ratio test statistic
  T0 <- 2 * (X * log(estimate / pi0) + (n - X) * log((1 - estimate) / (1 - pi0)))

  # When the test statistic is uncomputable, set T0 <- 0
  if (X == 0 || X == n) {
    T0 <- 0
  }

  # The two-sided P-value (reference distribution: chi-squared with 1 degree
  # of freedom)
  df <- 1
  P <- 1 - pchisq(T0, df)

  return(
    contingencytables_result(
      list("Pvalue" = P, "T" = T0, "df" = df),
      sprintf("The likelihood ratio test: P = %7.5f, T = %5.3f (df = %i)", P, T0, df)
    )
  )
}
