#' @title The mid-P binomial test for the binomial probability (pi)
#' @description  The mid-P binomial test for the binomial probability (pi)
#' H_0: pi = pi0  vs  H_A: pi ~= pi0 (two-sided)
#' Described in Chapter 2 "The 1x2 Table and the Binomial Distribution"
#' @param X the number of successes
#' @param n the total number of observations
#' @param pi0 a given probability
#' @examples
#' # The number of 1st order male births (Singh et al. 2010, adapted)
#' MidP_binomial_test_1x2(singh_2010["1st", "X"], singh_2010["1st", "n"], pi0 = .5)
#' # The number of 2nd order male births (Singh et al. 2010, adapted)
#' MidP_binomial_test_1x2(singh_2010["2nd", "X"], singh_2010["2nd", "n"], pi0 = .5)
#' # The number of 3rd order male births (Singh et al. 2010, adapted)
#' MidP_binomial_test_1x2(singh_2010["3rd", "X"], singh_2010["3rd", "n"], pi0 = .5)
#' # The number of 4th order male births (Singh et al. 2010, adapted)
#' MidP_binomial_test_1x2(singh_2010["4th", "X"], singh_2010["4th", "n"], pi0 = .5)
#' # Ligarden et al. (2010, adapted)
#' MidP_binomial_test_1x2(ligarden_2010["X"], ligarden_2010["n"], pi0 = .5)
#' @export
#' @return An object of the [contingencytables_result] class,
#' basically a subclass of [base::list()]. Use the [utils::str()] function
#' to see the specific elements returned.
MidP_binomial_test_1x2 <- function(X, n, pi0) {
  validateArguments(mget(ls()))

  # The right tail mid-P value (for H_A: pi > pi0)
  midPright <- sum(dbinom(X:n, n, pi0))
  midPright <- midPright - 0.5 * dbinom(X, n, pi0)

  # The left tail mid-P value (for H_A: pi < pi0)
  midPleft <- sum(dbinom(0:X, n, pi0))
  midPleft <- midPleft - 0.5 * dbinom(X, n, pi0)

  # The two-sided twice the smallest tail mid-P value
  midP <- 2 * min(midPright, midPleft)
  midP <- min(midP, 1)

  return(
    contingencytables_result(
      list("midP" = midP), sprintf("The mid-P binomial test: P = %7.5f", midP)
    )
  )
}
