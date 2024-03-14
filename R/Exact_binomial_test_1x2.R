#' @title The exact binomial test for the binomial probability (pi)
#' @description H_0 pi = pi0  vs  H_A: pi ~= pi0 (two-sided)
#' @description Described in Chapter 2 "The 1x2 Table and the Binomial Distribution"
#
#' @param X the number of successes
#' @param n the total number of observations
#' @param pi0 a given probability
#' @return An object of the [contingencytables_result] class,
#' basically a subclass of [base::list()]. Use the [utils::str()] function
#' to see the specific elements returned.
#' @examples
#' Exact_binomial_test_1x2(singh_2010["1st", "X"], singh_2010["1st", "n"], pi0 = 0.513)
#' Exact_binomial_test_1x2(singh_2010["2nd", "X"], singh_2010["2nd", "n"], pi0 = 0.513)
#' Exact_binomial_test_1x2(singh_2010["3rd", "X"], singh_2010["3rd", "n"], pi0 = 0.513)
#' Exact_binomial_test_1x2(singh_2010["4th", "X"], singh_2010["4th", "n"], pi0 = 0.513)
#' Exact_binomial_test_1x2(ligarden_2010["X"], ligarden_2010["n"], pi0 = 0.5)
#' @export
Exact_binomial_test_1x2 <- function(X, n, pi0) {
  validateArguments(mget(ls()))

  # The exact right tail P-value (for H_A: pi > pi0)
  Pright <- sum(dbinom(X:n, n, pi0))

  # The exact left tail P-value (for H_A: pi < pi0)
  Pleft <- sum(dbinom(0:X, n, pi0))

  # The two-sided twice the smallest tail P-value
  P <- 2 * min(Pright, Pleft)
  P <- min(P, 1)

  # Output
  printresults <- function() {
    cat_sprintf("The exact binomial test: P = %7.5f", P)
  }
  return(contingencytables_result(list("Pvalue" = P), printresults))
}
