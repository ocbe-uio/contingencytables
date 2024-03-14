#' @title The Blaker mid-P test
#' @description The Blaker mid-P test for the binomial probability (pi)
#' H_0: pi = pi0  vs  H_A: pi ~= pi0 (two-sided)
#' Described in Chapter 2 "The 1x2 Table and the Binomial Distribution"
#
#' @references Blaker H (2000) Confidence curves and improved exact
#' confidence intervals for discrete distributions. The Canadian Journal of
#' Statistics; 28:783-798
#
#' @param X the number of successes
#' @param n the total number of observations
#' @param pi0 a given probability
#' @return An object of the [contingencytables_result] class,
#' basically a subclass of [base::list()]. Use the [utils::str()] function
#' to see the specific elements returned.
#' @examples
#' Blaker_midP_test_1x2(singh_2010["1st", "X"], singh_2010["1st", "n"], pi0 = 0.513)
#' Blaker_midP_test_1x2(singh_2010["2nd", "X"], singh_2010["2nd", "n"], pi0 = 0.513)
#' Blaker_midP_test_1x2(singh_2010["3rd", "X"], singh_2010["3rd", "n"], pi0 = 0.513)
#' Blaker_midP_test_1x2(singh_2010["4th", "X"], singh_2010["4th", "n"], pi0 = 0.513)
#' Blaker_midP_test_1x2(ligarden_2010["X"], ligarden_2010["n"], pi0 = 0.5)
#' @export

Blaker_midP_test_1x2 <- function(X, n, pi0) {
  validateArguments(mget(ls()))
  # Calculate the two-sided mid-P value
  Pvalues <- dbinom(0:n, n, pi0)
  gammaobs <- min(sum(Pvalues[(X + 1):(n + 1)]), sum(Pvalues[1:(X + 1)]))
  midP <- 0
  for (k in 0:n) {
    gammak <- min(sum(Pvalues[(k + 1):(n + 1)]), sum(Pvalues[1:(k + 1)]))
    if (gammak == gammaobs) {
      midP <- midP + 0.5 * dbinom(k, n, pi0)
    } else if (gammak < gammaobs) {
      midP <- midP + dbinom(k, n, pi0)
    }
  }

  # Output
  printresults <- function() {
    cat_sprintf("The Blaker mid-P test: P = %7.5f", midP)
  }
  return(contingencytables_result(list("midP" = midP), printresults))
}
