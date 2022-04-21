#' @title The exact binomial test for the binomial probability (pi)
#' @description H_0 pi = pi0  vs  H_A: pi ~= pi0 (two-sided)
#' @description Described in Chapter 2 "The 1x2 Table and the Binomial Distribution"
#
#' @param X the number of successes
#' @param n the total number of observations
#' @param pi0 a given probability
#' @param printresults display results (F = no, T = yes)
#' @return The two-sided, twice-the-smallest tail p-value
#' @examples
#'
#' # The number of 1st order male births (Singh et al. 2010)
#' Exact_binomial_test_1x2(X = 250, n = 533, pi0 = 0.513)
#' # The number of 2nd order male births (Singh et al. 2010)
#' Exact_binomial_test_1x2(X = 204, n = 412, pi0 = 0.513)
#' # The number of 3rd order male births (Singh et al. 2010)
#' Exact_binomial_test_1x2(X = 103, n = 167, pi0 = 0.513)
#' # The number of 4th order male births (Singh et al. 2010)
#' Exact_binomial_test_1x2(X = 33, n = 45, pi0 = 0.513)
#' # Ligarden et al. (2010)
#' Exact_binomial_test_1x2(X = 13, n = 16, pi0 = 0.5)
#'
#' @export
Exact_binomial_test_1x2 <- function(X, n, pi0, printresults = TRUE) {

  # The exact right tail P-value (for H_A: pi > pi0)
  Pright <- sum(dbinom(X:n, n, pi0))

  # The exact left tail P-value (for H_A: pi < pi0)
  Pleft <- sum(dbinom(0:X, n, pi0))

  # The two-sided twice the smallest tail P-value
  P <- 2 * min(Pright, Pleft)
  P <- min(P, 1)

  if (printresults) {
    print(sprintf("The exact binomial test: P = %7.5f", P), quote = FALSE)
  }

  invisible(P)
}
