#' @title The mid-P binomial test for the binomial probability (pi)
#' @description  The mid-P binomial test for the binomial probability (pi)
#' H_0: pi = pi0  vs  H_A: pi ~= pi0 (two-sided)
#' Described in Chapter 2 "The 1x2 Table and the Binomial Distribution"
#' @param X the number of successes
#' @param n the total number of observations
#' @param pi0 a given probability
#' @param printresults display results (0 = no, 1 = yes)
#' @examples
#' # The number of 1st order male births (Singh et al. 2010, adapted)
#' MidP_binomial_test_1x2(X=250, n=533, pi0 = .5)
#' # The number of 2nd order male births (Singh et al. 2010, adapted)
#' MidP_binomial_test_1x2(X=204, n=412, pi0 = .5)
#' # The number of 3rd order male births (Singh et al. 2010, adapted)
#' MidP_binomial_test_1x2(X=103, n=167, pi0 = .5)
#' # The number of 4th order male births (Singh et al. 2010, adapted)
#' MidP_binomial_test_1x2(X=33, n=45, pi0 = .5)
#' # Ligarden et al. (2010, adapted)
#' MidP_binomial_test_1x2(X=13, n=16, pi0 = .5)
#' @export
#' @return probability value
MidP_binomial_test_1x2 <- function(X, n, pi0, printresults=TRUE) {
	# The right tail mid-P value (for H_A: pi > pi0)
	# midPright = sum(binopdf(X:n, n, pi0));
	# midPright = midPright - 0.5 * binopdf(X, n, pi0);
	midPright = sum(dbinom(X:n, n, pi0))
	midPright = midPright - 0.5 * dbinom(X, n, pi0)

	# The left tail mid-P value (for H_A: pi < pi0)
	midPleft = sum(dbinom(0:X, n, pi0))
	midPleft = midPleft - 0.5 * dbinom(X, n, pi0)

	# The two-sided twice the smallest tail mid-P value
	midP = 2 * min(midPright, midPleft)
	midP = min(midP, 1)

	if (printresults) {
		print(sprintf('The mid-P binomial test: P = %7.5f', midP), quote=FALSE)
	}

	invisible(midP)

}
