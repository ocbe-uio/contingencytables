#' @title The Blaker exact test
#' @description The Blaker exact test for the binomial probability (pi)
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
#' @param printresults display results (0 = no, 1 = yes)
#' @return The two-sided p-value
#' @examples
#'
#' # The number of 1st order male births (Singh et al. 2010)
#' Blaker_exact_test_1x2(X=250, n=533, pi0=0.513)
#' # The number of 2nd order male births (Singh et al. 2010)
#' Blaker_exact_test_1x2(X=204, n=412, pi0=0.513)
#' # The number of 3rd order male births (Singh et al. 2010)
#' Blaker_exact_test_1x2(X=103, n=167, pi0=0.513)
#' # The number of 4th order male births (Singh et al. 2010)
#' Blaker_exact_test_1x2(X=33, n=45, pi0=0.513)
#' # Ligarden et al. (2010)
#' Blaker_exact_test_1x2(X=13, n=16, pi0=0.5)
#' @export
Blaker_exact_test_1x2 <- function(X, n, pi0, printresults=TRUE) {
	# Calculate the two-sided P-value
	Pvalues = dbinom(0:n, n, pi0)
	gammaobs = min(c(sum(Pvalues[(X+1):(n+1)]), sum(Pvalues[1:(X+1)])))
	P = 0
	for (k in 0:n) {
		gammak = min(c(sum(Pvalues[(k+1):(n+1)]), sum(Pvalues[1:(k+1)])))
		if (gammak <= gammaobs) {
			P = P + dbinom(k, n, pi0)
		}
	}

	if (printresults) {
		print(
			sprintf('The Blaker exact test: P = %7.5f', P),
			quote=FALSE
		)
	}

	invisible(P)
}
