#' @title The score test for the binomial probability (pi)
#' @description The score test for the binomial probability (pi)
#' H_0: pi = pi0  vs  H_A: pi ~= pi0 (two-sided)
#' Described in Chapter 2 "The 1x2 Table and the Binomial Distribution"
#' @param X the number of successes
#' @param n the total number of observations
#' @param pi0 a given probability
#' @param printresults display results (0 = no, 1 = yes)
#' @importFrom stats pnorm
#' @examples load_chapter(2)
#' # The number of 1st order male births (Singh et al. 2010, adapted)
#' Score_test_1x2(X=250, n=533, pi0=.5)
#' # The number of 2nd order male births (Singh et al. 2010, adapted)
#' Score_test_1x2(X=204, n=412, pi0=.5)
#' # The number of 3rd order male births (Singh et al. 2010, adapted)
#' Score_test_1x2(X=103, n=167, pi0=.5)
#' # The number of 4th order male births (Singh et al. 2010, adapted)
#' Score_test_1x2(X=33, n=45, pi0=.5)
#' # Ligarden et al. (2010, adapted)
#' Score_test_1x2(X=13, n=16, pi0=.5)
#' unload_chapter(2)
Score_test_1x2 <- function(X, n, pi0, printresults=TRUE) {
	# Estimate of the binomial probability (pihat)
	estimate <- X / n

	# The standard error under the null hypothesis
	SE <- sqrt(pi0 * (1 - pi0) / n)

	# The score test statistic
	Z <- (estimate - pi0) / SE

	# The two-sided P-value (reference distribution: standard normal)
	P <- 2 * (1 - pnorm(abs(Z), 0, 1))

	if (printresults) {
		print(
			sprintf('The score test: P = %7.5f, Z = %6.3f', P, Z),
			quote=FALSE
		)
	}

	res <- c(P,Z)
	names(res) <- c("p.value", "statistic")
	invisible(res)

}
