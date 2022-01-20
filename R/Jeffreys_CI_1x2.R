#' @title Jeffreys confidence interval for the binomial probability
#' @description Jeffreys confidence interval for the binomial probability
#' @description Described in Chapter 2 "The 1x2 Table and the Binomial Distribution"
#' @param X the number of successes
#' @param n the total number of observations
#' @param alpha the nominal level, e.g. 0.05 for 95% CIs
#' @param printresults display results (0 = no, 1 <- yes)
#' @importFrom stats qbeta
#' @examples
#'
#' # The number of 1st order male births (Singh et al. 2010)
#' Jeffreys_CI_1x2(X=250, n=533)
#' # The number of 2nd order male births (Singh et al. 2010)
#' Jeffreys_CI_1x2(X=204, n=412)
#' # The number of 3rd order male births (Singh et al. 2010)
#' Jeffreys_CI_1x2(X=103, n=167)
#' # The number of 4th order male births (Singh et al. 2010)
#' Jeffreys_CI_1x2(X=33, n=45)
#' # Ligarden et al. (2010)
#' Jeffreys_CI_1x2(X=13, n=16)
#' @export
#' @return A vector containing lower, upper and point estimates of the statistic
Jeffreys_CI_1x2 <- function(X, n, alpha=0.05, printresults=TRUE) {

	# Estimate of the binomial probability (pihat)
	estimate <- X / n

	# Calculate the confidence limits with Jeffreys noninformative prior, B(0.5, 0.5)
	# L <- betainv(alpha / 2, X + 0.5, n - X + 0.5);
	# U <- betainv(1 - alpha / 2, X + 0.5, n - X + 0.5);
	L <- qbeta(alpha / 2, X + 0.5, n - X + 0.5)
	U <- qbeta(1 - alpha / 2, X + 0.5, n - X + 0.5)

	if (printresults) {
		print(
			sprintf(
				'The Jeffreys CI: estimate = %6.4f (%g%% CI %6.4f to %6.4f)',
				estimate, 100 * (1 - alpha), L, U
			)
		)
	}

	res <- c(L, U, estimate)
	names(res) <- c("lower", "upper", "estimate")
	invisible(res)
}
