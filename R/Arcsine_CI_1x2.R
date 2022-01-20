#' @title Arcsine confidence interval
#' @description The Arcsine confidence interval for the binomial probability
#' (with Anscombe variance stabilizing transformation)
#' Described in Chapter 2 "The 1x2 Table and the Binomial Distribution"
#' @references Anscombe FJ (1948) The transformation of Poisson, binomial and
#' negative binomial data. Biometrika; 35:246-254
#'
#' @param X the number of successes
#' @param n the total number of observations
#' @param alpha the nominal level, e.g. 0.05 for 95% CIs
#' @param printresults display results (0 = no, 1 = yes)
#' @return A vector containing lower, upper and point estimates of the statistic
#' @examples
#' # The number of 1st order male births (Singh et al. 2010)
#' Arcsine_CI_1x2(X = 250, n = 533)
#' # The number of 2nd order male births (Singh et al. 2010)
#' Arcsine_CI_1x2(X = 204, n = 412)
#' # The number of 3rd order male births (Singh et al. 2010)
#' Arcsine_CI_1x2(X = 103, n = 167)
#' # The number of 4th order male births (Singh et al. 2010)
#' Arcsine_CI_1x2(X = 33, n = 45)
#' # Ligarden et al. (2010)
#' Arcsine_CI_1x2(X = 13, n = 16)
#' @export
Arcsine_CI_1x2 = function(X, n, alpha=0.05, printresults=TRUE) {
	# Estimate of the binomial probability (pihat)
	estimate = X/n

	# Anscombe variance stabilizing transformation
	ptilde = (X + 3/8)/(n + 3/4)

	# The upper alpha/2 percentile of the standard normal distribution
	# z = norminv(1 - alpha/2, 0, 1)
	z = qnorm(1-alpha/2, 0, 1)

	# Calculate the confidence limits
	L = sin(asin(sqrt(ptilde)) - z/(2*sqrt(n)))^2
	U = sin(asin(sqrt(ptilde)) + z/(2*sqrt(n)))^2

	if (printresults) {
		print(
			sprintf(
				'The arcsine CI: estimate = %6.4f (%g%% CI %6.4f to %6.4f)',
				estimate, 100*(1 - alpha), L, U
			),
		quote=FALSE)
	}

	res = c(L, U, estimate)
	names(res) = c("lower", "upper", "estimate")
	invisible(res)
}
