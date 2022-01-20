#' @title The Agresti-Coull confidence interval for the binomial probability
#' @description Described in Chapter 2 "The 1x2 Table and the Binomial
#' Distribution"
#' @references Agresti A, Coull BA (1998) Approximate is better than "exact"
#' for interval estimation of binomial proportions. The American
#' Statistician; 52:119-126
#' @seealso Wald_CI_1x2
#' @param X the number of successes
#' @param n the total number of observations
#' @param alpha the nominal level, e.g. 0.05 for 95% CIs
#' @param printresults display results (0 = no, 1 = yes)
#' @return A vector containing lower, upper and point estimates of the statistic
#' @examples
#'
#' # The number of 1st order male births (Singh et al. 2010)
#' AgrestiCoull_CI_1x2(X = 250, n = 533)
#' # The number of 2nd order male births (Singh et al. 2010)
#' AgrestiCoull_CI_1x2(X = 204, n = 412)
#' # The number of 3rd order male births (Singh et al. 2010)
#' AgrestiCoull_CI_1x2(X = 103, n = 167)
#' # Example: The number of 4th order male births (Singh et al. 2010)
#' AgrestiCoull_CI_1x2(X = 33, n = 45)
#' # Example: Ligarden et al. (2010)
#' AgrestiCoull_CI_1x2(X = 13, n = 16)
#' @export
AgrestiCoull_CI_1x2 = function(X, n, alpha=0.05, printresults=TRUE) {
	# Estimate of the binomial probability (pihat)
	estimate = X/n

	# Add two successes and two failures and calculate the Wald CI
	res = Wald_CI_1x2(X + 2, n + 4, alpha, 0)
	estimate = res[3]
	L = res[1]
	U = res[2]

	if (printresults) {
		print(
			sprintf(
				'The Agresti-Coull CI: estimate = %6.4f (%g%% CI %6.4f to %6.4f)',
				estimate, 100*(1 - alpha), L, U
			),
			quote=FALSE
		)
	}

	invisible(res)

}
