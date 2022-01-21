#' @title The Blaker exact confidence interval
#' @description The Blaker exact confidence interval for the binomial
#' probability
#' Described in Chapter 2 "The 1x2 Table and the Binomial Distribution"
#
#' @references Blaker H (2000) Confidence curves and improved exact
#' confidence intervals for discrete distributions. The Canadian Journal of
#' Statistics; 28:783-798
#'
#' @param X the number of successes
#' @param n the total number of observations
#' @param alpha the nominal level, e.g. 0.05 for 95% CIs
#' @param printresults display results (0 = no, 1 = yes)
#' @return A vector containing lower, upper and point estimates of the statistic
#' @examples
#'
#' # The number of 1st order male births (Singh et al. 2010)
#' Blaker_exact_CI_1x2(X=250, n=533)
#' # The number of 2nd order male births (Singh et al. 2010)
#' Blaker_exact_CI_1x2(X=204, n=412)
#' # The number of 3rd order male births (Singh et al. 2010)
#' Blaker_exact_CI_1x2(X=103, n=167)
#' # The number of 4th order male births (Singh et al. 2010)
#' Blaker_exact_CI_1x2(X=33, n=45)
#' # Ligarden et al. (2010)
#' Blaker_exact_CI_1x2(X=13, n=16)
#' @export
#' @importFrom stats dbinom uniroot
Blaker_exact_CI_1x2 = function(X, n, alpha=0.05, printresults=TRUE) {
	# Estimate of the binomial probability (pihat)
	estimate = X/n

	# Define global variables that are needed in the function for the confidence limits below
	# global Xglobal nglobal alphaglobal
	# Xglobal = X
	# nglobal = n
	# alphaglobal = alpha;

	# Use Matlabs fzero function to solve the equations for the confidence limits
	# options = optimset('Display', 'off', 'TolX', tol);

	tol = 0.00000001

	# Find the lower CI limit
	if (estimate == 0) {
		L = 0
	} else {
		L = uniroot(calculate_limit_Blaker, interval=c(0,X/n), X=X, n=n, alpha=alpha, tol=tol)$root
	}

	# Find the upper CI limit
	if (estimate == 1) {
		U = 1
	} else {
		U = uniroot(calculate_limit_Blaker, interval=c(X/n,1), X=X, n=n, alpha=alpha, tol=tol)$root
	}

	if (printresults) {
		print(sprintf("The Blaker exact CI: estimate = %6.4f (%g%% CI %6.4f to %6.4f)",
			estimate, 100*(1 - alpha), L, U), quote=F)
	}

	res = c(L, U, estimate)
	names(res) = c("lower", "upper", "estimate")
	invisible(res)

}

# ===============================

calculate_limit_Blaker = function(pi0,X,n,alpha) {
	# Pvalues = binopdf(0:n, n, pi0)
	Pvalues = dbinom(0:n, n, pi0)
	# gammaobs = min([sum(Pvalues(X+1:n+1)), sum(Pvalues(1:X+1))])
	gammaobs = min(c(sum(Pvalues[(X+1):(n+1)]), sum(Pvalues[1:(X+1)])))
	T0 = 0
	for (k in 0:n) {
		# gammak = min([sum(Pvalues(k+1:n+1)), sum(Pvalues(1:k+1))]);
		gammak = min(c(sum(Pvalues[(k+1):(n+1)]), sum(Pvalues[1:(k+1)])))
		if (gammak <= gammaobs) {
			T0 = T0 + dbinom(k, n, pi0)
		}
	}
	f = T0 - alpha
	return(f)
}
