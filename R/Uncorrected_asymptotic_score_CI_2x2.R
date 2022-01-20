#' @title The uncorrected asymptotic score confidence interval for the odds ratio
#' @description The uncorrected asymptotic score confidence interval for the odds ratio
#' @description Described in Chapter 4 "The 2x2 Table"
#' @param n the observed counts (a 2x2 matrix)
#' @param alpha the nominal level, e.g. 0.05 for 95% CIs
#' @param printresults display results (FALSE = no, TRUE = yes)
#' @examples
#' # A case-control study of GADA exposure on IPEX syndrome (Lampasona et al., 2013):
#' n <- matrix(c(9,4,4,10), nrow=2, byrow=TRUE)
#' Uncorrected_asymptotic_score_CI_2x2(n)
#'
#' # The association between CHRNA4 genotype and XFS (Ritland et al., 2007):
#' n <- matrix(c(0,16,15,57), nrow=2, byrow=TRUE)
#' Uncorrected_asymptotic_score_CI_2x2(n)
#'
#'
#' @export
#' @return A data frame containing lower, upper and point estimates of the statistic
Uncorrected_asymptotic_score_CI_2x2 <- function(n, alpha=0.05, printresults=TRUE) {
	# global n11 n21 n1p n2p alphaglobal limit

	n11 <- n[1, 1]
	n21 <- n[2, 1]
	n1p <- n[1, 1] + n[1, 2]
	n2p <- n[2, 1] + n[2, 2]

	# Estimate of the odds ratio (thetahat)
	estimate <- n[1, 1] * n[2, 2] / (n[1, 2] * n[2, 1])

	# Options for Matlab's fzero command
	tol <- 0.0000001
	theta0 <- 0.00001
	theta1 <- 100000

	# Lower CI limit
	# limit = 'lower'
	if (is.na(estimate) || estimate==Inf) {
		L <- uniroot(calculate_limit_lower.Uncorrected, c(theta0, theta1), n11=n11, n21=n21, n1p=n1p,
			n2p=n2p, alpha=alpha, tol=tol)$root
	} else if (estimate == 0) {
		L <- 0
		# exitflag = 1
	} else {
		L <- uniroot(calculate_limit_lower.Uncorrected, c(theta0, estimate), n11=n11, n21=n21, n1p=n1p,
			n2p=n2p, alpha=alpha, tol=tol)$root
	}
	# if exitflag ~= 1, display_warning(exitflag), }

	# Upper CI limit
	# limit = 'upper'
	if (n[2, 1] == 0 || n[1, 2] == 0) {
		U <- Inf
		# exitflag = 1
	} else if (estimate == 0) {
		U <- uniroot(calculate_limit_upper.Uncorrected, c(theta0, theta1), n11=n11, n21=n21, n1p=n1p,
			n2p=n2p, alpha=alpha, tol=tol)$root
	} else {
		U <- uniroot(calculate_limit_upper.Uncorrected, c(estimate, theta1), n11=n11, n21=n21, n1p=n1p,
			n2p=n2p, alpha=alpha, tol=tol)$root
	}
	# if exitflag ~= 1, display_warning(exitflag), }

	if (printresults) {
		print(sprintf('Uncorrected asymptotic score CI: estimate = %6.4f (%g%% CI %6.4f to %6.4f)',
			estimate, 100 * (1 - alpha), L, U), quote=FALSE)
	}

	res <- data.frame(lower=L, upper=U, estimate=estimate)
	invisible(res)

}
