#' @title The Newcombe hybrid score confidence interval for the difference between probabilities
#' @description The Newcombe hybrid score confidence interval for the difference between probabilities
#' @description Described in Chapter 4 "The 2x2 Table"
#' @param n the observed counts (a 2x2 matrix)
#' @param alpha the nominal level, e.g. 0.05 for 95% CIs
#' @param printresults display results (FALSE = no, TRUE = yes)
#' @examples
#' # An RCT of high vs standard dose of epinephrine (Perondi et al., 2004)
#' n <- matrix(c(7, 27, 1, 33), nrow=2, byrow=TRUE)
#' Newcombe_hybrid_score_CI_2x2(n)
#'
#' # The association between CHRNA4 genotype and XFS (Ritland et al., 2007)
#' n <- matrix(c(0, 16, 15, 57), nrow=2, byrow=TRUE)
#' Newcombe_hybrid_score_CI_2x2(n)
#'
#' @export
#' @return A data frame containing lower, upper and point estimates of the statistic
Newcombe_hybrid_score_CI_2x2 <- function(n, alpha=0.05, printresults=TRUE) {
	n1p <- n[1, 1] + n[1, 2]
	n2p <- n[2, 1] + n[2, 2]

	# Estimates of the two probabilities of success
	pi1hat <- n[1, 1] / n1p
	pi2hat <- n[2, 1] / n2p

	# Estimate of the difference between probabilities (deltahat)
	estimate <- pi1hat - pi2hat

	# Use Wilson score CIs for the two probabilities of success
	res1 <- Wilson_score_CI_1x2(n[1, 1], n1p, alpha, printresults=FALSE)
	res2 <- Wilson_score_CI_1x2(n[2, 1], n2p, alpha, printresults=FALSE)
	L <- estimate - sqrt((pi1hat-res1["lower"]) ^ 2 + (res2["upper"]-pi2hat) ^ 2)
	U <- estimate + sqrt((pi2hat-res2["lower"]) ^ 2 + (res1["upper"]-pi1hat) ^ 2)

	# Fix overshoot by truncation
	L <- max(-1, L)
	U <- min(U, 1)

	if (printresults) {
		print(sprintf('The Newcombe hybrid score CI: estimate = %6.4f (%g%% CI %6.4f to %6.4f)',
			estimate, 100 * (1 - alpha), L, U), quote=FALSE)
	}

	res <- data.frame(lower=L, upper=U, estimate=estimate)
	invisible(res)

}
