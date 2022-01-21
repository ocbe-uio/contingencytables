#' @title The MOVER-R Wilson confidence interval for the odds ratio
#' @description The MOVER-R Wilson confidence interval for the odds ratio
#' @description Described in Chapter 4 "The 2x2 Table"
#' @param n the observed counts (a 2x2 matrix)
#' @param alpha the nominal level, e.g. 0.05 for 95% CIs
#' @param printresults display results (F = no, 1T= yes)
#' @examples
#' # A case-control study of GADA exposure on IPEX syndrome (Lampasona et al., 2013):
#' n <- matrix(c(9,4,4,10), nrow=2, byrow=TRUE)
#' MOVER_R_Wilson_CI_OR_2x2(n)
#'
#' # The association between CHRNA4 genotype and XFS (Ritland et al., 2007):
#' n <- matrix(c(0,16,15,57), nrow=2, byrow=TRUE)
#' MOVER_R_Wilson_CI_OR_2x2(n)
#'
#' @export
#' @return A data frame containing lower, upper and point estimates of the statistic
MOVER_R_Wilson_CI_OR_2x2 <- function(n, alpha=0.05, printresults=TRUE) {

	# If n_22 = 0, use an equivalent case, which gives a finite upper limit
	if (n[2, 2] == 0) {
		n <- matrix(c(0, n[2, 1], n[1, 2], n[1, 1]), nrow=2, byrow=TRUE)
	}

	n1p <- n[1, 1] + n[1, 2]
	n2p <- n[2, 1] + n[2, 2]

	# Estimates of the two probabilities of success
	pi1hat <- n[1, 1] / n1p
	pi2hat <- n[2, 1] / n2p

	# Estimate of the odds ratio (thetahat)
	estimate <- n[1, 1] * n[2, 2] / (n[1, 2] * n[2, 1])

	# Use Wilson score CIs for the two probabilities of success
	res1 <- Wilson_score_CI_1x2(n[1, 1], n1p, alpha, printresults=F)
	res2 <- Wilson_score_CI_1x2(n[2, 1], n2p, alpha, printresults=F)

	# The estimated logits of pi_1 and pi_2 and their CIs
	q1hat <- pi1hat / (1 - pi1hat)
	q2hat <- pi2hat / (1 - pi2hat)
	L1 <- res1["lower"] / (1 - res1["lower"])
	L2 <- res2["lower"] / (1 - res2["lower"])
	U1 <- res1["upper"] / (1 - res1["upper"])
	U2 <- res2["upper"] / (1 - res2["upper"])

	# Calculate the confidence limits
	L <- (q1hat * q2hat - sqrt((q1hat * q2hat) ^ 2 -
		res1["lower"] * res2["upper"] * (2 * q1hat - res1["lower"]) * (2 * q2hat - res2["upper"])))/
		(res2["upper"] * (2 * q2hat - res2["upper"]))
	U <- (q1hat * q2hat + sqrt((q1hat * q2hat) ^ 2 -
		res1["upper"] * res2["lower"] * (2 * q1hat - res1["upper"]) * (2 * q2hat - res2["lower"])))/
		(res2["lower"] * (2 * q2hat - res2["lower"]))

	# In case of n_12 = 0, let L = 1 / U_tmp, where U_tmp comes from the CI for 0 / n1+ vs n22 / n2+
	if (n[1, 2] == 0) {
		U_tmp <- MOVER_R_Wilson_CI_OR_2x2(matrix(c(0,n1p,n[2, 2],n[2, 1]),nrow=2,byrow=TRUE), alpha, printresults=F)$upper
		L <- 1 / U_tmp
	}

	# Fix limits for some special cases
	# if isnan(L) || isreal(L) == 0,
	if (is.na(L)) {
		L <- 0
	}
	# if isnan(U) || isreal(U) == 0 || U < 0,
	if (is.na(U) || U < 0) {
		U <- Inf
	}
	L <- max(c(0, L))

	if (printresults) {
		print(sprintf('The MOVER-R Wilson CI: estimate = %6.4f (%g%% CI %6.4f to %6.4f)',
			estimate, 100 * (1 - alpha), L, U), quote=FALSE)
	}

	res <- data.frame(lower=L, upper=U, estimate=estimate)
	invisible(res)
}
