#' @title The Agresti-Caffo confidence interval for the difference between probabilities
#' @description The Agresti-Caffo confidence interval for the difference between probabilities
#' @description Described in Chapter 4 "The 2x2 Table"
#' @param n the observed counts (a 2x2 matrix)
#' @param alpha the nominal level, e.g. 0.05 for 95% CIs
#' @param printresults display results (F = no, T = yes)
#' @return A data frame containing lower, upper and point estimates of the statistic
#' @examples
#' # An RCT of high vs standard dose of epinephrine (Perondi et al., 2004):
#' AgrestiCaffo_CI_2x2(n = matrix(c(7,27,1,33), nrow=2, byrow=TRUE))
#' # The association between CHRNA4 genotype and XFS (Ritland et al., 2007):
#' AgrestiCaffo_CI_2x2(n = matrix(c(0,16,15,57), nrow=2, byrow=TRUE))
#' @export
AgrestiCaffo_CI_2x2 <- function(n, alpha=0.05, printresults=TRUE) {
	# Estimate of the difference between probabilities (deltahat)
	estimate <- n[1, 1] / (n[1, 1] + n[1, 2]) - n[2, 1] / (n[2, 1] + n[2, 2])

	# Add one success and one failure in each group and calculate the Wald CI
	res.wald = Wald_CI_2x2(n + 1, alpha, printresults=FALSE)

	if (printresults) {
		print(
			sprintf(
				'The Agresti-Caffo CI: estimate = %6.4f (%g%% CI %6.4f to %6.4f)',
				estimate, 100 * (1 - alpha), res.wald$lower, res.wald$upper
			),
			quote=FALSE
		)
	}

	res <- data.frame(
		lower=res.wald$lower, upper=res.wald$upper, estimate=estimate
	)
	invisible(res)

}
