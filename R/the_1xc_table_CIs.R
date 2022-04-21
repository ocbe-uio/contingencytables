#' @title The 1xc table CIs
#' @param n the observed counts (a 1xc vector, where c is the number of categories)
#' @param alpha the nominal level, e.g. 0.05 for 95# CIs
#' @examples
#' # Genotype counts for SNP rs 6498169 in RA patients
#' the_1xc_table_CIs(n=c(276, 380, 118))
#' @export
#' @return A data frame containing lower, upper and point estimates of the statistic
the_1xc_table_CIs <- function(n, alpha=0.05) {

	# ======================================================== #
	# Ad-hoc function to print output                          #
	# ======================================================== #
	myprint <- function(txt, ...) cat(sprintf(txt, ...), "\n")

	# ======================================================== #
	# Output                                                   #
	# ======================================================== #

	c0 <- length(n)

	myprint('Interval method                 Simultaneous CIs     width')
	myprint('----------------------------------------------------------')

	res1 <- Gold_Wald_CIs_1xc(n, alpha, printresults=FALSE)
	res2 <- Goodman_Wald_CIs_1xc(n, alpha, printresults=FALSE)
	res3 <- QuesenberryHurst_Wilson_score_CIs_1xc(n, alpha, printresults=FALSE)
	res4 <- Goodman_Wilson_score_CIs_1xc(n, alpha, printresults=FALSE)
	for (i in 1:c0) {
		myprint('Estimate of pi_%i: %6.4f', i, res1$estimate[i])
		myprint(
			'Gold Wald                       %6.4f to %6.4f    %6.4f',
			res1$lower[i], res1$upper[i], res1$upper[i]-res1$lower[i]
		)
		myprint(
			'Goodman Wald                    %6.4f to %6.4f    %6.4f',
			res2$lower[i], res2$upper[i], res2$upper[i]-res2$lower[i]
		)
		myprint(
			'Quesenberry-Hurst Wilson score  %6.4f to %6.4f    %6.4f',
			res3$lower[i], res3$upper[i], res3$upper[i]-res3$lower[i]
		)
		myprint(
			'Goodman Wilson score            %6.4f to %6.4f    %6.4f',
			res4$lower[i], res4$upper[i], res4$upper[i]-res4$lower[i]
		)
	}
	myprint('----------------------------------------------------------')

	Goodman_Wald_CIs_for_diffs_1xc(n, alpha, 'Bonferroni')

}
