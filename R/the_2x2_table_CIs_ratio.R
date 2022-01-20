#' @title The 2x2 table CIs ratio
#' @description Wrapper for \code{_CI_2x2} functions on Chapter 4.
#' @param n frequency matrix
#' @param alpha type I error
#' @seealso the_2x2_table_CIs_difference the_2x2_table_CIs_OR the_2x2_table_tests
#' @examples
#' # An RCT of high vs standard dose of epinephrine (Perondi et al., 2004)
#' n <- matrix(c(7, 27, 1, 33), nrow=2, byrow=TRUE)
#' the_2x2_table_CIs_ratio(n)
#'
#' # The association between CHRNA4 genotype and XFS (Ritland et al., 2007)
#' n <- matrix(c(0, 16, 15, 57), nrow=2, byrow=TRUE)
#' the_2x2_table_CIs_ratio(n)
#'
#' @export
#' @return A string of "-". This function should be called for its printed output
the_2x2_table_CIs_ratio <- function(n, alpha=0.05) {
	pi1hat <- n[1, 1] / (n[1, 1] + n[1, 2])
	pi2hat <- n[2, 1] / (n[2, 1] + n[2, 2])
	phihat <- pi1hat / pi2hat

	print(sprintf('Estimate of pi_1: %i / %i = %5.3f', n[1, 1], n[1, 1] + n[1, 2], pi1hat), quote=FALSE)
	print(sprintf('Estimate of pi_2: %i / %i = %5.3f', n[2, 1], n[2, 1] + n[2, 2], pi2hat), quote=FALSE)
	print(sprintf('Estimate of phi = pi_1 / pi_2: %5.3f', phihat), quote=FALSE)

	print(sprintf('Interval method                            %i%% CI      Log width', 100 * (1-alpha)), quote=FALSE)
	print('----------------------------------------------------------------', quote=FALSE)

	res <- Katz_log_CI_2x2(n, alpha, printresults=FALSE)
	L <- res$lower; U = res$upper
	print(sprintf('Katz log                             %6.3f to %6.3f  %7.3f', L, U, log(U) - log(L)), quote=FALSE)

	ers <- Adjusted_log_CI_2x2(n, alpha, printresults=FALSE)
	L <- res$lower; U = res$upper
	print(sprintf('Adjusted log                         %6.3f to %6.3f  %7.3f', L, U, log(U) - log(L)), quote=FALSE)

	res <- PriceBonett_approximate_Bayes_CI_2x2(n, 1.25, 2.5, alpha, printresults=FALSE)
	L <- res$lower; U = res$upper
	print(sprintf('Price-Bonett approximate Bayes       %6.3f to %6.3f  %7.3f', L, U, log(U) - log(L)), quote=FALSE)

	res <- Inv_sinh_CI_ratio_2x2(n, alpha, printresults=FALSE)
	L <- res$lower; U = res$upper
	print(sprintf('Inverse sinh                         %6.3f to %6.3f  %7.3f', L, U, log(U) - log(L)), quote=FALSE)

	res <- Adjusted_inv_sinh_CI_ratio_2x2(n, 0, 0, 0, 1, alpha, printresults=FALSE)
	L <- res$lower; U = res$upper
	print(sprintf('Adjusted inverse sinh                %6.3f to %6.3f  %7.3f', L, U, log(U) - log(L)), quote=FALSE)

	res <- MOVER_R_Wilson_CI_ratio_2x2(n, alpha, printresults=FALSE)
	L <- res$lower; U = res$upper
	print(sprintf('MOVER-R Wilson                       %6.3f to %6.3f  %7.3f', L, U, log(U) - log(L)), quote=FALSE)

	res <- MiettinenNurminen_asymptotic_score_CI_ratio_2x2(n, alpha, printresults=FALSE)
	L <- res$lower; U = res$upper
	print(sprintf('Miettinen-Nurminen asymptotic score  %6.3f to %6.3f  %7.3f', L, U, log(U) - log(L)), quote=FALSE)

	res <- Koopman_asymptotic_score_CI_2x2(n, alpha, printresults=FALSE)
	L <- res$lower; U = res$upper
	print(sprintf('Koopman asymptotic score             %6.3f to %6.3f  %7.3f', L, U, log(U) - log(L)), quote=FALSE)

	print('----------------------------------------------------------------', quote=FALSE)

}
