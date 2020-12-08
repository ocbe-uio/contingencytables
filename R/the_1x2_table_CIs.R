#' @title The 1x2 Table CIs
#' @examples load_chapter()
#' # The number of 1st order male births (Singh et al. 2010)
#' the_1x2_table_CIs(X=250, n=533)
#' # The number of 2nd order male births (Singh et al. 2010)
#' the_1x2_table_CIs(X=204, n=412)
#' # The number of 3rd order male births (Singh et al. 2010)
#' the_1x2_table_CIs(X=103, n=167)
#' # The number of 4th order male births (Singh et al. 2010)
#' the_1x2_table_CIs(X=33, n=45)
#' # Ligarden et al. (2010)
#' the_1x2_table_CIs(X=13, n=16)
#'
the_1x2_table_CIs <- function(X, n, alpha=0.05) {

	estimate <- X / n

	# ======================================================== #
	# Ad-hoc function to print output                          #
	# ======================================================== #
	myprint <- function(txt, ...) print(sprintf(txt, ...), quote=FALSE)

	# ======================================================== #
	# Output                                                   #
	# ======================================================== #

	print(sprintf('Estimate of pi: %i / %i = %5.3f', X, n, estimate), quote=FALSE)

	print(sprintf('Interval method                  %i%% CI        width', 100 * (1-alpha)), quote=FALSE)
	print('----------------------------------------------------', quote=FALSE)

	res <- Wald_CI_1x2(X, n, alpha, F)
	print(sprintf('Wald                         %5.3f to %5.3f %8.3f', res[1], res[2], res[2] - res[1]), quote=FALSE)

	res <- Wald_CI_CC_1x2(X, n, alpha, F)
	print(sprintf('Wald with CC                 %5.3f to %5.3f %8.3f', res[1], res[2], res[2] - res[1]), quote=FALSE)

	res <- LR_CI_1x2(X, n, alpha, F)
	print(sprintf('Likelihood ratio             %5.3f to %5.3f %8.3f', res[1], res[2], res[2] - res[1]), quote=FALSE)

	res <- Wilson_score_CI_1x2(X, n, alpha, F)
	print(sprintf('Wilson score                 %5.3f to %5.3f %8.3f', res[1], res[2], res[2] - res[1]), quote=FALSE)

	res <- Wilson_score_CI_CC_1x2(X, n, alpha, F)
	print(sprintf('Wilson score with CC         %5.3f to %5.3f %8.3f', res[1], res[2], res[2] - res[1]), quote=FALSE)

	res <- AgrestiCoull_CI_1x2(X, n, alpha, F)
	print(sprintf('Agresti-Coull                %5.3f to %5.3f %8.3f', res[1], res[2], res[2] - res[1]), quote=FALSE)

	res <- Jeffreys_CI_1x2(X, n, alpha, F)
	print(sprintf('Jeffreys                     %5.3f to %5.3f %8.3f', res[1], res[2], res[2] - res[1]), quote=FALSE)

	res <- Arcsine_CI_1x2(X, n, alpha, F)
	print(sprintf('Arcsine (Anscombe)           %5.3f to %5.3f %8.3f', res[1], res[2], res[2] - res[1]), quote=FALSE)

	res <- ClopperPearson_exact_CI_1x2(X, n, alpha, F)
	print(sprintf('Clopper-Pearson exact        %5.3f to %5.3f %8.3f', res[1], res[2], res[2] - res[1]), quote=FALSE)

	#res = ClopperPearson_exact_CI_1x2_beta_version(X, n, alpha, F)
	#print(sprintf('Clopper-Pearson exact (beta) %5.3f to %5.3f %8.3f', res[1], res[2], res[2] - res[1]), quote=FALSE)

	res <- Blaker_exact_CI_1x2(X, n, alpha, F)
	print(sprintf('Blaker exact                 %5.3f to %5.3f %8.3f', res[1], res[2], res[2] - res[1]), quote=FALSE)

	res <- ClopperPearson_midP_CI_1x2(X, n, alpha, F)
	print(sprintf('Clopper-Pearson mid-p        %5.3f to %5.3f %8.3f', res[1], res[2], res[2] - res[1]), quote=FALSE)

	res <- Blaker_midP_CI_1x2(X, n, alpha, F)
	print(sprintf('Blaker mid-P                 %5.3f to %5.3f %8.3f', res[1], res[2], res[2] - res[1]), quote=FALSE)

	print('----------------------------------------------------', quote=FALSE)
	print('CC = continuity correction', quote=FALSE)

}
