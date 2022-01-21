#' @title The rx2 table
#' @param n the total number of observations
#' @param alpha the nominal level, e.g. 0.05 for 95% CIs
#' @param direction the direction of the success probabilities
#' @param skip_exact If `FALSE`, skips the exact conditional and mid-P tests
#' for unspecific ordering (often saves calculation time)
#' ("increasing" or "decreasing")
#' @examples
#' \dontrun{
#'
#' # Alcohol consumption and malformations (Mills and Graubard, 1987)
#' n.0 <- rbind(c(48, 17066), c(38, 14464), c(5, 788), c(1, 126), c(1, 37))
#' a.0 <- c(1, 2, 3, 4, 5)
#' the_rx2_table(n.0, a.0, 'increasing')
#'
#'
#' # Elevated troponin T levels in stroke patients (Indredavik et al., 2008)
#' n.1 <- rbind(c(8, 53), c(10, 48), c(11, 100), c(22, 102), c(6, 129))
#' a.1 <- c(7, 22, 37, 48, 55)
#' the_rx2_table(n.1, a.1, 'decreasing')
#' }
#' @export
#' @return A string of "-". This function should be called for its printed output.
the_rx2_table <- function(
	n, alpha=0.05, direction="increasing", skip_exact=FALSE
) {
	a <- seq_len(nrow(n))

	.print('Method                          Statistic      P-value')
	.print('-------------------------------------------------------')

	.print('Tests for unordered alternatives')
	results <- Pearson_LR_tests_rxc(n, 0)
	.print('  Pearson chi-squared         %6.3f (df=%g)   %8.5f', results$T_Pearson, results$df_Pearson, results$P_Pearson)
	.print('  Likelihood ratio            %6.3f (df=%g)   %8.5f', results$T_LR, results$df_LR, results$P_LR)


	.print('Tests for ordered alternatives')
	results <- Pearson_LR_tests_unspecific_ordering_rx2(n, direction, 0)
	.print('  Pearson chi-squared         %6.3f (chibar) %8.5f', results$T_Pearson, results$P_Pearson)
	.print('  Likelihood ratio            %6.3f (chibar) %8.5f', results$T_LR, results$P_LR)


	# A little bit of computation time for the exact conditional and mid-P tests
	# for unspecific ordering
	if (!skip_exact) {
		tmp <- Exact_cond_midP_unspecific_ordering_rx2(n, direction, 'Pearson', 0)
		P_Pearson <- tmp$P; midP_Pearson <- tmp$midP
		tmp <- Exact_cond_midP_unspecific_ordering_rx2(n, direction, 'LR', 0)
		P_LR <- tmp$P; midP_LR <- tmp$midP

		.print('  Exact conditional (Pearson)                 %8.5f', P_Pearson)
		.print('  Mid-P (Pearson)                             %8.5f', midP_Pearson)
		.print('  Exact conditional (LR)                      %8.5f', P_LR)
		.print('  Mid-P (LR)                                  %8.5f', midP_LR)
	}

	.print('Tests for trend in the linear model')
	results <- CochranArmitage_MH_tests_rx2(n, a, 0)
	.print('  Cochran-Armitage            %6.3f          %8.5f', results$Z_CA, results$P_CA)
	.print('  Modified Cochran-Armitage   %6.3f          %8.5f', results$Z_CA_mod, results$P_CA_mod)
	.print('  Mantel-Haenszel             %6.3f          %8.5f', results$Z_MH, results$P_MH)

	results_linear <- Trend_estimate_CI_tests_rx2(n, a, 'identity', alpha, 0)
	.print('  Wald                        %6.3f          %8.5f', results_linear$Z_Wald, results_linear$P_Wald)
	.print('  Likelihood ratio            %6.3f (df=%g)   %8.5f', results_linear$T_LR, results_linear$df_LR, results_linear$P_LR)

	.print('Testing the fit of the linear model')
	.print('  Pearson goodness-of-fit     %6.3f (df=%g)   %8.5f', results_linear$chi2, results_linear$df_chi2, results_linear$P_chi2)
	.print('  Likelihood ratio (deviance) %6.3f (df=%g)   %8.5f', results_linear$D, results_linear$df_D, results_linear$P_D)

	.print('Tests for trend in the logit model')
	results_logit <- Trend_estimate_CI_tests_rx2(n, a, 'logit', alpha, 0)
	.print('  Wald                        %6.3f          %8.5f', results_logit$Z_Wald, results_logit$P_Wald)
	.print('  Likelihood ratio            %6.3f (df=%g)   %8.5f', results_logit$T_LR, results_logit$df_LR, results_logit$P_LR)

	.print('Testing the fit of the logit model')
	.print('  Pearson goodness-of-fit     %6.3f (df=%g)   %8.5f', results_logit$chi2, results_logit$df_chi2, results_logit$P_chi2)
	.print('  Likelihood ratio (deviance) %6.3f (df=%g)   %8.5f', results_logit$D, results_logit$df_D, results_logit$P_D)
	.print('-------------------------------------------------------')

	.print('Method                  Estimate         %g%% CI           Width', 100*(1-alpha))
	.print('----------------------------------------------------------------')
	.print('Linear model')
	tmp <- CochranArmitage_CI_rx2(n, a, alpha, 0)
	betahat <- tmp$estimate
	L <- tmp$lower
	U <- tmp$upper
	.print('  Cochran-Armitage CI   %7.5f    %7.5f to %7.5f   %6.4f', betahat, L, U, U-L)
	.print('  Wald CI               %7.5f    %7.5f to %7.5f   %6.4f', results_linear$betahat, results_linear$CI_Wald[1], results_linear$CI_Wald[2], results_linear$CI_Wald_width)
	.print('Logit model')
	.print('  Wald CI               %7.5f    %7.5f to %7.5f   %6.4f', results_logit$betahat, results_logit$CI_Wald[1], results_logit$CI_Wald[2], results_logit$CI_Wald_width)
	.print('----------------------------------------------------------------')
}

.print <- function(s, ...) {
	print(sprintf(gsub('\n','',s), ...), quote=FALSE)
}
