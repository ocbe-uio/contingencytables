#' @title The rxc table
#' @param n the total number of observations
#' @param alpha the nominal level, e.g. 0.05 for 95% CIs
#' @param nboot number of boostrap samples. If 0, skips tests that use bootstrapping
#' @examples
#'
#' \dontrun{
#'   # Unordered tables
#'
#'   ## Treatment for ear infection (van Balen et al., 2003)
#'   n <- matrix(c(40, 25, 54, 7, 63, 10), byrow = TRUE, ncol = 2)
#'   the_rxc_table(n)
#'
#'   ## Psychiatric diagnoses vs PA (Mangerud et al., 2004)
#'   n <- matrix(
#'     c(62, 21, 97, 48, 10, 12, 30, 7, 132, 78, 34, 17), byrow = TRUE, ncol = 2
#'   )
#'   the_rxc_table(n)
#'
#'   # Singly ordered tables
#'   ## Psychiatric diag. vs BMI (Mangerud et al., 2004)
#'   n <- matrix(
#'     c(3, 55, 23, 8, 102, 36, 6, 14, 1, 5, 21, 12, 19, 130, 64, 7, 26, 18),
#'     byrow = TRUE, ncol = 3
#'   )
#'   the_rxc_table(n)
#'
#'   ## Low birth weight vs psychiatric morbitidy (Lund et al., 2012)
#'   n <- matrix(c(22, 4, 12, 24, 9, 10, 51, 7, 6), byrow = TRUE, ncol = 3)
#'   the_rxc_table(n)
#'
#'   # Doubly ordered tables
#'   # Colorectal cancer (Jullumstroe et al., 2009)
#'   n <- rbind(
#'     c(2, 4, 29, 19), c(7, 6, 116, 51), c(19, 27, 201, 76), c(18, 22, 133, 54)
#'   )
#'   the_rxc_table(n)
#'
#'   # Breast Tumor (Bofin et al., 2004)
#'   n <- matrix(
#'   c(15, 35, 6, 9, 6, 2, 4, 2, 11, 11, 0, 0, 1, 10, 21), byrow = TRUE, ncol = 5
#'   )
#'   the_rxc_table(n)
#'
#'   # Self-rated health (Breidablik et al., 2008)
#'   n <- matrix(
#'     c(2, 3, 3, 3, 2, 58, 98, 14, 8, 162, 949, 252, 4, 48, 373, 369),
#'     byrow = TRUE, ncol = 4
#'   )
#'   the_rxc_table(n)
#' }
#' @export
#' @return NULL. This function should be called for its printed output.
the_rxc_table <- function(n, alpha = 0.05, nboot = 10000) {
	r <- nrow(n)
	c <- ncol(n)

	#--------------------------
	# Tests for association in unordered tables
	#--------------------------

	.print("\nMethod                                     Statistic      P-value\n")
	.print("-------------------------------------------------------------------\n")
	.print("Unordered rxc tables\n")
	results <- Pearson_LR_tests_rxc(n, printresults = FALSE)
	.print("  Pearson chi-square                      %6.3f (df=%g)  %9.6f\n", results$T_Pearson, results$df_Pearson, results$P_Pearson)
	.print("  Likelihood ratio                        %6.3f (df=%g)  %9.6f\n", results$T_LR, results$df_LR, results$P_LR)

	tmp <- FisherFreemanHalton_asymptotic_test_rxc(n, printresults = FALSE)
	P <- tmp[[1]]
	T0 <- tmp[[2]]
	df <- tmp[[3]]

	if (!is.na(P)) {
		.print("  Fisher-Freeman-Halton asymptotic        %6.3f (df=%g)  %9.6f\n", T0, df, P)
	}

	# There is some computation time for the exact and mid-P tests on the 3x3 table
	# (Low birth weight vs psychiatric morbitidy)
	# Change "c <=2" to "c <= 3" to calculate the tests also on these data
	if (r <= 3 && c <= 2) {
		results_exact_midP <- Exact_cond_midP_tests_rxc(n, printresults = FALSE)
		.print("  Fisher-Freeman-Halton exact conditional                %9.6f\n", results_exact_midP$P_FFH)
		.print("  Fisher-Freeman-Halton mid-P                            %9.6f\n", results_exact_midP$midP_FFH)
		.print("  Pearson exact conditional                              %9.6f\n", results_exact_midP$P_Pearson)
		.print("  Pearson mid-P                                          %9.6f\n", results_exact_midP$midP_Pearson)
		.print("  Likelihood ratio exact conditional                     %9.6f\n", results_exact_midP$P_LR)
		.print("  Likelihood ratio mid-P                                 %9.6f\n", results_exact_midP$midP_LR)
	}
	.print("-------------------------------------------------------------------\n")


	# ---------
	# Residuals
	# ---------

	tmp <- Pearson_residuals_rxc(n, printresults = FALSE)
	residuals <- tmp[[1]]
	std_residuals <- tmp[[2]]
	.print("Pearson residuals:\n")
	print(residuals)
	cat("\n")
	.print("Standardized Pearson residuals:\n")
	print(std_residuals)


	#---------------------------------
	# Simultaneous confidence intervals for rx2 tables
	#---------------------------------

	if (c == 2) {
		Scheffe_type_CIs_rxc(n, alpha, printresults = TRUE)
		.print("\n")
		Bonferroni_type_CIs_rxc(n, alpha, printresults = TRUE)
		.print("\n")
	}


	#-------------------------------
	# Tests for association in singly ordered tables
	#-------------------------------

	cat("\n")
	.print("Method                                     Statistic      P-value\n")
	.print("------------------------------------------------------------------\n")
	.print("Singly ordered rxc tables\n")
	tmp <- KruskalWallis_asymptotic_test_rxc(n, printresults = FALSE)
	P <- tmp[[1]]
	T0 <- tmp[[2]]
	df <- tmp[[3]]
	.print("  Kruskal-Wallis asymptotic               %6.3f (df=%g)  %9.6f\n", T0, df, P)

	if (exists("results_exact_midP")) {
		.print("  Kruskal-Wallis exact conditional                       %9.6f\n", results_exact_midP$P_KW)
		.print("  Kruskal-Wallis mid-P                                   %9.6f\n", results_exact_midP$midP_KW)
	}


	#------------
	# The proportional odds model
	#------------

	if (ncol(n) > 2) {
		results <- Cumulative_models_for_rxc(n, "logit", alpha, printresults = FALSE)

		cat("\n")
		.print("\nTesting the fit of a proportional odds model\n")
		.print("  Pearson goodness of fit:                %6.3f (df=%g)  %9.6f\n", results$X2, results$df_X2, results$P_X2)
		.print("  Likelihodd ratio (deviance):            %6.3f (df=%g)  %9.6f\n", results$D, results$df_D, results$P_D)

		cat("\n")
		.print("\nTesting the effect in a proportional odds model\n")
		.print("  Likelihood ratio                        %6.3f (df=%g)  %9.6f\n", results$T_LR, results$df_LR, results$P_LR)
		.print("------------------------------------------------------------------\n")

		cat("\n")
		.print("\nComparing the rows                  Statistic   P-value\n")
		.print("--------------------------------------------------------\n")
		for (i in 1:(r - 1)) {
			.print("Wald (Z-statistic) row %g vs row 1    %6.3f    %9.6f\n", i + 1, results$Z_Wald[i], results$P_Wald[i])
		}
		.print("--------------------------------------------------------\n\n")

		cat("\n")
		.print("Comparing the rows     Estimate (%g%% Wald CI)     Odds ratio (%g%% Wald CI)\n", 100 * (1 - alpha), 100 * (1 - alpha))
		.print("--------------------------------------------------------------------------\n")
		for (i in 1:(r - 1)) {
			.print("row %g vs row 1:      %6.3f (%6.3f to %6.3f)     %5.3f (%5.3f to %5.3f)\n", i + 1, results$betahat[i], results$Wald_CI[i, 1], results$Wald_CI[i, 2], results$OR[i], results$Wald_CI_OR[i, 1], results$Wald_CI_OR[i, 2])
		}
		.print("--------------------------------------------------------------------------\n")
	}


	#-------------------------------
	# Tests for association in doubly ordered tables
	#-------------------------------

	cat("\n")
	.print("\nMethod                                    Statistic    P-value\n")
	.print("---------------------------------------------------------------\n")
	.print("Doubly ordered rxc tables\n")
	tmp <- linear_by_linear_test_rxc(n, 1:r, 1:c, printresults = FALSE)
	P <- tmp[[1]]
	Z <- tmp[[2]]
	.print("  Linear-by-linear                         %6.3f    %9.6f\n", Z, P)
	tmp <- JonckheereTerpstra_test_rxc(n, printresults = FALSE)
	P <- tmp[[1]]
	Z <- tmp[[2]]
	.print("  Jonckheere-Terpstra                      %6.3f    %9.6f\n", Z, P)

	if (exists("results_exact_midP")) {
		.print("  Linear-by-linear exact conditional                 %9.6f\n", results_exact_midP$P_KW)
		.print("  Linear-by-linear mid-P                             %9.6f\n", results_exact_midP$midP_KW)
		.print("  Jonckheere-Terpstra exact conditional              %9.6f\n", results_exact_midP$P_KW)
		.print("  Jonckheere-Terpstra mid-P                          %9.6f\n", results_exact_midP$midP_KW)
	}
	.print("---------------------------------------------------------------\n")


	#-----
	# Correlation measures
	#-----

	cat("\n")
	.print("\nCorrelation measures\n")
	.print("-----------------------------------------------------------------------------------------\n")

	tmp <- Pearson_correlation_coefficient_rxc(n, 1:r, 1:c, alpha, printresults = FALSE)
	rP <- tmp[[1]]
	L <- tmp[[2]]
	U <- tmp[[3]]
	.print("Pearson correlation coefficient           %6.3f (%g%% CI %6.3f to %6.3f)\n", rP, 100 * (1 - alpha), L, U)
	if (nboot > 0) {
		tmp <- Pearson_correlation_coefficient_rxc_bca(n, nboot, 1:r, 1:c, alpha, printresults = FALSE)
		rP <- tmp[[1]]
		L <- tmp[[2]]
		U <- tmp[[3]]
		.print("Pearson correlation w / BCa bootstrap CI    %6.3f (%g%% CI %6.3f to %6.3f), nboot = %g\n", rP, 100 * (1 - alpha), L, U, nboot)
	}

	cat("\n")
	tmp <- Spearman_correlation_coefficient_rxc(n, alpha, printresults = FALSE)
	rho <- tmp[[1]]
	L <- tmp[[2]]
	U <- tmp[[3]]
	L_BW <- tmp[[4]]
	U_BW <- tmp[[5]]
	.print("\nSpearman correlation w / Fieller CI         %6.3f (%g%% CI %6.3f to %6.3f)\n", rho, 100 * (1 - alpha), L, U)
	.print("Spearman correlation w / Bonett-Wright CI   %6.3f (%g%% CI %6.3f to %6.3f)\n", rho, 100 * (1 - alpha), L_BW, U_BW)

	if (nboot > 0) {
		tmp <- Spearman_correlation_coefficient_rxc_bca(n, nboot, alpha, printresults = FALSE)
		rho <- tmp[[1]]
		L <- tmp[[2]]
		U <- tmp[[3]]
		.print("Spearman correlation w / BCa bootstrap CI   %6.3f (%g%% CI %6.3f to %6.3f), nboot = %g\n", rho, 100 * (1 - alpha), L, U, nboot)
	}

	cat("\n")
	gamma <- gamma_coefficient_rxc(n, printresults = FALSE)$gamma
	.print("\nThe gamma coefficient                     %6.3f\n", gamma)

	if (nboot > 0) {
		tmp <- gamma_coefficient_rxc_bca(n, nboot, alpha, printresults = FALSE)
		gamma <- tmp[[1]]
		L <- tmp[[2]]
		U <- tmp[[3]]
		.print("The gamma coefficient w / BCa bootstrap CI  %6.3f (%g%% CI %6.3f to %6.3f), nboot = %g\n", gamma, 100 * (1 - alpha), L, U, nboot)
	}

	cat("\n")
	tmp <- Kendalls_tau_b_rxc(n, alpha, printresults = FALSE)
	tau_b <- tmp[[1]]
	L <- tmp[[2]]
	U <- tmp[[3]]
	.print("\nKendalls tau-b w / Fieller CI              %6.3f (%g%% CI %6.3f to %6.3f)\n", tau_b, 100 * (1 - alpha), L, U)

	if (nboot > 0) {
		tmp <- Kendalls_tau_b_rxc_bca(n, nboot, alpha, printresults = FALSE)
		tau_b <- tmp[[1]]
		L <- tmp[[2]]
		U <- tmp[[3]]
		.print("Kendalls tau-b w / BCa bootstrap CI        %6.3f (%g%% CI %6.3f to %6.3f), nboot = %g\n", tau_b, 100 * (1 - alpha), L, U, nboot)
	}

	.print("-----------------------------------------------------------------------------------------\n")
}

.print <- function(s, ...) {
	print(sprintf(gsub("\n", "", s), ...), quote = FALSE)
}
