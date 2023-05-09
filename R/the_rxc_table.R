#' @title The rxc table
#' @param n the total number of observations
#' @param alpha the nominal level, e.g. 0.05 for 95% CIs
#' @param nboot number of boostrap samples. If 0, skips tests that use
#' bootstrapping
#' @examples
#' set.seed(8047)
#' # Unordered tables
#'
#' ## Treatment for ear infection (van Balen et al., 2003)
#' the_rxc_table(table_7.3, nboot = 200)
#'
#' ## Psychiatric diagnoses vs PA (Mangerud et al., 2004)
#' the_rxc_table(table_7.4, nboot = 0)
#'
#' # Singly ordered tables
#'
#' ## Psychiatric diag. vs BMI (Mangerud et al., 2004)
#' the_rxc_table(table_7.5, nboot = 0)
#'
#' ## Low birth weight vs psychiatric morbitidy (Lund et al., 2012)
#' the_rxc_table(table_7.6, nboot = 150)
#'
#' # Doubly ordered tables
#'
#' ## Colorectal cancer (Jullumstroe et al., 2009)
#' the_rxc_table(table_7.7, nboot = 0)
#'
#' ## Breast Tumor (Bofin et al., 2004)
#' the_rxc_table(table_7.8, nboot = 200)
#'
#' ## Self-rated health (Breidablik et al., 2008)
#' the_rxc_table(table_7.9, nboot = 0)
#' @export
#' @return NULL. This function should be called for its printed output.
the_rxc_table <- function(n, alpha = 0.05, nboot = 10000) {
  validateArguments(mget(ls()))

  r <- nrow(n)
  c <- ncol(n)

  #--------------------------
  # Tests for association in unordered tables
  #--------------------------

  my_sprintf_cat("\nMethod                                     Statistic      P-value\n")
  my_sprintf_cat("-------------------------------------------------------------------\n")
  my_sprintf_cat("Unordered rxc tables\n")
  results <- Pearson_LR_tests_rxc(n)
  my_sprintf_cat("  Pearson chi-square                      %6.3f (df=%g)  %9.6f\n", results$T_Pearson, results$df_Pearson, results$P_Pearson)
  my_sprintf_cat("  Likelihood ratio                        %6.3f (df=%g)  %9.6f\n", results$T_LR, results$df_LR, results$P_LR)

  tmp <- FisherFreemanHalton_asymptotic_test_rxc(n)
  P <- tmp[[1]]
  T0 <- tmp[[2]]
  df <- tmp[[3]]

  if (!is.na(P)) {
    my_sprintf_cat("  Fisher-Freeman-Halton asymptotic        %6.3f (df=%g)  %9.6f\n", T0, df, P)
  }

  # There is some computation time for the exact and mid-P tests on the 3x3 table
  # (Low birth weight vs psychiatric morbitidy)
  # Change "c <=2" to "c <= 3" to calculate the tests also on these data
  if (r <= 3 && c <= 2) {
    results_exact_midP <- Exact_cond_midP_tests_rxc(n)
    my_sprintf_cat("  Fisher-Freeman-Halton exact conditional                %9.6f\n", results_exact_midP$P_FFH)
    my_sprintf_cat("  Fisher-Freeman-Halton mid-P                            %9.6f\n", results_exact_midP$midP_FFH)
    my_sprintf_cat("  Pearson exact conditional                              %9.6f\n", results_exact_midP$P_Pearson)
    my_sprintf_cat("  Pearson mid-P                                          %9.6f\n", results_exact_midP$midP_Pearson)
    my_sprintf_cat("  Likelihood ratio exact conditional                     %9.6f\n", results_exact_midP$P_LR)
    my_sprintf_cat("  Likelihood ratio mid-P                                 %9.6f\n", results_exact_midP$midP_LR)
  }
  my_sprintf_cat("-------------------------------------------------------------------\n")


  # ---------
  # Residuals
  # ---------

  tmp <- Pearson_residuals_rxc(n)
  residuals <- tmp[[1]]
  std_residuals <- tmp[[2]]
  my_sprintf_cat("Pearson residuals:\n")
  print(residuals)
  cat("\n")
  my_sprintf_cat("Standardized Pearson residuals:\n")
  print(std_residuals)
  cat("\n")

  #---------------------------------
  # Simultaneous confidence intervals for rx2 tables
  #---------------------------------

  if (c == 2) {
    print(Scheffe_type_CIs_rxc(n, alpha))
    my_sprintf_cat("\n")
    print(Bonferroni_type_CIs_rxc(n, alpha))
  }


  #-------------------------------
  # Tests for association in singly ordered tables
  #-------------------------------

  cat("\n")
  my_sprintf_cat("Method                                     Statistic      P-value\n")
  my_sprintf_cat("------------------------------------------------------------------\n")
  my_sprintf_cat("Singly ordered rxc tables\n")
  tmp <- KruskalWallis_asymptotic_test_rxc(n)
  P <- tmp[[1]]
  T0 <- tmp[[2]]
  df <- tmp[[3]]
  my_sprintf_cat("  Kruskal-Wallis asymptotic               %6.3f (df=%g)  %9.6f\n", T0, df, P)

  if (exists("results_exact_midP")) {
    my_sprintf_cat("  Kruskal-Wallis exact conditional                       %9.6f\n", results_exact_midP$P_KW)
    my_sprintf_cat("  Kruskal-Wallis mid-P                                   %9.6f\n", results_exact_midP$midP_KW)
  }


  #------------
  # The proportional odds model
  #------------

  if (ncol(n) > 2) {
    results <- Cumulative_models_for_rxc(n, "logit", alpha)

    cat("\n")
    my_sprintf_cat("\nTesting the fit of a proportional odds model\n")
    my_sprintf_cat("  Pearson goodness of fit:                %6.3f (df=%g)  %9.6f\n", results$X2, results$df_X2, results$P_X2)
    my_sprintf_cat("  Likelihodd ratio (deviance):            %6.3f (df=%g)  %9.6f\n", results$D, results$df_D, results$P_D)

    cat("\n")
    my_sprintf_cat("\nTesting the effect in a proportional odds model\n")
    my_sprintf_cat("  Likelihood ratio                        %6.3f (df=%g)  %9.6f\n", results$T_LR, results$df_LR, results$P_LR)
    my_sprintf_cat("------------------------------------------------------------------\n")

    cat("\n")
    my_sprintf_cat("\nComparing the rows                  Statistic   P-value\n")
    my_sprintf_cat("--------------------------------------------------------\n")
    for (i in 1:(r - 1)) {
      my_sprintf_cat("Wald (Z-statistic) row %g vs row 1    %6.3f    %9.6f\n", i + 1, results$Z_Wald[i], results$P_Wald[i])
    }
    my_sprintf_cat("--------------------------------------------------------\n\n")

    cat("\n")
    my_sprintf_cat("Comparing the rows     Estimate (%g%% Wald CI)     Odds ratio (%g%% Wald CI)\n", 100 * (1 - alpha), 100 * (1 - alpha))
    my_sprintf_cat("--------------------------------------------------------------------------\n")
    for (i in 1:(r - 1)) {
      my_sprintf_cat("row %g vs row 1:      %6.3f (%6.3f to %6.3f)     %5.3f (%5.3f to %5.3f)\n", i + 1, results$betahat[i], results$Wald_CI[i, 1], results$Wald_CI[i, 2], results$OR[i], results$Wald_CI_OR[i, 1], results$Wald_CI_OR[i, 2])
    }
    my_sprintf_cat("--------------------------------------------------------------------------\n")
  }


  #-------------------------------
  # Tests for association in doubly ordered tables
  #-------------------------------

  cat("\n")
  my_sprintf_cat("\nMethod                                    Statistic    P-value\n")
  my_sprintf_cat("---------------------------------------------------------------\n")
  my_sprintf_cat("Doubly ordered rxc tables\n")
  tmp <- linear_by_linear_test_rxc(n, 1:r, 1:c)
  P <- tmp[[1]]
  Z <- tmp[[2]]
  my_sprintf_cat("  Linear-by-linear                         %6.3f    %9.6f\n", Z, P)
  tmp <- JonckheereTerpstra_test_rxc(n)
  P <- tmp[[1]]
  Z <- tmp[[2]]
  my_sprintf_cat("  Jonckheere-Terpstra                      %6.3f    %9.6f\n", Z, P)

  if (exists("results_exact_midP")) {
    my_sprintf_cat("  Linear-by-linear exact conditional                 %9.6f\n", results_exact_midP$P_KW)
    my_sprintf_cat("  Linear-by-linear mid-P                             %9.6f\n", results_exact_midP$midP_KW)
    my_sprintf_cat("  Jonckheere-Terpstra exact conditional              %9.6f\n", results_exact_midP$P_KW)
    my_sprintf_cat("  Jonckheere-Terpstra mid-P                          %9.6f\n", results_exact_midP$midP_KW)
  }
  my_sprintf_cat("---------------------------------------------------------------\n")


  #-----
  # Correlation measures
  #-----

  cat("\n")
  my_sprintf_cat("\nCorrelation measures\n")
  my_sprintf_cat("-----------------------------------------------------------------------------------------\n")

  tmp <- Pearson_correlation_coefficient_rxc(n, 1:r, 1:c, alpha)
  rP <- tmp[[1]]
  L <- tmp[[2]]
  U <- tmp[[3]]
  my_sprintf_cat("Pearson correlation coefficient           %6.3f (%g%% CI %6.3f to %6.3f)\n", rP, 100 * (1 - alpha), L, U)
  if (nboot > 0) {
    tmp <- Pearson_correlation_coefficient_rxc_bca(n, nboot, 1:r, 1:c, alpha)
    rP <- tmp[[1]]
    L <- tmp[[2]]
    U <- tmp[[3]]
    my_sprintf_cat("Pearson correlation w / BCa bootstrap CI    %6.3f (%g%% CI %6.3f to %6.3f), nboot = %g\n", rP, 100 * (1 - alpha), L, U, nboot)
  }

  cat("\n")
  tmp <- Spearman_correlation_coefficient_rxc(n, alpha)
  rho <- tmp[[1]]
  L <- tmp[[2]]
  U <- tmp[[3]]
  L_BW <- tmp[[4]]
  U_BW <- tmp[[5]]
  my_sprintf_cat("Spearman correlation w / Fieller CI         %6.3f (%g%% CI %6.3f to %6.3f)\n", rho, 100 * (1 - alpha), L, U)
  my_sprintf_cat("Spearman correlation w / Bonett-Wright CI   %6.3f (%g%% CI %6.3f to %6.3f)\n", rho, 100 * (1 - alpha), L_BW, U_BW)

  if (nboot > 0) {
    tmp <- Spearman_correlation_coefficient_rxc_bca(n, nboot, alpha)
    rho <- tmp[[1]]
    L <- tmp[[2]]
    U <- tmp[[3]]
    my_sprintf_cat("Spearman correlation w / BCa bootstrap CI   %6.3f (%g%% CI %6.3f to %6.3f), nboot = %g\n", rho, 100 * (1 - alpha), L, U, nboot)
  }

  cat("\n")
  gamma <- gamma_coefficient_rxc(n)$gamma
  my_sprintf_cat("The gamma coefficient                     %6.3f\n", gamma)

  if (nboot > 0) {
    tmp <- gamma_coefficient_rxc_bca(n, nboot, alpha)
    gamma <- tmp[[1]]
    L <- tmp[[2]]
    U <- tmp[[3]]
    my_sprintf_cat("The gamma coefficient w / BCa bootstrap CI  %6.3f (%g%% CI %6.3f to %6.3f), nboot = %g\n", gamma, 100 * (1 - alpha), L, U, nboot)
  }

  cat("\n")
  tmp <- Kendalls_tau_b_rxc(n, alpha)
  tau_b <- tmp[[1]]
  L <- tmp[[2]]
  U <- tmp[[3]]
  my_sprintf_cat("Kendalls tau-b w / Fieller CI              %6.3f (%g%% CI %6.3f to %6.3f)\n", tau_b, 100 * (1 - alpha), L, U)

  if (nboot > 0) {
    tmp <- Kendalls_tau_b_rxc_bca(n, nboot, alpha)
    tau_b <- tmp[[1]]
    L <- tmp[[2]]
    U <- tmp[[3]]
    my_sprintf_cat("Kendalls tau-b w / BCa bootstrap CI        %6.3f (%g%% CI %6.3f to %6.3f), nboot = %g\n", tau_b, 100 * (1 - alpha), L, U, nboot)
  }

  my_sprintf_cat("-----------------------------------------------------------------------------------------\n")
  invisible(NULL)
}
