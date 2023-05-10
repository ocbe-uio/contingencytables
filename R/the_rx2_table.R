#' @title The rx2 table
#' @param n the total number of observations
#' @param alpha the nominal level, e.g. 0.05 for 95% CIs
#' @param direction the direction of the success probabilities
#' @param skip_exact If `FALSE`, skips the exact conditional and mid-P tests
#' for unspecific ordering (often saves calculation time)
#' ("increasing" or "decreasing")
#' @examples
#' the_rx2_table(mills_graubard_1987, skip_exact = TRUE)
#' the_rx2_table(indredavik_2008, direction = "decreasing", skip_exact = TRUE)
#' @export
#' @return NULL. This function should be called for its printed output.
the_rx2_table <- function(n, alpha = 0.05, direction = "increasing", skip_exact = FALSE) {
  validateArguments(mget(ls()))
  a <- seq_len(nrow(n))

  my_sprintf_cat("Method                          Statistic      P-value\n")
  my_sprintf_cat("-------------------------------------------------------\n")

  my_sprintf_cat("Tests for unordered alternatives\n")
  results <- Pearson_LR_tests_rxc(n)
  my_sprintf_cat("  Pearson chi-squared         %6.3f (df=%g)   %8.5f\n", results$T_Pearson, results$df_Pearson, results$P_Pearson)
  my_sprintf_cat("  Likelihood ratio            %6.3f (df=%g)   %8.5f\n", results$T_LR, results$df_LR, results$P_LR)


  my_sprintf_cat("Tests for ordered alternatives\n")
  results <- Pearson_LR_tests_unspecific_ordering_rx2(n, direction)
  my_sprintf_cat("  Pearson chi-squared         %6.3f (chibar) %8.5f\n", results$T_Pearson, results$P_Pearson)
  my_sprintf_cat("  Likelihood ratio            %6.3f (chibar) %8.5f\n", results$T_LR, results$P_LR)


  # A little bit of computation time for the exact conditional and mid-P tests
  # for unspecific ordering
  if (!skip_exact) {
    tmp <- Exact_cond_midP_unspecific_ordering_rx2(n, direction, "Pearson")
    P_Pearson <- tmp$P
    midP_Pearson <- tmp$midP
    tmp <- Exact_cond_midP_unspecific_ordering_rx2(n, direction, "LR")
    P_LR <- tmp$P
    midP_LR <- tmp$midP

    my_sprintf_cat("  Exact conditional (Pearson)                 %8.5f\n", P_Pearson)
    my_sprintf_cat("  Mid-P (Pearson)                             %8.5f\n", midP_Pearson)
    my_sprintf_cat("  Exact conditional (LR)                      %8.5f\n", P_LR)
    my_sprintf_cat("  Mid-P (LR)                                  %8.5f\n", midP_LR)
  }

  my_sprintf_cat("Tests for trend in the linear model\n")
  results <- CochranArmitage_MH_tests_rx2(n, a)
  my_sprintf_cat("  Cochran-Armitage            %6.3f          %8.5f\n", results[["Z_CA"]], results[["P_CA"]])
  my_sprintf_cat("  Modified Cochran-Armitage   %6.3f          %8.5f\n", results[["Z_CA_mod"]], results[["P_CA_mod"]])
  my_sprintf_cat("  Mantel-Haenszel             %6.3f          %8.5f\n", results[["Z_MH"]], results[["P_MH"]])

  results_linear <- Trend_estimate_CI_tests_rx2(n, a, "identity", alpha)
  my_sprintf_cat("  Wald                        %6.3f          %8.5f\n", results_linear$Z_Wald, results_linear$P_Wald)
  my_sprintf_cat("  Likelihood ratio            %6.3f (df=%g)   %8.5f\n", results_linear$T_LR, results_linear$df_LR, results_linear$P_LR)

  my_sprintf_cat("Testing the fit of the linear model\n")
  my_sprintf_cat("  Pearson goodness-of-fit     %6.3f (df=%g)   %8.5f\n", results_linear$chi2, results_linear$df_chi2, results_linear$P_chi2)
  my_sprintf_cat("  Likelihood ratio (deviance) %6.3f (df=%g)   %8.5f\n", results_linear$D, results_linear$df_D, results_linear$P_D)

  my_sprintf_cat("Tests for trend in the logit model\n")
  results_logit <- Trend_estimate_CI_tests_rx2(n, a, "logit", alpha)
  my_sprintf_cat("  Wald                        %6.3f          %8.5f\n", results_logit$Z_Wald, results_logit$P_Wald)
  my_sprintf_cat("  Likelihood ratio            %6.3f (df=%g)   %8.5f\n", results_logit$T_LR, results_logit$df_LR, results_logit$P_LR)

  my_sprintf_cat("Testing the fit of the logit model\n")
  my_sprintf_cat("  Pearson goodness-of-fit     %6.3f (df=%g)   %8.5f\n", results_logit$chi2, results_logit$df_chi2, results_logit$P_chi2)
  my_sprintf_cat("  Likelihood ratio (deviance) %6.3f (df=%g)   %8.5f\n", results_logit$D, results_logit$df_D, results_logit$P_D)
  my_sprintf_cat("-------------------------------------------------------\n")

  my_sprintf_cat("Method                  Estimate         %g%% CI           Width\n", 100 * (1 - alpha))
  my_sprintf_cat("----------------------------------------------------------------\n")
  my_sprintf_cat("Linear model\n")
  tmp <- CochranArmitage_CI_rx2(n, a, alpha)
  betahat <- tmp$estimate
  L <- tmp$lower
  U <- tmp$upper
  my_sprintf_cat("  Cochran-Armitage CI   %7.5f    %7.5f to %7.5f   %6.4f\n", betahat, L, U, U - L)
  my_sprintf_cat("  Wald CI               %7.5f    %7.5f to %7.5f   %6.4f\n", results_linear$betahat, results_linear$CI_Wald[1], results_linear$CI_Wald[2], results_linear$CI_Wald_width)
  my_sprintf_cat("Logit model\n")
  my_sprintf_cat("  Wald CI               %7.5f    %7.5f to %7.5f   %6.4f\n", results_logit$betahat, results_logit$CI_Wald[1], results_logit$CI_Wald[2], results_logit$CI_Wald_width)
  my_sprintf_cat("----------------------------------------------------------------\n")
  invisible(NULL)
}
