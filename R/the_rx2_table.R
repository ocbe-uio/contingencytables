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

  cat_sprintf("Method                          Statistic      P-value\n")
  cat_sprintf("-------------------------------------------------------\n")

  cat_sprintf("Tests for unordered alternatives\n")
  results <- Pearson_LR_tests_rxc(n)
  cat_sprintf("  Pearson chi-squared         %6.3f (df=%g)   %8.5f\n", results$T_Pearson, results$df_Pearson, results$P_Pearson)
  cat_sprintf("  Likelihood ratio            %6.3f (df=%g)   %8.5f\n", results$T_LR, results$df_LR, results$P_LR)


  cat_sprintf("Tests for ordered alternatives\n")
  results <- Pearson_LR_tests_unspecific_ordering_rx2(n, direction)
  cat_sprintf("  Pearson chi-squared         %6.3f (chibar) %8.5f\n", results$T_Pearson, results$P_Pearson)
  cat_sprintf("  Likelihood ratio            %6.3f (chibar) %8.5f\n", results$T_LR, results$P_LR)


  # A little bit of computation time for the exact conditional and mid-P tests
  # for unspecific ordering
  if (!skip_exact) {
    tmp <- Exact_cond_midP_unspecific_ordering_rx2(n, direction, "Pearson")
    P_Pearson <- tmp$P
    midP_Pearson <- tmp$midP
    tmp <- Exact_cond_midP_unspecific_ordering_rx2(n, direction, "LR")
    P_LR <- tmp$P
    midP_LR <- tmp$midP

    cat_sprintf("  Exact conditional (Pearson)                 %8.5f\n", P_Pearson)
    cat_sprintf("  Mid-P (Pearson)                             %8.5f\n", midP_Pearson)
    cat_sprintf("  Exact conditional (LR)                      %8.5f\n", P_LR)
    cat_sprintf("  Mid-P (LR)                                  %8.5f\n", midP_LR)
  }

  cat_sprintf("Tests for trend in the linear model\n")
  results <- CochranArmitage_MH_tests_rx2(n, a)
  cat_sprintf("  Cochran-Armitage            %6.3f          %8.5f\n", results[["Z_CA"]], results[["P_CA"]])
  cat_sprintf("  Modified Cochran-Armitage   %6.3f          %8.5f\n", results[["Z_CA_mod"]], results[["P_CA_mod"]])
  cat_sprintf("  Mantel-Haenszel             %6.3f          %8.5f\n", results[["Z_MH"]], results[["P_MH"]])

  results_linear <- Trend_estimate_CI_tests_rx2(n, a, "identity", alpha)
  cat_sprintf("  Wald                        %6.3f          %8.5f\n", results_linear$Z_Wald, results_linear$P_Wald)
  cat_sprintf("  Likelihood ratio            %6.3f (df=%g)   %8.5f\n", results_linear$T_LR, results_linear$df_LR, results_linear$P_LR)

  cat_sprintf("Testing the fit of the linear model\n")
  cat_sprintf("  Pearson goodness-of-fit     %6.3f (df=%g)   %8.5f\n", results_linear$chi2, results_linear$df_chi2, results_linear$P_chi2)
  cat_sprintf("  Likelihood ratio (deviance) %6.3f (df=%g)   %8.5f\n", results_linear$D, results_linear$df_D, results_linear$P_D)

  cat_sprintf("Tests for trend in the logit model\n")
  results_logit <- Trend_estimate_CI_tests_rx2(n, a, "logit", alpha)
  cat_sprintf("  Wald                        %6.3f          %8.5f\n", results_logit$Z_Wald, results_logit$P_Wald)
  cat_sprintf("  Likelihood ratio            %6.3f (df=%g)   %8.5f\n", results_logit$T_LR, results_logit$df_LR, results_logit$P_LR)

  cat_sprintf("Testing the fit of the logit model\n")
  cat_sprintf("  Pearson goodness-of-fit     %6.3f (df=%g)   %8.5f\n", results_logit$chi2, results_logit$df_chi2, results_logit$P_chi2)
  cat_sprintf("  Likelihood ratio (deviance) %6.3f (df=%g)   %8.5f\n", results_logit$D, results_logit$df_D, results_logit$P_D)
  cat_sprintf("-------------------------------------------------------\n")

  cat_sprintf("Method                  Estimate         %g%% CI           Width\n", 100 * (1 - alpha))
  cat_sprintf("----------------------------------------------------------------\n")
  cat_sprintf("Linear model\n")
  tmp <- CochranArmitage_CI_rx2(n, a, alpha)
  betahat <- tmp$estimate
  L <- tmp$lower
  U <- tmp$upper
  cat_sprintf("  Cochran-Armitage CI   %7.5f    %7.5f to %7.5f   %6.4f\n", betahat, L, U, U - L)
  cat_sprintf("  Wald CI               %7.5f    %7.5f to %7.5f   %6.4f\n", results_linear$betahat, results_linear$CI_Wald[1], results_linear$CI_Wald[2], results_linear$CI_Wald_width)
  cat_sprintf("Logit model\n")
  cat_sprintf("  Wald CI               %7.5f    %7.5f to %7.5f   %6.4f\n", results_logit$betahat, results_logit$CI_Wald[1], results_logit$CI_Wald[2], results_logit$CI_Wald_width)
  cat_sprintf("----------------------------------------------------------------\n")
  invisible(NULL)
}
