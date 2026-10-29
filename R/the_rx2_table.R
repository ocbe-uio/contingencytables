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
  cat_sprintf("  Pearson chi-squared         %6.3f (df=%g)   %8s\n", results$T_Pearson, results$df_Pearson, format_pvalue(results$P_Pearson, fixed_digits = 5))
  cat_sprintf("  Likelihood ratio            %6.3f (df=%g)   %8s\n", results$T_LR, results$df_LR, format_pvalue(results$P_LR, fixed_digits = 5))


  cat_sprintf("Tests for ordered alternatives\n")
  results <- Pearson_LR_tests_unspecific_ordering_rx2(n, direction)
  cat_sprintf("  Pearson chi-squared         %6.3f (chibar) %8s\n", results$T_Pearson, format_pvalue(results$P_Pearson, fixed_digits = 5))
  cat_sprintf("  Likelihood ratio            %6.3f (chibar) %8s\n", results$T_LR, format_pvalue(results$P_LR, fixed_digits = 5))


  # A little bit of computation time for the exact conditional and mid-P tests
  # for unspecific ordering
  if (!skip_exact) {
    tmp <- Exact_cond_midP_unspecific_ordering_rx2(n, direction, "Pearson")
    P_Pearson <- tmp$P
    midP_Pearson <- tmp$midP
    tmp <- Exact_cond_midP_unspecific_ordering_rx2(n, direction, "LR")
    P_LR <- tmp$P
    midP_LR <- tmp$midP

    cat_sprintf("  Exact conditional (Pearson)                 %8s\n", format_pvalue(P_Pearson, fixed_digits = 5))
    cat_sprintf("  Mid-P (Pearson)                             %8s\n", format_pvalue(midP_Pearson, fixed_digits = 5))
    cat_sprintf("  Exact conditional (LR)                      %8s\n", format_pvalue(P_LR, fixed_digits = 5))
    cat_sprintf("  Mid-P (LR)                                  %8s\n", format_pvalue(midP_LR, fixed_digits = 5))
  }

  cat_sprintf("Tests for trend in the linear model\n")
  results <- CochranArmitage_MH_tests_rx2(n, a)
  cat_sprintf("  Cochran-Armitage            %6.3f          %8s\n", results[["Z_CA"]], format_pvalue(results[["P_CA"]], fixed_digits = 5))
  cat_sprintf("  Modified Cochran-Armitage   %6.3f          %8s\n", results[["Z_CA_mod"]], format_pvalue(results[["P_CA_mod"]], fixed_digits = 5))
  cat_sprintf("  Mantel-Haenszel             %6.3f          %8s\n", results[["Z_MH"]], format_pvalue(results[["P_MH"]], fixed_digits = 5))

  results_linear <- Trend_estimate_CI_tests_rx2(n, a, "identity", alpha)
  cat_sprintf("  Wald                        %6.3f          %8s\n", results_linear$Z_Wald, format_pvalue(results_linear$P_Wald, fixed_digits = 5))
  cat_sprintf("  Likelihood ratio            %6.3f (df=%g)   %8s\n", results_linear$T_LR, results_linear$df_LR, format_pvalue(results_linear$P_LR, fixed_digits = 5))

  cat_sprintf("Testing the fit of the linear model\n")
  cat_sprintf("  Pearson goodness-of-fit     %6.3f (df=%g)   %8s\n", results_linear$chi2, results_linear$df_chi2, format_pvalue(results_linear$P_chi2, fixed_digits = 5))
  cat_sprintf("  Likelihood ratio (deviance) %6.3f (df=%g)   %8s\n", results_linear$D, results_linear$df_D, format_pvalue(results_linear$P_D, fixed_digits = 5))

  cat_sprintf("Tests for trend in the logit model\n")
  results_logit <- Trend_estimate_CI_tests_rx2(n, a, "logit", alpha)
  cat_sprintf("  Wald                        %6.3f          %8s\n", results_logit$Z_Wald, format_pvalue(results_logit$P_Wald, fixed_digits = 5))
  cat_sprintf("  Likelihood ratio            %6.3f (df=%g)   %8s\n", results_logit$T_LR, results_logit$df_LR, format_pvalue(results_logit$P_LR, fixed_digits = 5))

  cat_sprintf("Testing the fit of the logit model\n")
  cat_sprintf("  Pearson goodness-of-fit     %6.3f (df=%g)   %8s\n", results_logit$chi2, results_logit$df_chi2, format_pvalue(results_logit$P_chi2, fixed_digits = 5))
  cat_sprintf("  Likelihood ratio (deviance) %6.3f (df=%g)   %8s\n", results_logit$D, results_logit$df_D, format_pvalue(results_logit$P_D, fixed_digits = 5))
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
