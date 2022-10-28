#' @title The rx2 table
#' @param n the total number of observations
#' @param alpha the nominal level, e.g. 0.05 for 95% CIs
#' @param direction the direction of the success probabilities
#' @param skip_exact If `FALSE`, skips the exact conditional and mid-P tests
#' for unspecific ordering (often saves calculation time)
#' ("increasing" or "decreasing")
#' @examples
#' the_rx2_table(
#'   mills_graubard_1987, direction = "increasing", skip_exact = TRUE
#' )
#' the_rx2_table(indredavik_2008, direction = "decreasing", skip_exact = TRUE)
#' @export
#' @return A string of "-". This function should be called for its printed output.
the_rx2_table <- function(n, alpha = 0.05, direction = "increasing", skip_exact = FALSE) {
  validateArguments(mget(ls()))
  a <- seq_len(nrow(n))

  my_sprintf("Method                          Statistic      P-value")
  my_sprintf("-------------------------------------------------------")

  my_sprintf("Tests for unordered alternatives")
  results <- Pearson_LR_tests_rxc(n, 0)
  my_sprintf("  Pearson chi-squared         %6.3f (df=%g)   %8.5f", results$T_Pearson, results$df_Pearson, results$P_Pearson)
  my_sprintf("  Likelihood ratio            %6.3f (df=%g)   %8.5f", results$T_LR, results$df_LR, results$P_LR)


  my_sprintf("Tests for ordered alternatives")
  results <- Pearson_LR_tests_unspecific_ordering_rx2(n, direction, 0)
  my_sprintf("  Pearson chi-squared         %6.3f (chibar) %8.5f", results$T_Pearson, results$P_Pearson)
  my_sprintf("  Likelihood ratio            %6.3f (chibar) %8.5f", results$T_LR, results$P_LR)


  # A little bit of computation time for the exact conditional and mid-P tests
  # for unspecific ordering
  if (!skip_exact) {
    tmp <- Exact_cond_midP_unspecific_ordering_rx2(n, direction, "Pearson", 0)
    P_Pearson <- tmp$P
    midP_Pearson <- tmp$midP
    tmp <- Exact_cond_midP_unspecific_ordering_rx2(n, direction, "LR", 0)
    P_LR <- tmp$P
    midP_LR <- tmp$midP

    my_sprintf("  Exact conditional (Pearson)                 %8.5f", P_Pearson)
    my_sprintf("  Mid-P (Pearson)                             %8.5f", midP_Pearson)
    my_sprintf("  Exact conditional (LR)                      %8.5f", P_LR)
    my_sprintf("  Mid-P (LR)                                  %8.5f", midP_LR)
  }

  my_sprintf("Tests for trend in the linear model")
  results <- CochranArmitage_MH_tests_rx2(n, a)$statistics
  my_sprintf("  Cochran-Armitage            %6.3f          %8.5f", results[["t"]]["Z_CA"], results[["pvalue"]]["P_CA"])
  my_sprintf("  Modified Cochran-Armitage   %6.3f          %8.5f", results[["t"]]["Z_CA_mod"], results[["pvalue"]]["P_CA_mod"])
  my_sprintf("  Mantel-Haenszel             %6.3f          %8.5f", results[["t"]]["Z_MH"], results[["pvalue"]]["P_MH"])

  results_linear <- Trend_estimate_CI_tests_rx2(n, a, "identity", alpha, 0)
  my_sprintf("  Wald                        %6.3f          %8.5f", results_linear$Z_Wald, results_linear$P_Wald)
  my_sprintf("  Likelihood ratio            %6.3f (df=%g)   %8.5f", results_linear$T_LR, results_linear$df_LR, results_linear$P_LR)

  my_sprintf("Testing the fit of the linear model")
  my_sprintf("  Pearson goodness-of-fit     %6.3f (df=%g)   %8.5f", results_linear$chi2, results_linear$df_chi2, results_linear$P_chi2)
  my_sprintf("  Likelihood ratio (deviance) %6.3f (df=%g)   %8.5f", results_linear$D, results_linear$df_D, results_linear$P_D)

  my_sprintf("Tests for trend in the logit model")
  results_logit <- Trend_estimate_CI_tests_rx2(n, a, "logit", alpha, 0)
  my_sprintf("  Wald                        %6.3f          %8.5f", results_logit$Z_Wald, results_logit$P_Wald)
  my_sprintf("  Likelihood ratio            %6.3f (df=%g)   %8.5f", results_logit$T_LR, results_logit$df_LR, results_logit$P_LR)

  my_sprintf("Testing the fit of the logit model")
  my_sprintf("  Pearson goodness-of-fit     %6.3f (df=%g)   %8.5f", results_logit$chi2, results_logit$df_chi2, results_logit$P_chi2)
  my_sprintf("  Likelihood ratio (deviance) %6.3f (df=%g)   %8.5f", results_logit$D, results_logit$df_D, results_logit$P_D)
  my_sprintf("-------------------------------------------------------")

  my_sprintf("Method                  Estimate         %g%% CI           Width", 100 * (1 - alpha))
  my_sprintf("----------------------------------------------------------------")
  my_sprintf("Linear model")
  tmp <- CochranArmitage_CI_rx2(n, a, alpha)$statistics
  betahat <- tmp$estimate
  L <- tmp$lower
  U <- tmp$upper
  my_sprintf("  Cochran-Armitage CI   %7.5f    %7.5f to %7.5f   %6.4f", betahat, L, U, U - L)
  my_sprintf("  Wald CI               %7.5f    %7.5f to %7.5f   %6.4f", results_linear$betahat, results_linear$CI_Wald[1], results_linear$CI_Wald[2], results_linear$CI_Wald_width)
  my_sprintf("Logit model")
  my_sprintf("  Wald CI               %7.5f    %7.5f to %7.5f   %6.4f", results_logit$betahat, results_logit$CI_Wald[1], results_logit$CI_Wald[2], results_logit$CI_Wald_width)
  my_sprintf("----------------------------------------------------------------")
}
