#' @title The 2xc table
#' @param n the total number of observations
#' @param alpha the nominal level, e.g. 0.05 for 95% CIs
#' @param direction the direction of the success probabilities
#' @examples
#' \dontrun{
#' # The Adolescent Placement Study (Fontanella et al., 2008)
#' the_2xc_table(fontanella_2008)
#'
#' # Postoperative nausea (Lydersen et al., 2012a)
#' the_2xc_table(lydersen_2012a, direction = "decreasing")
#' }
#' @export
#' @return NULL. This function should be called for its printed output.
the_2xc_table <- function(n, alpha = 0.05, direction = "increasing") {
  validateArguments(mget(ls()))

  cat_sprintf("\nMethod                            Statistic      P-value\n")
  cat_sprintf("--------------------------------------------------------\n")
  cat_sprintf("Tests for unordered alternatives\n")
  results <- Pearson_LR_tests_rxc(n)
  cat_sprintf("  Pearson chi-squared           %6.3f (df=%g)   %8.5f\n", results$T_Pearson, results$df_Pearson, results$P_Pearson)
  cat_sprintf("  Likelihood ratio              %6.3f (df=%g)   %8.5f\n", results$T_LR, results$df_LR, results$P_LR)

  cat_sprintf("\nTests for ordered local odds ratios\n")
  results <- Pearson_LR_tests_unspecific_ordering_rx2(t(n), direction)
  cat_sprintf("  Pearson chi-squared           %6.3f (chibar) %8.5f\n", results$T_Pearson, results$P_Pearson)
  cat_sprintf("  Likelihood ratio              %6.3f (chibar) %8.5f\n", results$T_LR, results$P_LR)

  res1 <- Exact_cond_midP_unspecific_ordering_rx2(t(n), direction, "Pearson")
  res2 <- Exact_cond_midP_unspecific_ordering_rx2(t(n), direction, "LR")
  cat_sprintf("  Exact conditional (Pearson)                   %8.5f\n", res1$Pvalue)
  cat_sprintf("  Mid-P (Pearson)                               %8.5f\n", res1$midP)
  cat_sprintf("  Exact conditional (LR)                        %8.5f\n", res2$Pvalue)
  cat_sprintf("  Mid-P (LR)                                    %8.5f\n", res2$midP)

  cat_sprintf("\nTests for ordered cumulative odds ratios\n")
  results <- Pearson_LR_tests_cum_OR_2xc(n, direction)
  cat_sprintf("  Pearson chi-squared           %6.3f (chibar) %8.5f\n", results$T_Pearson, results$P_Pearson)
  cat_sprintf("  Likelihood ratio              %6.3f (chibar) %8.5f\n", results$T_LR, results$P_LR)

  res1 <- Exact_cond_midP_unspecific_ordering_rx2(t(n), direction, "PearsonCumOR")
  res2 <- Exact_cond_midP_unspecific_ordering_rx2(t(n), direction, "LRCumOR")
  cat_sprintf("  Exact conditional (Pearson)                   %8.5f\n", res1$Pvalue)
  cat_sprintf("  Mid-P (Pearson)                               %8.5f\n", res1$midP)
  cat_sprintf("  Exact conditional (LR)                        %8.5f\n", res2$Pvalue)
  cat_sprintf("  Mid-P (LR)                                    %8.5f\n", res2$midP)

  cat_sprintf("\nTest for association with column scores \n")
  res <- MantelHaenszel_test_2xc(n, 0)
  cat_sprintf("  Mantel-Haenszel               %6.3f (df=%g)   %8.5f\n", res$T, res$df, res$Pvalue)


  cat_sprintf("\nTesting the fit of a proportional odds model\n")
  results <- Cumulative_models_for_2xc(n, "logit", alpha)
  cat_sprintf("  Pearson goodness of fit       %6.3f (df=%g)   %8.5f\n", results$X2, results$df_X2, results$P_X2)
  cat_sprintf("  Likelihodd ratio (deviance)   %6.3f (df=%g)   %8.5f\n", results$D, results$df_D, results$P_D)

  res <- Brant_test_2xc(n)
  cat_sprintf("  Brant                         %6.3f (df=%g)   %8.5f\n", res$T, res$df, res$pvalue)


  cat_sprintf("\nTesting the effect in a proportional odds model\n")
  cat_sprintf("  Wald                          %6.3f          %8.5f\n", results$Z_Wald, results$P_Wald)
  cat_sprintf("  Likelihood ratio              %6.3f (df=%g)   %8.5f\n", results$T_LR, results$df_LR, results$P_LR)
  cat_sprintf("  Score (WMW)                   %6.3f          %8.5f\n", results$Z_MW, results$P_MW)

  res <- Exact_cond_midP_linear_rank_tests_2xc(n, 0)
  cat_sprintf("  Exact conditional linear rank                 %8.5f\n", res$Pvalue)
  cat_sprintf("  Mid-P linear rank                             %8.5f\n", res$midP)


  cat_sprintf("\nTesting the fit of a probit model\n")
  resultsProbit <- Cumulative_models_for_2xc(n, "probit", alpha)
  cat_sprintf("  Pearson goodness of fit       %6.3f (df=%g)   %8.5f\n", resultsProbit$X2, resultsProbit$df_X2, resultsProbit$P_X2)
  cat_sprintf("  Likelihodd ratio (deviance)   %6.3f (df=%g)   %8.5f\n", resultsProbit$D, resultsProbit$df_D, resultsProbit$P_D)


  cat_sprintf("\nTesting the effect in a probit model\n")
  cat_sprintf("  Wald                          %6.3f          %8.5f\n", resultsProbit$Z_Wald, resultsProbit$P_Wald)
  cat_sprintf("  Likelihood ratio              %6.3f (df=%g)   %8.5f\n", resultsProbit$T_LR, resultsProbit$df_LR, resultsProbit$P_LR)

  cat_sprintf("--------------------------------------------------------\n")
  cat_sprintf("\nEstimation of the effect parameter beta with %g%% CIs\n", 100 * (1 - alpha))
  cat_sprintf("----------------------------------------------------\n")
  cat_sprintf("Interval         Estimate     Conf. int       Width\n")
  cat_sprintf("----------------------------------------------------\n")
  cat_sprintf("Proportional odds model\n")
  cat_sprintf("  Wald           %6.3f    %6.3f to %6.3f   %6.4f\n", results$betahat, results$Wald_CI[1], results$Wald_CI[2], results$Wald_CI_width)
  cat_sprintf("  Wald (OR)      %6.3f    %6.3f to %6.3f\n", results$OR, results$Wald_CI_OR[1], results$Wald_CI_OR[2])

  cat_sprintf("\nCumulative probit model\n")
  cat_sprintf("  Wald           %6.3f    %6.3f to %6.3f   %6.4f\n", resultsProbit$betahat, resultsProbit$Wald_CI[1], resultsProbit$Wald_CI[2], resultsProbit$Wald_CI_width)

  cat_sprintf("----------------------------------------------------\n")
  invisible(NULL)
}
