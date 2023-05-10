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

  my_sprintf_cat("\nMethod                            Statistic      P-value\n")
  my_sprintf_cat("--------------------------------------------------------\n")
  my_sprintf_cat("Tests for unordered alternatives\n")
  results <- Pearson_LR_tests_rxc(n)
  my_sprintf_cat("  Pearson chi-squared           %6.3f (df=%g)   %8.5f\n", results$T_Pearson, results$df_Pearson, results$P_Pearson)
  my_sprintf_cat("  Likelihood ratio              %6.3f (df=%g)   %8.5f\n", results$T_LR, results$df_LR, results$P_LR)

  my_sprintf_cat("\nTests for ordered local odds ratios\n")
  results <- Pearson_LR_tests_unspecific_ordering_rx2(t(n), direction)
  my_sprintf_cat("  Pearson chi-squared           %6.3f (chibar) %8.5f\n", results$T_Pearson, results$P_Pearson)
  my_sprintf_cat("  Likelihood ratio              %6.3f (chibar) %8.5f\n", results$T_LR, results$P_LR)

  res1 <- Exact_cond_midP_unspecific_ordering_rx2(t(n), direction, "Pearson") # TODO: optimize, takes forever! (ISSUE #44)
  res2 <- Exact_cond_midP_unspecific_ordering_rx2(t(n), direction, "LR") # TODO: optimize, takes forever! (ISSUE #44)
  my_sprintf_cat("  Exact conditional (Pearson)                   %8.5f\n", res1$P)
  my_sprintf_cat("  Mid-P (Pearson)                               %8.5f\n", res1$midP)
  my_sprintf_cat("  Exact conditional (LR)                        %8.5f\n", res2$P)
  my_sprintf_cat("  Mid-P (LR)                                    %8.5f\n", res2$midP)

  my_sprintf_cat("\nTests for ordered cumulative odds ratios\n")
  results <- Pearson_LR_tests_cum_OR_2xc(n, direction)
  my_sprintf_cat("  Pearson chi-squared           %6.3f (chibar) %8.5f\n", results$T_Pearson, results$P_Pearson)
  my_sprintf_cat("  Likelihood ratio              %6.3f (chibar) %8.5f\n", results$T_LR, results$P_LR)

  res1 <- Exact_cond_midP_unspecific_ordering_rx2(t(n), direction, "PearsonCumOR") # TODO: optimize, takes forever! (ISSUE #44)
  res2 <- Exact_cond_midP_unspecific_ordering_rx2(t(n), direction, "LRCumOR") # TODO: optimize, takes forever! (ISSUE #44)
  my_sprintf_cat("  Exact conditional (Pearson)                   %8.5f\n", res1$P)
  my_sprintf_cat("  Mid-P (Pearson)                               %8.5f\n", res1$midP)
  my_sprintf_cat("  Exact conditional (LR)                        %8.5f\n", res2$P)
  my_sprintf_cat("  Mid-P (LR)                                    %8.5f\n", res2$midP)

  my_sprintf_cat("\nTest for association with column scores \n")
  res <- MantelHaenszel_test_2xc(n, 0)
  my_sprintf_cat("  Mantel-Haenszel               %6.3f (df=%g)   %8.5f\n", res$T, res$df, res$P)


  my_sprintf_cat("\nTesting the fit of a proportional odds model\n")
  results <- Cumulative_models_for_2xc(n, "logit", alpha)
  my_sprintf_cat("  Pearson goodness of fit       %6.3f (df=%g)   %8.5f\n", results$X2, results$df_X2, results$P_X2)
  my_sprintf_cat("  Likelihodd ratio (deviance)   %6.3f (df=%g)   %8.5f\n", results$D, results$df_D, results$P_D)

  res <- Brant_test_2xc(n)
  my_sprintf_cat("  Brant                         %6.3f (df=%g)   %8.5f\n", res$T, res$df, res$pvalue)


  my_sprintf_cat("\nTesting the effect in a proportional odds model\n")
  my_sprintf_cat("  Wald                          %6.3f          %8.5f\n", results$Z_Wald, results$P_Wald)
  my_sprintf_cat("  Likelihood ratio              %6.3f (df=%g)   %8.5f\n", results$T_LR, results$df_LR, results$P_LR)
  my_sprintf_cat("  Score (WMW)                   %6.3f          %8.5f\n", results$Z_MW, results$P_MW)

  res <- Exact_cond_midP_linear_rank_tests_2xc(n, 0)
  my_sprintf_cat("  Exact conditional linear rank                 %8.5f\n", res$P)
  my_sprintf_cat("  Mid-P linear rank                             %8.5f\n", res$midP)


  my_sprintf_cat("\nTesting the fit of a probit model\n")
  resultsProbit <- Cumulative_models_for_2xc(n, "probit", alpha)
  my_sprintf_cat("  Pearson goodness of fit       %6.3f (df=%g)   %8.5f\n", resultsProbit$X2, resultsProbit$df_X2, resultsProbit$P_X2)
  my_sprintf_cat("  Likelihodd ratio (deviance)   %6.3f (df=%g)   %8.5f\n", resultsProbit$D, resultsProbit$df_D, resultsProbit$P_D)


  my_sprintf_cat("\nTesting the effect in a probit model\n")
  my_sprintf_cat("  Wald                          %6.3f          %8.5f\n", resultsProbit$Z_Wald, resultsProbit$P_Wald)
  my_sprintf_cat("  Likelihood ratio              %6.3f (df=%g)   %8.5f\n", resultsProbit$T_LR, resultsProbit$df_LR, resultsProbit$P_LR)

  alphahat0 <- NULL
  if (all(n == fontanella_2008)) {
    alphahat0 <- c(-1.246452, -0.5097363, 0.2087471)
  }
  if (all(n == lydersen_2012a)) {
    alphahat0 <- c(-0.1923633, 0.5588396, 1.271953)
  }
  if (!is.null(alphahat0)) {
    res <- Score_test_for_effect_in_the_probit_model_2xc(n, alphahat0)
    my_sprintf_cat("  Score                         %6.3f (df=%g)   %8.5f\n", res$T, res$df, res$P)
  }
  my_sprintf_cat("--------------------------------------------------------\n")
  my_sprintf_cat("\nEstimation of the effect parameter beta with %g%% CIs\n", 100 * (1 - alpha))
  my_sprintf_cat("----------------------------------------------------\n")
  my_sprintf_cat("Interval         Estimate     Conf. int       Width\n")
  my_sprintf_cat("----------------------------------------------------\n")
  my_sprintf_cat("Proportional odds model\n")
  my_sprintf_cat("  Wald           %6.3f    %6.3f to %6.3f   %6.4f\n", results$betahat, results$Wald_CI[1], results$Wald_CI[2], results$Wald_CI_width)
  my_sprintf_cat("  Wald (OR)      %6.3f    %6.3f to %6.3f\n", results$OR, results$Wald_CI_OR[1], results$Wald_CI_OR[2])

  my_sprintf_cat("\nCumulative probit model\n")
  my_sprintf_cat("  Wald           %6.3f    %6.3f to %6.3f   %6.4f\n", resultsProbit$betahat, resultsProbit$Wald_CI[1], resultsProbit$Wald_CI[2], resultsProbit$Wald_CI_width)

  my_sprintf_cat("----------------------------------------------------\n")
  invisible(NULL)
}
