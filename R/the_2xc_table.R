#' @title The 2xc table
#' @param n the total number of observations
#' @param alpha the nominal level, e.g. 0.05 for 95% CIs
#' @param direction the direction of the success probabilities
#' @examples
#' \dontrun{
#' # The Adolescent Placement Study (Fontanella et al., 2008)
#' n <- rbind(c(8, 28, 72, 126), c(46, 73, 69, 86))
#' the_2xc_table(n)
#'
#' # Postoperative nausea (Lydersen et al., 2012a)
#' n <- rbind(c(14, 10, 3, 2), c(11, 7, 8, 4))
#' dir <- "decreasing"
#' the_2xc_table(n, direction = dir)
#' }
#' @export
#' @return A string of "-". This function should be called for its printed output.
the_2xc_table <- function(n, alpha = 0.05, direction = "increasing") {
  .print("\nMethod                            Statistic      P-value\n")
  .print("--------------------------------------------------------\n")
  .print("Tests for unordered alternatives\n")
  results <- Pearson_LR_tests_rxc(n, FALSE)
  .print("  Pearson chi-squared           %6.3f (df=%g)   %8.5f\n", results$T_Pearson, results$df_Pearson, results$P_Pearson)
  .print("  Likelihood ratio              %6.3f (df=%g)   %8.5f\n", results$T_LR, results$df_LR, results$P_LR)

  .print("\nTests for ordered local odds ratios\n")
  results <- Pearson_LR_tests_unspecific_ordering_rx2(t(n), direction, FALSE)
  .print("  Pearson chi-squared           %6.3f (chibar) %8.5f\n", results$T_Pearson, results$P_Pearson)
  .print("  Likelihood ratio              %6.3f (chibar) %8.5f\n", results$T_LR, results$P_LR)

  res1 <- Exact_cond_midP_unspecific_ordering_rx2(t(n), direction, "Pearson", FALSE)
  res2 <- Exact_cond_midP_unspecific_ordering_rx2(t(n), direction, "LR", FALSE)
  .print("  Exact conditional (Pearson)                   %8.5f\n", res1$P)
  .print("  Mid-P (Pearson)                               %8.5f\n", res1$midP)
  .print("  Exact conditional (LR)                        %8.5f\n", res2$P)
  .print("  Mid-P (LR)                                    %8.5f\n", res2$midP)

  .print("\nTests for ordered cumulative odds ratios\n")
  results <- Pearson_LR_tests_cum_OR_2xc(n, direction, FALSE)
  .print("  Pearson chi-squared           %6.3f (chibar) %8.5f\n", results$T_Pearson, results$P_Pearson)
  .print("  Likelihood ratio              %6.3f (chibar) %8.5f\n", results$T_LR, results$P_LR)

  res1 <- Exact_cond_midP_unspecific_ordering_rx2(t(n), direction, "PearsonCumOR", FALSE)
  res2 <- Exact_cond_midP_unspecific_ordering_rx2(t(n), direction, "LRCumOR", FALSE)
  .print("  Exact conditional (Pearson)                   %8.5f\n", res1$P)
  .print("  Mid-P (Pearson)                               %8.5f\n", res1$midP)
  .print("  Exact conditional (LR)                        %8.5f\n", res2$P)
  .print("  Mid-P (LR)                                    %8.5f\n", res2$midP)

  .print("\nTest for association with column scores \n")
  res <- MantelHaenszel_test_2xc(n, 0, FALSE)
  .print("  Mantel-Haenszel               %6.3f (df=%g)   %8.5f\n", res$T, res$df, res$P)


  .print("\nTesting the fit of a proportional odds model\n")
  results <- Cumulative_models_for_2xc(n, "logit", alpha, FALSE)
  .print("  Pearson goodness of fit       %6.3f (df=%g)   %8.5f\n", results$X2, results$df_X2, results$P_X2)
  .print("  Likelihodd ratio (deviance)   %6.3f (df=%g)   %8.5f\n", results$D, results$df_D, results$P_D)

  res <- Brant_test_2xc(n, FALSE)
  .print("  Brant                         %6.3f (df=%g)   %8.5f\n", res$T, res$df, res$P)


  .print("\nTesting the effect in a proportional odds model\n")
  .print("  Wald                          %6.3f          %8.5f\n", results$Z_Wald, results$P_Wald)
  .print("  Likelihood ratio              %6.3f (df=%g)   %8.5f\n", results$T_LR, results$df_LR, results$P_LR)
  .print("  Score (WMW)                   %6.3f          %8.5f\n", results$Z_MW, results$P_MW)

  res <- Exact_cond_midP_linear_rank_tests_2xc(n, 0, FALSE)
  .print("  Exact conditional linear rank                 %8.5f\n", res$P)
  .print("  Mid-P linear rank                             %8.5f\n", res$midP)


  .print("\nTesting the fit of a probit model\n")
  resultsProbit <- Cumulative_models_for_2xc(n, "probit", alpha, FALSE)
  .print("  Pearson goodness of fit       %6.3f (df=%g)   %8.5f\n", resultsProbit$X2, resultsProbit$df_X2, resultsProbit$P_X2)
  .print("  Likelihodd ratio (deviance)   %6.3f (df=%g)   %8.5f\n", resultsProbit$D, resultsProbit$df_D, resultsProbit$P_D)


  .print("\nTesting the effect in a probit model\n")
  .print("  Wald                          %6.3f          %8.5f\n", resultsProbit$Z_Wald, resultsProbit$P_Wald)
  .print("  Likelihood ratio              %6.3f (df=%g)   %8.5f\n", resultsProbit$T_LR, resultsProbit$df_LR, resultsProbit$P_LR)

  alphahat0 <- NULL
  if (all(n == rbind(c(8, 28, 72, 126), c(46, 73, 69, 86)))) {
    alphahat0 <- c(-1.246452, -0.5097363, 0.2087471)
  }
  if (all(n == rbind(c(14, 10, 3, 2), c(11, 7, 8, 4)))) {
    alphahat0 <- c(-0.1923633, 0.5588396, 1.271953)
  }
  if (!is.null(alphahat0)) {
    res <- Score_test_for_effect_in_the_probit_model_2xc(n, alphahat0, FALSE)
    .print("  Score                         %6.3f (df=%g)   %8.5f\n", res$T, res$df, res$P)
  }
  .print("--------------------------------------------------------\n")
  .print("\nEstimation of the effect parameter beta with %g%% CIs\n", 100 * (1 - alpha))
  .print("----------------------------------------------------\n")
  .print("Interval         Estimate     Conf. int       Width\n")
  .print("----------------------------------------------------\n")
  .print("Proportional odds model\n")
  .print("  Wald           %6.3f    %6.3f to %6.3f   %6.4f\n", results$betahat, results$Wald_CI[1], results$Wald_CI[2], results$Wald_CI_width)
  .print("  Wald (OR)      %6.3f    %6.3f to %6.3f\n", results$OR, results$Wald_CI_OR[1], results$Wald_CI_OR[2])

  .print("\nCumulative probit model\n")
  .print("  Wald           %6.3f    %6.3f to %6.3f   %6.4f\n", resultsProbit$betahat, resultsProbit$Wald_CI[1], resultsProbit$Wald_CI[2], resultsProbit$Wald_CI_width)

  .print("----------------------------------------------------\n")
}

.print <- function(s, ...) {
  print(sprintf(gsub("\n", "", s), ...), quote = FALSE)
}
