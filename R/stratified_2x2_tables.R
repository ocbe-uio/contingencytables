#' @title Stratified 2x2 tables
#' @param n the observed table (a 2x2xk matrix, where k is the number of strata)
#' @param alpha the nominal level, e.g. 0.05 for 95% CIs
#' @examples
#' # Smoking and lung cancer (Doll and Hill, 1950)
#' stratified_2x2_tables(doll_hill_1950)
#'
#' # Prophylactice use of Lidocaine in myocardial infarction (Hine et al., 1989)
#' stratified_2x2_tables(hine_1989)
#'
#' @export
#' @return NULL. This function should be called for its printed output
stratified_2x2_tables <- function(n, alpha = 0.05) {
  validateArguments(mget(ls()))

  cat("The stratum-specific effect estimates\n")
  cat("-------------------------------------\n")

  K <- dim(n)[3]
  n1pk <- apply(n[1, , ], 2, sum)
  n2pk <- apply(n[2, , ], 2, sum)
  z_alpha <- qnorm(1 - alpha / 2, 0, 1)

  cat("\nThe difference between probabilities\n")
  for (k in 1:K) {
    estimate <- n[1, 1, k] / n1pk[k] - n[2, 1, k] / n2pk[k]
    SE <- sqrt(n[1, 1, k] * n[1, 2, k] / (n1pk[k]^3) + n[2, 1, k] * n[2, 2, k] / (n2pk[k]^3))
    L <- estimate - z_alpha * SE
    U <- estimate + z_alpha * SE
    cat(sprintf("  Stratum #%i: deltahat_%i = %6.4f (%g%% Wald CI %6.4f to %6.4f)\n", k, k, estimate, 100 * (1 - alpha), L, U))
  }

  cat("\nThe ratio of probabilities\n")
  for (k in 1:K) {
    estimate <- (n[1, 1, k] / n1pk[k]) / (n[2, 1, k] / n2pk[k])
    SE <- sqrt(1 / n[1, 1, k] - 1 / n1pk[k] + 1 / n[2, 1, k] - 1 / n2pk[k])
    L <- exp(log(estimate) - z_alpha * SE)
    U <- exp(log(estimate) + z_alpha * SE)
    cat(sprintf("  Stratum #%i: phihat_%i = %6.4f (%g%% Wald CI %6.4f to %6.4f)\n", k, k, estimate, 100 * (1 - alpha), L, U))
  }

  cat("\nThe odds ratio\n")
  for (k in 1:K) {
    estimate <- (n[1, 1, k] / n[1, 2, k]) / (n[2, 1, k] / n[2, 2, k])
    SE <- sqrt(1 / n[1, 1, k] + 1 / n[1, 2, k] + 1 / n[2, 1, k] + 1 / n[2, 2, k])
    L <- exp(log(estimate) - z_alpha * SE)
    U <- exp(log(estimate) + z_alpha * SE)
    cat(sprintf("  Stratum #%i: thetahat_%i = %6.4f (%g%% Wald CI %6.4f to %6.4f)\n", k, k, estimate, 100 * (1 - alpha), L, U))
  }

  cat("\nEstimating a common difference between probabilities\n")
  cat("----------------------------------------------------\n")
  estimate <- MantelHaenszel_estimate_stratified_2x2(n, "linear")[[1]]
  cat(sprintf("The Mantel-Haenszel estimate = %7.4f\n", estimate))
  estimate <- InverseVariance_estimate_stratified_2x2(n, "linear")[[1]]
  cat(sprintf("The inverse variance estimate = %7.4f\n", estimate))

  cat("\nEstimating a common ratio of probabilities\n")
  cat("------------------------------------------\n")
  estimate <- MantelHaenszel_estimate_stratified_2x2(n, "log")[[1]]
  cat(sprintf("The Mantel-Haenszel estimate = %7.4f\n", estimate))
  estimate <- InverseVariance_estimate_stratified_2x2(n, "log")[[1]]
  cat(sprintf("The inverse variance estimate = %7.4f\n", estimate))

  cat("\nEstimating a common odds ratio\n")
  cat("------------------------------\n")
  estimate <- MantelHaenszel_estimate_stratified_2x2(n, "logit")[[1]]
  cat(sprintf("The Mantel-Haenszel estimate = %7.4f\n", estimate))
  estimate <- InverseVariance_estimate_stratified_2x2(n, "logit")[[1]]
  cat(sprintf("The inverse variance estimate = %7.4f\n", estimate))
  estimate <- Peto_OR_estimate_stratified_2x2(n)[[1]]
  cat(sprintf("The Peto OR estimate = %7.4f\n", estimate))

  cat("\nTests of homogeneity of the difference between probabilities\n")
  cat("============================================================\n")
  cat("Test                 P-value  (test statistic)\n")
  cat("-------------------------------------------------\n")
  tmp <- Cochran_Q_test_stratified_2x2(n, "linear", "MH")
  P <- tmp$pvalue
  Q <- tmp$estimate
  df <- tmp$df
  cat(sprintf("Cochran Q (MH)       %6.4f   (Q = %5.3f, df = %g)\n", P, Q, df))
  tmp <- Cochran_Q_test_stratified_2x2(n, "linear", "IV")
  P <- tmp$pvalue
  Q <- tmp$estimate
  df <- tmp$df
  cat(sprintf("Cochran Q (IV)       %6.4f   (Q = %5.3f, df = %g)\n", P, Q, df))
  results <- Pearson_LR_homogeneity_test_stratified_2x2(n, "linear")
  cat(sprintf("Likelihood ratio     %6.4f   (T = %5.3f, df = %g)\n", results$P_LR, results$T_LR, results$df_LR))
  cat(sprintf("Pearson chi-squared  %6.4f   (T = %5.3f, df = %g)\n", results$P_Pearson, results$T_Pearson, results$df_Pearson))
  cat("-------------------------------------------------\n")

  cat("\nTests of homogeneity of the ratio of probabilities\n")
  cat("================================================\n")
  cat("Test                 P-value  (test statistic)\n")
  cat("-------------------------------------------------\n")
  tmp <- Cochran_Q_test_stratified_2x2(n, "log", "MH")
  P <- tmp$pvalue
  Q <- tmp$estimate
  df <- tmp$df
  cat(sprintf("Cochran Q (MH)       %6.4f   (Q = %5.3f, df = %g)\n", P, Q, df))
  tmp <- Cochran_Q_test_stratified_2x2(n, "log", "IV")
  P <- tmp$pvalue
  Q <- tmp$estimate
  df <- tmp$df
  cat(sprintf("Cochran Q (IV)       %6.4f   (Q = %5.3f, df = %g)\n", P, Q, df))
  results <- Pearson_LR_homogeneity_test_stratified_2x2(n, "log")
  cat(sprintf("Likelihood ratio     %6.4f   (T = %5.3f, df = %g)\n", results$P_LR, results$T_LR, results$df_LR))
  cat(sprintf("Pearson chi-squared  %6.4f   (T = %5.3f, df = %g)\n", results$P_Pearson, results$T_Pearson, results$df_Pearson))
  cat("-------------------------------------------------\n")

  cat("\nTests of homogeneity of odds ratios\n")
  cat("===================================\n")
  cat("Test                             P-value  (test statistic)\n")
  cat("-------------------------------------------------------------\n")
  tmp <- Cochran_Q_test_stratified_2x2(n, "logit", "MH")
  P <- tmp$pvalue
  Q <- tmp$estimate
  df <- tmp$df
  cat(sprintf("Cochran Q (MH)                   %6.4f   (Q = %5.3f, df = %g)\n", P, Q, df))
  tmp <- Cochran_Q_test_stratified_2x2(n, "logit", "IV")
  P <- tmp$pvalue
  Q <- tmp$estimate
  df <- tmp$df
  cat(sprintf("Cochran Q (IV)                   %6.4f   (Q = %5.3f, df = %g)\n", P, Q, df))
  results <- Pearson_LR_homogeneity_test_stratified_2x2(n, "logit")
  cat(sprintf("Likelihood ratio                 %6.4f   (T = %5.3f, df = %g)\n", results$P_LR, results$T_LR, results$df_LR))
  cat(sprintf("Pearson chi-squared              %6.4f   (T = %5.3f, df = %g)\n", results$P_Pearson, results$T_Pearson, results$df_Pearson))
  tmp <- BreslowDay_homogeneity_test_stratified_2x2(n)
  P <- tmp$pvalue
  T0 <- tmp$estimate
  df <- tmp$df
  cat(sprintf("Breslow-Day W/Tarone correction  %6.4f   (T = %5.3f, df = %g)\n", P, T0, df))
  tmp <- Peto_homogeneity_test_stratified_2x2(n)
  P <- tmp[[1]]
  T0 <- tmp[[2]]
  df <- tmp[[3]]
  cat(sprintf("Peto                             %6.4f   (T = %5.3f, df = %g)\n", P, T0, df))
  cat("-------------------------------------------------------------\n")


  cat("\nTests and CIs for a common difference between probabilities\n")
  cat("===========================================================\n")
  results <- Pearson_LR_test_common_effect_stratified_2x2(n, "linear")
  cat("Test                 P-value  (test statistic)\n")
  cat("-------------------------------------------------\n")
  cat(sprintf("Likelihood ratio     %6.4f   (T = %5.3f, df = %g)\n", results$P_LR, results$T_LR, results$df_LR))
  cat(sprintf("Pearson chi-squared  %6.4f   (T = %5.3f, df = %g)\n", results$P_Pearson, results$T_Pearson, results$df_Pearson))
  tmp <- Wald_test_and_CI_common_diff_stratified_2x2(n, "MH", alpha)
  P <- tmp[[1]]
  Z <- tmp[[2]]
  L_MH <- tmp[[3]]
  U_MH <- tmp[[4]]
  deltahat_MH <- tmp[[5]]
  cat(sprintf("Wald (MH)            %6.4f   (Z = %5.3f)\n", P, Z))
  tmp <- Wald_test_and_CI_common_diff_stratified_2x2(n, "IV", alpha)
  P <- tmp[[1]]
  Z <- tmp[[2]]
  L_IV <- tmp[[3]]
  U_IV <- tmp[[4]]
  deltahat_IV <- tmp[[5]]
  cat(sprintf("Wald (IV)            %6.4f   (Z = %5.3f)\n", P, Z))
  cat("-------------------------------------------------\n")
  results <- ML_estimates_and_CIs_stratified_2x2(n, "linear", alpha)
  cat(sprintf("Interval method     estimate         %i%% CI\n", 100 * (1 - alpha)))
  cat("-------------------------------------------------\n")
  cat(sprintf("Wald (MH)           %7.4f    %7.4f to %7.4f\n", deltahat_MH, L_MH, U_MH))
  cat(sprintf("Wald (IV)           %7.4f    %7.4f to %7.4f\n", deltahat_IV, L_IV, U_IV))
  cat(sprintf("Maximum likelihood  %7.4f    %7.4f to %7.4f\n", results$betahat, results$betahatCI[1], results$betahatCI[2]))
  cat("-------------------------------------------------\n")

  cat("\nTests and CIs for a common ratio of probabilities\n")
  cat("=================================================\n")
  results <- Pearson_LR_test_common_effect_stratified_2x2(n, "log")
  cat("Test                 P-value  (test statistic)\n")
  cat("-------------------------------------------------\n")
  cat(sprintf("Likelihood ratio     %6.4f   (T = %5.3f, df = %g)\n", results$P_LR, results$T_LR, results$df_LR))
  cat(sprintf("Pearson chi-squared  %6.4f   (T = %5.3f, df = %g)\n", results$P_Pearson, results$T_Pearson, results$df_Pearson))
  tmp <- Wald_test_and_CI_common_ratio_stratified_2x2(n, "MH", alpha)
  P <- tmp[[1]]
  Z <- tmp[[2]]
  L_MH <- tmp[[3]]
  U_MH <- tmp[[4]]
  deltahat_MH <- tmp[[5]]
  cat(sprintf("Wald (MH)            %6.4f   (Z = %5.3f)\n", P, Z))
  tmp <- Wald_test_and_CI_common_ratio_stratified_2x2(n, "IV", alpha)
  P <- tmp[[1]]
  Z <- tmp[[2]]
  L_IV <- tmp[[3]]
  U_IV <- tmp[[4]]
  deltahat_IV <- tmp[[5]]
  cat(sprintf("Wald (IV)            %6.4f   (Z = %5.3f)\n", P, Z))
  cat("-------------------------------------------------\n")
  results <- ML_estimates_and_CIs_stratified_2x2(n, "log", alpha)
  cat(sprintf("Interval method     estimate         %i%% CI\n", 100 * (1 - alpha)))
  cat("-------------------------------------------------\n")
  cat(sprintf("Wald (MH)           %7.4f    %7.4f to %7.4f\n", deltahat_MH, L_MH, U_MH))
  cat(sprintf("Wald (IV)           %7.4f    %7.4f to %7.4f\n", deltahat_IV, L_IV, U_IV))
  cat(sprintf("Maximum likelihood  %7.4f    %7.4f to %7.4f\n", exp(results$betahat), exp(results$betahatCI[1]), exp(results$betahatCI[2])))
  cat("-------------------------------------------------\n")

  cat("\nTests and CIs for a common odds ratio\n")
  cat("=====================================\n")
  results <- Pearson_LR_test_common_effect_stratified_2x2(n, "logit")
  cat(sprintf("Test                     P-value  (test statistic)\n"))
  cat("-----------------------------------------------------\n")
  cat(sprintf("Likelihood ratio         %6.4f   (T = %5.3f, df = %g)\n", results$P_LR, results$T_LR, results$df_LR))
  cat(sprintf("Pearson chi-squared      %6.4f   (T = %5.3f, df = %g)\n", results$P_Pearson, results$T_Pearson, results$df_Pearson))
  tmp <- CochranMantelHaenszel_test_stratified_2x2(n)
  P <- tmp[[1]]
  T0 <- tmp[[3]]
  df <- tmp[[2]]
  cat(sprintf("Cochran-Mantel-Haenszel  %6.4f   (T = %5.3f, df = %g)\n", P, T0, df))
  tmp <- RBG_test_and_CI_stratified_2x2(n, alpha)
  P <- tmp[[1]]
  Z <- tmp[[2]]
  L_RBG <- tmp[[3]]
  U_RBG <- tmp[[4]]
  phihat_RBG <- tmp[[5]]
  cat(sprintf("RBG                      %6.4f   (Z = %5.3f)\n", P, Z))
  tmp <- Woolf_test_and_CI_stratified_2x2(n, alpha)
  P <- tmp[[1]]
  Z <- tmp[[2]]
  L_Woolf <- tmp[[3]]
  U_Woolf <- tmp[[4]]
  phihat_Woolf <- tmp[[5]]
  cat(sprintf("Woolf                    %6.4f   (Z = %5.3f)\n", P, Z))
  cat("-----------------------------------------------------\n")
  results <- ML_estimates_and_CIs_stratified_2x2(n, "logit", alpha)
  cat(sprintf("Interval method     estimate         %i%% CI\n", 100 * (1 - alpha)))
  cat("-------------------------------------------------\n")
  cat(sprintf("RBG (MH)            %7.4f    %7.4f to %7.4f\n", phihat_RBG, L_RBG, U_RBG))
  cat(sprintf("Woolf (IV)          %7.4f    %7.4f to %7.4f\n", phihat_Woolf, L_Woolf, U_Woolf))
  cat(sprintf("Maximum likelihood  %7.4f    %7.4f to %7.4f\n", exp(results$betahat), exp(results$betahatCI[1]), exp(results$betahatCI[1])))
  cat("-------------------------------------------------\n")
  invisible(NULL)
}
