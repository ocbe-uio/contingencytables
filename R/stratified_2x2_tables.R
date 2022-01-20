#' @title Stratified 2x2 tables
#' @param n the observed table (a 2x2xk matrix, where k is the number of strata)
#' @param alpha the nominal level, e.g. 0.05 for 95% CIs
#' @examples
#' # Smoking and lung cancer (Doll and Hill, 1950)
#' n <- array(dim = c(2, 2, 2))
#' n[, , 1] <- matrix(c(647, 622, 2, 27), 2, byrow = TRUE)
#' n[, , 2] <- matrix(c(41, 28, 19, 32), 2, byrow = TRUE)
#' stratified_2x2_tables(n)
#'
#' # Prophylactice use of Lidocaine in myocardial infarction (Hine et al., 1989)
#' n <- array(0, dim = c(2, 2, 6))
#' n[, , 1] <- rbind(c(2, 37), c(1, 42))
#' n[, , 2] <- rbind(c(4, 40), c(4, 40))
#' n[, , 3] <- rbind(c(6, 101), c(4, 106))
#' n[, , 4] <- rbind(c(7, 96), c(5, 95))
#' n[, , 5] <- rbind(c(7, 103), c(3, 103))
#' n[, , 6] <- rbind(c(11, 143), c(4, 142))
#' stratified_2x2_tables(n)
#'
#' @export
#' @return A string of "-". This function should be called for its printed output
stratified_2x2_tables <- function(n, alpha = 0.05) {
  cat("\n")
  .print("\nThe stratum-specific effect estimates\n")
  .print("-------------------------------------\n")

  K <- dim(n)[3]
  n1pk <- apply(n[1, , ], 2, sum)
  n2pk <- apply(n[2, , ], 2, sum)
  z_alpha <- qnorm(1 - alpha / 2, 0, 1)

  .print("The difference between probabilities\n")
  for (k in 1:K) {
    estimate <- n[1, 1, k] / n1pk[k] - n[2, 1, k] / n2pk[k]
    SE <- sqrt(n[1, 1, k] * n[1, 2, k] / (n1pk[k]^3) + n[2, 1, k] * n[2, 2, k] / (n2pk[k]^3))
    L <- estimate - z_alpha * SE
    U <- estimate + z_alpha * SE
    .print("  Stratum #%i: deltahat_%i = %6.4f (%g%% Wald CI %6.4f to %6.4f)\n", k, k, estimate, 100 * (1 - alpha), L, U)
  }

  cat("\n")
  .print("\nThe ratio of probabilities\n")
  for (k in 1:K) {
    estimate <- (n[1, 1, k] / n1pk[k]) / (n[2, 1, k] / n2pk[k])
    SE <- sqrt(1 / n[1, 1, k] - 1 / n1pk[k] + 1 / n[2, 1, k] - 1 / n2pk[k])
    L <- exp(log(estimate) - z_alpha * SE)
    U <- exp(log(estimate) + z_alpha * SE)
    .print("  Stratum #%i: phihat_%i = %6.4f (%g%% Wald CI %6.4f to %6.4f)\n", k, k, estimate, 100 * (1 - alpha), L, U)
  }

  cat("\n")
  .print("\nThe odds ratio\n")
  for (k in 1:K) {
    estimate <- (n[1, 1, k] / n[1, 2, k]) / (n[2, 1, k] / n[2, 2, k])
    SE <- sqrt(1 / n[1, 1, k] + 1 / n[1, 2, k] + 1 / n[2, 1, k] + 1 / n[2, 2, k])
    L <- exp(log(estimate) - z_alpha * SE)
    U <- exp(log(estimate) + z_alpha * SE)
    .print("  Stratum #%i: thetahat_%i = %6.4f (%g%% Wald CI %6.4f to %6.4f)\n", k, k, estimate, 100 * (1 - alpha), L, U)
  }

  cat("\n")
  .print("\nEstimating a common difference between probabilities\n")
  .print("----------------------------------------------------\n")
  estimate <- MantelHaenszel_estimate_stratified_2x2(n, "linear", F)[[1]]
  .print("The Mantel-Haenszel estimate = %7.4f\n", estimate)
  estimate <- InverseVariance_estimate_stratified_2x2(n, "linear", F)[[1]]
  .print("The inverse variance estimate = %7.4f\n", estimate)

  cat("\n")
  .print("\nEstimating a common ratio of probabilities\n")
  .print("------------------------------------------\n")
  estimate <- MantelHaenszel_estimate_stratified_2x2(n, "log", F)[[1]]
  .print("The Mantel-Haenszel estimate = %7.4f\n", estimate)
  estimate <- InverseVariance_estimate_stratified_2x2(n, "log", F)[[1]]
  .print("The inverse variance estimate = %7.4f\n", estimate)

  cat("\n")
  .print("\nEstimating a common odds ratio\n")
  .print("------------------------------\n")
  estimate <- MantelHaenszel_estimate_stratified_2x2(n, "logit", F)[[1]]
  .print("The Mantel-Haenszel estimate = %7.4f\n", estimate)
  estimate <- InverseVariance_estimate_stratified_2x2(n, "logit", F)[[1]]
  .print("The inverse variance estimate = %7.4f\n", estimate)
  estimate <- Peto_OR_estimate_stratified_2x2(n, F)[[1]]
  .print("The Peto OR estimate = %7.4f\n", estimate)

  .print("\nTests of homogeneity of the difference between probabilities\n")
  .print("============================================================\n")
  .print("Test                 P-value  (test statistic)\n")
  .print("-------------------------------------------------\n")
  tmp <- Cochran_Q_test_stratified_2x2(n, "linear", "MH", F)
  P <- tmp[[1]]
  Q <- tmp[[2]]
  df <- tmp[[3]]
  .print("Cochran Q (MH)       %6.4f   (Q = %5.3f, df = %i)\n", P, Q, df)
  tmp <- Cochran_Q_test_stratified_2x2(n, "linear", "IV", F)
  P <- tmp[[1]]
  Q <- tmp[[2]]
  df <- tmp[[3]]
  .print("Cochran Q (IV)       %6.4f   (Q = %5.3f, df = %i)\n", P, Q, df)
  results <- Pearson_LR_homogeneity_test_stratified_2x2(n, "linear", F)
  .print("Likelihood ratio     %6.4f   (T = %5.3f, df = %i)\n", results$P_LR, results$T_LR, results$df_LR)
  .print("Pearson chi-squared  %6.4f   (T = %5.3f, df = %i)\n", results$P_Pearson, results$T_Pearson, results$df_Pearson)
  .print("-------------------------------------------------\n")

  cat("\n")
  .print("\nTests of homogeneity of the ratio of probabilities\n")
  .print("================================================\n")
  .print("Test                 P-value  (test statistic)\n")
  .print("-------------------------------------------------\n")
  tmp <- Cochran_Q_test_stratified_2x2(n, "log", "MH", F)
  P <- tmp[[1]]
  Q <- tmp[[2]]
  df <- tmp[[3]]
  .print("Cochran Q (MH)       %6.4f   (Q = %5.3f, df = %i)\n", P, Q, df)
  tmp <- Cochran_Q_test_stratified_2x2(n, "log", "IV", F)
  P <- tmp[[1]]
  Q <- tmp[[2]]
  df <- tmp[[3]]
  .print("Cochran Q (IV)       %6.4f   (Q = %5.3f, df = %i)\n", P, Q, df)
  results <- Pearson_LR_homogeneity_test_stratified_2x2(n, "log", F)
  .print("Likelihood ratio     %6.4f   (T = %5.3f, df = %i)\n", results$P_LR, results$T_LR, results$df_LR)
  .print("Pearson chi-squared  %6.4f   (T = %5.3f, df = %i)\n", results$P_Pearson, results$T_Pearson, results$df_Pearson)
  .print("-------------------------------------------------\n")

  cat("\n")
  .print("\nTests of homogeneity of odds ratios\n")
  .print("===================================\n")
  .print("Test                             P-value  (test statistic)\n")
  .print("-------------------------------------------------------------\n")
  tmp <- Cochran_Q_test_stratified_2x2(n, "logit", "MH", F)
  P <- tmp[[1]]
  Q <- tmp[[2]]
  df <- tmp[[3]]
  .print("Cochran Q (MH)                   %6.4f   (Q = %5.3f, df = %i)\n", P, Q, df)
  tmp <- Cochran_Q_test_stratified_2x2(n, "logit", "IV", F)
  P <- tmp[[1]]
  Q <- tmp[[2]]
  df <- tmp[[3]]
  .print("Cochran Q (IV)                   %6.4f   (Q = %5.3f, df = %i)\n", P, Q, df)
  results <- Pearson_LR_homogeneity_test_stratified_2x2(n, "logit", F)
  .print("Likelihood ratio                 %6.4f   (T = %5.3f, df = %i)\n", results$P_LR, results$T_LR, results$df_LR)
  .print("Pearson chi-squared              %6.4f   (T = %5.3f, df = %i)\n", results$P_Pearson, results$T_Pearson, results$df_Pearson)
  tmp <- BreslowDay_homogeneity_test_stratified_2x2(n, F)
  P <- tmp[[1]]
  T0 <- tmp[[2]]
  df <- tmp[[3]]
  .print("Breslow-Day W/Tarone correction  %6.4f   (T = %5.3f, df = %i)\n", P, T0, df)
  tmp <- Peto_homogeneity_test_stratified_2x2(n, F)
  P <- tmp[[1]]
  T0 <- tmp[[2]]
  df <- tmp[[3]]
  .print("Peto                             %6.4f   (T = %5.3f, df = %i)\n", P, T0, df)
  .print("-------------------------------------------------------------\n")


  .print("\nTests and CIs for a common difference between probabilities\n")
  .print("===========================================================\n")
  results <- Pearson_LR_test_common_effect_stratified_2x2(n, "linear", F)
  .print("Test                 P-value  (test statistic)\n")
  .print("-------------------------------------------------\n")
  .print("Likelihood ratio     %6.4f   (T = %5.3f, df = %i)\n", results$P_LR, results$T_LR, results$df_LR)
  .print("Pearson chi-squared  %6.4f   (T = %5.3f, df = %i)\n", results$P_Pearson, results$T_Pearson, results$df_Pearson)
  tmp <- Wald_test_and_CI_common_diff_stratified_2x2(n, "MH", alpha, F)
  P <- tmp[[1]]
  Z <- tmp[[2]]
  L_MH <- tmp[[3]]
  U_MH <- tmp[[4]]
  deltahat_MH <- tmp[[5]]
  .print("Wald (MH)            %6.4f   (Z = %5.3f)\n", P, Z)
  tmp <- Wald_test_and_CI_common_diff_stratified_2x2(n, "IV", alpha, F)
  P <- tmp[[1]]
  Z <- tmp[[2]]
  L_IV <- tmp[[3]]
  U_IV <- tmp[[4]]
  deltahat_IV <- tmp[[5]]
  .print("Wald (IV)            %6.4f   (Z = %5.3f)\n", P, Z)
  .print("-------------------------------------------------\n")
  results <- ML_estimates_and_CIs_stratified_2x2(n, "linear", alpha, F)
  .print("Interval method     estimate         %i%% CI\n", 100 * (1 - alpha))
  .print("-------------------------------------------------\n")
  .print("Wald (MH)           %7.4f    %7.4f to %7.4f\n", deltahat_MH, L_MH, U_MH)
  .print("Wald (IV)           %7.4f    %7.4f to %7.4f\n", deltahat_IV, L_IV, U_IV)
  .print("Maximum likelihood  %7.4f    %7.4f to %7.4f\n", results$betahat, results$betahatCI[1], results$betahatCI[2])
  .print("-------------------------------------------------\n")

  .print("\nTests and CIs for a common ratio of probabilities\n")
  .print("=================================================\n")
  results <- Pearson_LR_test_common_effect_stratified_2x2(n, "log", F)
  .print("Test                 P-value  (test statistic)\n")
  .print("-------------------------------------------------\n")
  .print("Likelihood ratio     %6.4f   (T = %5.3f, df = %i)\n", results$P_LR, results$T_LR, results$df_LR)
  .print("Pearson chi-squared  %6.4f   (T = %5.3f, df = %i)\n", results$P_Pearson, results$T_Pearson, results$df_Pearson)
  tmp <- Wald_test_and_CI_common_ratio_stratified_2x2(n, "MH", alpha, F)
  P <- tmp[[1]]
  Z <- tmp[[2]]
  L_MH <- tmp[[3]]
  U_MH <- tmp[[4]]
  deltahat_MH <- tmp[[5]]
  .print("Wald (MH)            %6.4f   (Z = %5.3f)\n", P, Z)
  tmp <- Wald_test_and_CI_common_ratio_stratified_2x2(n, "IV", alpha, F)
  P <- tmp[[1]]
  Z <- tmp[[2]]
  L_IV <- tmp[[3]]
  U_IV <- tmp[[4]]
  deltahat_IV <- tmp[[5]]
  .print("Wald (IV)            %6.4f   (Z = %5.3f)\n", P, Z)
  .print("-------------------------------------------------\n")
  results <- ML_estimates_and_CIs_stratified_2x2(n, "log", alpha, 0)
  .print("Interval method     estimate         %i%% CI\n", 100 * (1 - alpha))
  .print("-------------------------------------------------\n")
  .print("Wald (MH)           %7.4f    %7.4f to %7.4f\n", deltahat_MH, L_MH, U_MH)
  .print("Wald (IV)           %7.4f    %7.4f to %7.4f\n", deltahat_IV, L_IV, U_IV)
  .print("Maximum likelihood  %7.4f    %7.4f to %7.4f\n", exp(results$betahat), exp(results$betahatCI[1]), exp(results$betahatCI[2]))
  .print("-------------------------------------------------\n")

  .print("\nTests and CIs for a common odds ratio\n")
  .print("=====================================\n")
  results <- Pearson_LR_test_common_effect_stratified_2x2(n, "logit", F)
  .print("Test                     P-value  (test statistic)\n")
  .print("-----------------------------------------------------\n")
  .print("Likelihood ratio         %6.4f   (T = %5.3f, df = %i)\n", results$P_LR, results$T_LR, results$df_LR)
  .print("Pearson chi-squared      %6.4f   (T = %5.3f, df = %i)\n", results$P_Pearson, results$T_Pearson, results$df_Pearson)
  tmp <- CochranMantelHaenszel_test_stratified_2x2(n, F)
  P <- tmp[[1]]
  T0 <- tmp[[2]]
  df <- tmp[[3]]
  .print("Cochran-Mantel-Haenszel  %6.4f   (T = %5.3f, df = %i)\n", P, T0, df)
  tmp <- RBG_test_and_CI_stratified_2x2(n, alpha, F)
  P <- tmp[[1]]
  Z <- tmp[[2]]
  L_RBG <- tmp[[3]]
  U_RBG <- tmp[[4]]
  phihat_RBG <- tmp[[5]]
  .print("RBG                      %6.4f   (Z = %5.3f)\n", P, Z)
  tmp <- Woolf_test_and_CI_stratified_2x2(n, alpha, F)
  P <- tmp[[1]]
  Z <- tmp[[2]]
  L_Woolf <- tmp[[3]]
  U_Woolf <- tmp[[4]]
  phihat_Woolf <- tmp[[5]]
  .print("Woolf                    %6.4f   (Z = %5.3f)\n", P, Z)
  .print("-----------------------------------------------------\n")
  results <- ML_estimates_and_CIs_stratified_2x2(n, "logit", alpha, 0)
  .print("Interval method     estimate         %i%% CI\n", 100 * (1 - alpha))
  .print("-------------------------------------------------\n")
  .print("RBG (MH)            %7.4f    %7.4f to %7.4f\n", phihat_RBG, L_RBG, U_RBG)
  .print("Woolf (IV)          %7.4f    %7.4f to %7.4f\n", phihat_Woolf, L_Woolf, U_Woolf)
  .print("Maximum likelihood  %7.4f    %7.4f to %7.4f\n", exp(results$betahat), exp(results$betahatCI[1]), exp(results$betahatCI[1]))
  .print("-------------------------------------------------\n")
}

.print <- function(s, ...) {
  print(sprintf(gsub("\n", "", s), ...), quote = F)
}
