#' @title The Paired CxC table - ordinal
#' @param n the total number of observations
#' @param a scores assigned to the outcome categories
#' @param alpha the nominal level, e.g. 0.05 for 95% CIs
#' @examples
#' # Pretherapy susceptability of pathogens (Peterson et al., 2007)
#' n <- rbind(
#'   c(1, 0, 1, 0, 0),
#'   c(0, 2, 8, 4, 4),
#'   c(1, 1, 31, 14, 11),
#'   c(1, 0, 15, 9, 12),
#'   c(0, 0, 2, 1, 3)
#' )
#' a <- c(8, 3.5, 0, -3.5, -8)
#' the_paired_cxc_table_ordinal(n, a)
#' @export
#' @return A string containing the last line of the printed text. This function should be called for its printed output.
the_paired_cxc_table_ordinal <- function(n, a, alpha = 0.05) {
  c <- nrow(n)
  N <- sum(n)
  nip <- apply(n, 1, sum)
  npi <- apply(n, 2, sum)

  if (c == 3) {
    .print("\nTest for three-level outomes         Statistic      P-value\n")
    .print("------------------------------------------------------------\n")
    tmp <- FleissLevinPaik_test_paired_cxc(n, F)
    P <- tmp[[1]]
    T0 <- tmp[[2]]
    df <- tmp[[3]]
    .print("Fleiss-Levin-Paik test              %6.3f (df=%g)  %9.6f\n", T0, df, P)
    .print("------------------------------------------------------------\n")
  }

  # ==============================
  # Comparing marginal mean scores
  # ==============================

  .print("\nScores: ")
  for (i in 1:c) {
    .print("%g  ", a[i])
  }
  Y1mean <- sum(a * (nip / N))
  Y2mean <- sum(a * (npi / N))
  estimate <- Y1mean - Y2mean
  .print("\nSample marginal mean scores (rows)  = %6.3f\n", Y1mean)
  .print("Sample maringal mean scores (cols)  = %6.3f\n", Y2mean)
  .print("Estimate of the difference = %6.3f\n", estimate)

  .print("\nMethod                                           %g%% CI       P-value  (test statistic)\n", 100 * (1 - alpha))
  .print("--------------------------------------------------------------------------------------\n")
  tmp <- Wald_test_and_CI_marginal_mean_scores_paired_cxc(n, a, alpha, F)
  P <- tmp[[1]]
  Z <- tmp[[2]]
  L <- tmp[[3]]
  U <- tmp[[4]]
  .print("Wald CI and test for marginal mean scores  (%6.3f to %6.3f)  %6.4f  (Z = %5.3f)\n", L, U, P, Z)

  tmp <- Score_test_and_CI_marginal_mean_scores_paired_cxc(n, a, alpha, F)
  P <- tmp[[1]]
  Z <- tmp[[2]]
  L <- tmp[[3]]
  U <- tmp[[4]]
  .print("Score CI and test for marginal mean scores (%6.3f to %6.3f)  %6.4f  (Z = %5.3f)\n", L, U, P, Z)
  .print("--------------------------------------------------------------------------------------\n")


  # ====================================
  # Comparing marginal mean ranks/ridits
  # ====================================

  results <- Wald_test_and_CI_marginal_mean_ranks_paired_cxc(n, alpha, F)
  .print("\nInference for tau\n")
  .print("-----------------\n")
  .print("Wald:       estimate = %6.4f (%g%% CI %6.4f to %6.4f); P = %7.5f, Z = %6.3f\n", results$tauhat, 100 * (1 - alpha), results$CI_tau[1], results$CI_tau[2], results$P, results$Z_Wald)
  .print("Wald logit: estimate = %6.4f (%g%% CI %6.4f to %6.4f); P = %7.5f, Z = %6.3f\n", results$tauhat, 100 * (1 - alpha), results$CI_tau_logit[1], results$CI_tau_logit[2], results$P_logit, results$Z_Wald_logit)
  .print("\nInference for alpha\n")
  .print("-------------------\n")
  .print("Wald:       estimate = %6.4f (%g%% CI %6.4f to %6.4f); P = %7.5f, Z = %6.3f\n", results$alphahat, 100 * (1 - alpha), results$CI_alpha[1], results$CI_alpha[2], results$P, results$Z_Wald)
  .print("Wald logit: estimate = %6.4f (%g%% CI %6.4f to %6.4f); P = %7.5f, Z = %6.3f\n\n", results$alphahat, 100 * (1 - alpha), results$CI_alpha_logit[1], results$CI_alpha_logit[2], results$P_logit, results$Z_Wald_logit)
}

.print <- function(s, ...) {
  print(sprintf(gsub("\n", "", s), ...), quote = F)
}
