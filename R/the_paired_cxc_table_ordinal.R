#' @title The Paired CxC table - ordinal
#' @param n the total number of observations
#' @param a scores assigned to the outcome categories
#' @param alpha the nominal level, e.g. 0.05 for 95% CIs
#' @examples
#' the_paired_cxc_table_ordinal(fischer_1999, c(8, 3.5, 0, -3.5, -8))
#' @export
#' @return NULL. This function should be called for its printed output.
the_paired_cxc_table_ordinal <- function(n, a = seq_len(nrow(n)), alpha = 0.05) {
  validateArguments(mget(ls()))

  c <- nrow(n)
  N <- sum(n)
  nip <- apply(n, 1, sum)
  npi <- apply(n, 2, sum)

  if (c == 3) {
    cat_sprintf("\nTest for three-level outomes         Statistic      P-value\n")
    cat_sprintf("------------------------------------------------------------\n")
    tmp <- FleissLevinPaik_test_paired_cxc(n)
    P <- tmp[[1]]
    T0 <- tmp[[2]]
    df <- tmp[[3]]
    cat_sprintf("Fleiss-Levin-Paik test              %6.3f (df=%g)  %9.6f\n", T0, df, P)
    cat_sprintf("------------------------------------------------------------\n")
  }

  # ==============================
  # Comparing marginal mean scores
  # ==============================

  cat_sprintf("\nScores: ")
  for (i in 1:c) {
    cat_sprintf("%g  ", a[i])
  }
  Y1mean <- sum(a * (nip / N))
  Y2mean <- sum(a * (npi / N))
  estimate <- Y1mean - Y2mean
  cat_sprintf("\nSample marginal mean scores (rows)  = %6.3f\n", Y1mean)
  cat_sprintf("Sample maringal mean scores (cols)  = %6.3f\n", Y2mean)
  cat_sprintf("Estimate of the difference = %6.3f\n", estimate)

  cat_sprintf("\nMethod                                           %g%% CI       P-value  (test statistic)\n", 100 * (1 - alpha))
  cat_sprintf("--------------------------------------------------------------------------------------\n")
  tmp <- Wald_test_and_CI_marginal_mean_scores_paired_cxc(n, a, alpha)
  P <- tmp[[1]]
  Z <- tmp[[2]]
  L <- tmp[[3]]
  U <- tmp[[4]]
  cat_sprintf("Wald CI and test for marginal mean scores  (%6.3f to %6.3f)  %6.4f  (Z = %5.3f)\n", L, U, P, Z)

  tmp <- Score_test_and_CI_marginal_mean_scores_paired_cxc(n, a, alpha)
  P <- tmp[[1]]
  Z <- tmp[[2]]
  L <- tmp[[3]]
  U <- tmp[[4]]
  cat_sprintf("Score CI and test for marginal mean scores (%6.3f to %6.3f)  %6.4f  (Z = %5.3f)\n", L, U, P, Z)
  cat_sprintf("--------------------------------------------------------------------------------------\n")


  # ====================================
  # Comparing marginal mean ranks/ridits
  # ====================================

  results <- Wald_test_and_CI_marginal_mean_ranks_paired_cxc(n, alpha)
  cat_sprintf("\nInference for tau\n")
  cat_sprintf("-----------------\n")
  cat_sprintf("Wald:       estimate = %6.4f (%g%% CI %6.4f to %6.4f); P = %7.5f, Z = %6.3f\n", results$tauhat, 100 * (1 - alpha), results$CI_tau[1], results$CI_tau[2], results$P, results$Z_Wald)
  cat_sprintf("Wald logit: estimate = %6.4f (%g%% CI %6.4f to %6.4f); P = %7.5f, Z = %6.3f\n", results$tauhat, 100 * (1 - alpha), results$CI_tau_logit[1], results$CI_tau_logit[2], results$P_logit, results$Z_Wald_logit)
  cat_sprintf("\nInference for alpha\n")
  cat_sprintf("-------------------\n")
  cat_sprintf("Wald:       estimate = %6.4f (%g%% CI %6.4f to %6.4f); P = %7.5f, Z = %6.3f\n", results$alphahat, 100 * (1 - alpha), results$CI_alpha[1], results$CI_alpha[2], results$P, results$Z_Wald)
  cat_sprintf("Wald logit: estimate = %6.4f (%g%% CI %6.4f to %6.4f); P = %7.5f, Z = %6.3f\n\n", results$alphahat, 100 * (1 - alpha), results$CI_alpha_logit[1], results$CI_alpha_logit[2], results$P_logit, results$Z_Wald_logit)
  invisible(NULL)
}
