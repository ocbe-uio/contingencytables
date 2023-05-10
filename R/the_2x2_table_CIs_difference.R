#' @title The 2x2 table CIs difference
#' @description Wrapper for `_CI_2x2` functions on Chapter 4.
#' @param n frequency matrix
#' @param alpha type I error
#' @examples
#' # An RCT of high vs standard dose of epinephrine (Perondi et al., 2004)
#' the_2x2_table_CIs_difference(perondi_2004)
#'
#' # The association between CHRNA4 genotype and XFS (Ritland et al., 2007)
#' the_2x2_table_CIs_difference(ritland_2007)
#'
#' @export
#' @return NULL. This function should be called for its printed output
the_2x2_table_CIs_difference <- function(n, alpha = 0.05) {
  validateArguments(mget(ls()))

  pi1hat <- n[1, 1] / (n[1, 1] + n[1, 2])
  pi2hat <- n[2, 1] / (n[2, 1] + n[2, 2])
  deltahat <- pi1hat - pi2hat

  my_sprintf_cat("Estimate of pi_1: %i / %i = %5.3f\n", n[1, 1], n[1, 1] + n[1, 2], pi1hat)
  my_sprintf_cat("Estimate of pi_2: %i / %i = %5.3f\n", n[2, 1], n[2, 1] + n[2, 2], pi2hat)
  my_sprintf_cat("Estimate of delta = pi_1 - pi_2: %5.3f\n\n", deltahat)

  my_sprintf_cat("Interval method                           %i%% CI         width\n", 100 * (1 - alpha))
  cat("--------------------------------------------------------------\n")

  res <- Wald_CI_2x2(n, alpha)
  my_sprintf_cat("Wald                                %7.4f to %7.4f %7.3f\n", res$lower, res$upper, res$upper - res$lower)

  res <- Wald_CI_CC_2x2(n, alpha)
  my_sprintf_cat("Wald with continuity correction     %7.4f to %7.4f %7.3f\n", res$lower, res$upper, res$upper - res$lower)

  res <- AgrestiCaffo_CI_2x2(n, alpha)
  my_sprintf_cat("Agresti-Caffo                       %7.4f to %7.4f %7.3f\n", res$lower, res$upper, res$upper - res$lower)

  res <- Newcombe_hybrid_score_CI_2x2(n, alpha)
  my_sprintf_cat("Newcombe hybrid score               %7.4f to %7.4f %7.3f\n", res$lower, res$upper, res$upper - res$lower)

  res <- Mee_asymptotic_score_CI_2x2(n, alpha)
  my_sprintf_cat("Mee asymptotic score                %7.4f to %7.4f %7.3f\n", res$lower, res$upper, res$upper - res$lower)

  res <- MiettinenNurminen_asymptotic_score_CI_difference_2x2(n, alpha)
  my_sprintf_cat("Miettinen-Nurminen asymptotic score %7.4f to %7.4f %7.3f\n", res$lower, res$upper, res$upper - res$lower)

  cat("--------------------------------------------------------------")
  invisible(NULL)
}
