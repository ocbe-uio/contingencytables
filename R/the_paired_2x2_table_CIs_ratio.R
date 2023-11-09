#' @title The Paired 2x2 table CIs ratio
#' @param n frequency matrix
#' @param alpha type I error
#' @examples
#' # Airway hyper-responsiveness before and after stem cell transplantation
#' # (Bentur et al., 2009)
#' the_paired_2x2_table_CIs_ratio(bentur_2009)
#'
#' # Complete response before and after consolidation therapy
#' # (Cavo et al., 2012)
#' the_paired_2x2_table_CIs_ratio(cavo_2012)
#'
#' @export
#' @return NULL. This function should be called for its printed output.
the_paired_2x2_table_CIs_ratio <- function(n, alpha = 0.05) {
  validateArguments(mget(ls()))

  N <- sum(n)

  pi1phat <- (n[1, 1] + n[1, 2]) / N
  pip1hat <- (n[1, 1] + n[2, 1]) / N
  phihat <- pi1phat / pip1hat

  cat_sprintf("\nEstimate of pi_1+: %i/%i = %5.3f\n", n[1, 1] + n[1, 2], N, pi1phat)
  cat_sprintf("Estimate of pi_+1: %i/%i = %5.3f\n", n[1, 1] + n[2, 1], N, pip1hat)
  cat_sprintf("Estimate of phi = pi_1+/pi_+1: %5.3f\n\n", phihat)

  cat_sprintf("Interval method                              %i%% CI        log width\n", 100 * (1 - alpha))
  cat_sprintf("--------------------------------------------------------------------\n")

  tmp <- Wald_CI_ratio_paired_2x2(n, alpha)
  L <- tmp[[1]]
  U <- tmp[[2]]
  cat_sprintf("Wald                                   %7.4f to %7.4f   %7.3f\n", L, U, log(U) - log(L))

  tmp <- Tang_asymptotic_score_CI_paired_2x2(n, alpha)
  L <- tmp[[1]]
  U <- tmp[[2]]
  cat_sprintf("Tang asymptotic score                  %7.4f to %7.4f   %7.3f\n", L, U, log(U) - log(L))

  tmp <- BonettPrice_hybrid_Wilson_score_CI_paired_2x2(n, alpha)
  L <- tmp[[1]]
  U <- tmp[[2]]
  cat_sprintf("Bonett-Price hybrid Wilson score       %7.4f to %7.4f   %7.3f\n", L, U, log(U) - log(L))

  tmp <- BonettPrice_hybrid_Wilson_score_CI_CC_paired_2x2(n, alpha)
  L <- tmp[[1]]
  U <- tmp[[2]]
  cat_sprintf("Bonett-Price hybrid Wilson score w/CC  %7.4f to %7.4f   %7.3f\n", L, U, log(U) - log(L))

  tmp <- MOVER_Wilson_score_CI_paired_2x2(n, alpha)
  L <- tmp[[1]]
  U <- tmp[[2]]
  cat_sprintf("MOVER Wilson score                     %7.4f to %7.4f   %7.3f\n", L, U, log(U) - log(L))

  cat_sprintf("--------------------------------------------------------------------\n")
  cat_sprintf("CC = continuity correction")
  invisible(NULL)
}
