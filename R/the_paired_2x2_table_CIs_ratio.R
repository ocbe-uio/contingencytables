#' @title The Paired 2x2 table CIs ratio
#' @param n frequency matrix
#' @param alpha type I error
#' @examples
#' # Airway hyper-responsiveness before and after stem cell transplantation
#' # (Bentur et al., 2009)
#' n <- rbind(c(1, 1), c(7, 12))
#' the_paired_2x2_table_CIs_ratio(n)
#'
#' # Complete response before and after consolidation therapy
#' # (Cavo et al., 2012)
#' n <- rbind(c(59, 6), c(16, 80))
#' the_paired_2x2_table_CIs_ratio(n)
#'
#' @export
#' @return A string containing the last line of the printed text. This function should be called for its printed output.
the_paired_2x2_table_CIs_ratio <- function(n, alpha = 0.05) {
  N <- sum(n)

  pi1phat <- (n[1, 1] + n[1, 2]) / N
  pip1hat <- (n[1, 1] + n[2, 1]) / N
  phihat <- pi1phat / pip1hat

  .print("\nEstimate of pi_1+: %i/%i = %5.3f\n", n[1, 1] + n[1, 2], N, pi1phat)
  .print("Estimate of pi_+1: %i/%i = %5.3f\n\n", n[1, 1] + n[2, 1], N, pip1hat)
  .print("Estimate of phi = pi_1+/pi_+1: %5.3f\n\n", phihat)

  .print("Interval method                              %i%% CI        log width\n", 100 * (1 - alpha))
  print("--------------------------------------------------------------------", quote = FALSE)

  tmp <- Wald_CI_ratio_paired_2x2(n, alpha, FALSE)
  L <- tmp[[1]]
  U <- tmp[[2]]
  .print("Wald                                   %7.4f to %7.4f   %7.3f\n", L, U, log(U) - log(L))

  tmp <- Tang_asymptotic_score_CI_paired_2x2(n, alpha, FALSE)
  L <- tmp[[1]]
  U <- tmp[[2]]
  .print("Tang asymptotic score                  %7.4f to %7.4f   %7.3f\n", L, U, log(U) - log(L))

  tmp <- BonettPrice_hybrid_Wilson_score_CI_paired_2x2(n, alpha, FALSE)
  L <- tmp[[1]]
  U <- tmp[[2]]
  .print("Bonett-Price hybrid Wilson score       %7.4f to %7.4f   %7.3f\n", L, U, log(U) - log(L))

  tmp <- BonettPrice_hybrid_Wilson_score_CI_CC_paired_2x2(n, alpha, FALSE)
  L <- tmp[[1]]
  U <- tmp[[2]]
  .print("Bonett-Price hybrid Wilson score w/CC  %7.4f to %7.4f   %7.3f\n", L, U, log(U) - log(L))

  tmp <- MOVER_Wilson_score_CI_paired_2x2(n, alpha, FALSE)
  L <- tmp[[1]]
  U <- tmp[[2]]
  .print("MOVER Wilson score                     %7.4f to %7.4f   %7.3f\n", L, U, log(U) - log(L))

  print("--------------------------------------------------------------------", quote = FALSE)
  print("CC = continuity correction", quote = FALSE)
}

.print <- function(s, ...) {
  print(sprintf(gsub("\n", "", s), ...), quote = FALSE)
}
