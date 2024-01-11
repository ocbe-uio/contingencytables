#' @title The Paired 2x2 table CIs difference
#' @param n frequency matrix
#' @param alpha type I error
#' @examples
#' # Airway hyper-responsiveness before and after stem cell transplantation
#' # (Bentur et al., 2009)
#' the_paired_2x2_table_CIs_difference(bentur_2009)
#'
#' # Complete response before and after consolidation therapy
#' # (Cavo et al., 2012)
#' the_paired_2x2_table_CIs_difference(cavo_2012)
#'
#' @export
#' @return NULL. This function should be called for its printed output.
the_paired_2x2_table_CIs_difference <- function(n, alpha = 0.05) {
  validateArguments(mget(ls()))

  N <- sum(n)

  pi1phat <- (n[1, 1] + n[1, 2]) / N
  pip1hat <- (n[1, 1] + n[2, 1]) / N
  deltahat <- pi1phat - pip1hat

  cat_sprintf("Estimate of pi_1+: %i/%i = %5.3f\n", n[1, 1] + n[1, 2], N, pi1phat)
  cat_sprintf("Estimate of pi_+1: %i/%i = %5.3f\n", n[1, 1] + n[2, 1], N, pip1hat)
  cat_sprintf("Estimate of delta = pi_1+ - pi_+1: %5.3f\n\n", deltahat)

  cat_sprintf("Interval method                           %i%% CI         width\n", 100 * (1 - alpha))
  cat_sprintf("--------------------------------------------------------------\n")

  tmp <- Wald_CI_diff_paired_2x2(n, alpha)
  L <- tmp[[1]]
  U <- tmp[[2]]
  cat_sprintf("Wald                                %7.4f to %7.4f %7.3f\n", L, U, U - L)

  tmp <- Wald_CI_diff_CC_paired_2x2(n, alpha)
  L <- tmp[[1]]
  U <- tmp[[2]]
  cat_sprintf("Wald w/continuity correction        %7.4f to %7.4f %7.3f\n", L, U, U - L)

  tmp <- Wald_CI_AgrestiMin_paired_2x2(n, alpha)
  L <- tmp[[1]]
  U <- tmp[[2]]
  cat_sprintf("Wald w/Agresti-Min adjustment       %7.4f to %7.4f %7.3f\n", L, U, U - L)

  tmp <- Wald_CI_BonettPrice_paired_2x2(n, alpha)
  L <- tmp[[1]]
  U <- tmp[[2]]
  cat_sprintf("Wald w/Bonett-Price adjustment      %7.4f to %7.4f %7.3f\n", L, U, U - L)

  tmp <- Newcombe_square_and_add_CI_paired_2x2(n, alpha)
  L <- tmp[[1]]
  U <- tmp[[2]]
  cat_sprintf("Newcombe square-and-add             %7.4f to %7.4f %7.3f\n", L, U, U - L)

  tmp <- Tango_asymptotic_score_CI_paired_2x2(n, alpha)
  L <- tmp[[1]]
  U <- tmp[[2]]
  cat_sprintf("Tango asymptotic score              %7.4f to %7.4f %7.3f\n", L, U, U - L)

  cat_sprintf("--------------------------------------------------------------")
  invisible(NULL)
}
