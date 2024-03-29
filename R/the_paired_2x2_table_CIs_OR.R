#' @title The Paired 2x2 table CIs OR
#' @param n frequency matrix
#' @param alpha type I error
#' @examples
#' the_paired_2x2_table_CIs_OR(ezra_2010)
#' @export
#' @return NULL. This function should be called for its printed output.
the_paired_2x2_table_CIs_OR <- function(n, alpha = 0.05) {
  validateArguments(mget(ls()))

  thetacondhat <- n[1, 2] / n[2, 1]

  cat_sprintf("Estimate of theta_cond = n_12/n_21: %5.3f\n\n", thetacondhat)

  cat_sprintf("Interval method                              %i%% CI        log width\n", 100 * (1 - alpha))
  cat_sprintf("--------------------------------------------------------------------\n")

  tmp <- Wald_CI_OR_paired_2x2(n, alpha)
  L <- tmp[[1]]
  U <- tmp[[2]]
  cat_sprintf("Wald                                   %7.4f to %7.4f   %7.3f\n", L, U, log(U) - log(L))

  tmp <- Wald_CI_OR_Laplace_paired_2x2(n, alpha)
  L <- tmp[[1]]
  U <- tmp[[2]]
  cat_sprintf("Wald with Laplace adjustment           %7.4f to %7.4f   %7.3f\n", L, U, log(U) - log(L))

  tmp <- Transformed_Wilson_score_CI_paired_2x2(n, alpha)
  L <- tmp[[1]]
  U <- tmp[[2]]
  cat_sprintf("Transformed Wilson score               %7.4f to %7.4f   %7.3f\n", L, U, log(U) - log(L))

  tmp <- Transformed_Clopper_Pearson_exact_CI_paired_2x2(n, alpha)
  L <- tmp[[1]]
  U <- tmp[[2]]
  cat_sprintf("Transformed Clopper-Pearson exact      %7.4f to %7.4f  %7.3f\n", L, U, log(U) - log(L))

  tmp <- Transformed_Clopper_Pearson_midP_CI_paired_2x2(n, alpha)
  L <- tmp[[1]]
  U <- tmp[[2]]
  cat_sprintf("Transformed Clopper-Pearson mid-P      %7.4f to %7.4f   %7.3f\n", L, U, log(U) - log(L))

  tmp <- Transformed_Blaker_exact_CI_paired_2x2(n, alpha)
  L <- tmp[[1]]
  U <- tmp[[2]]
  cat_sprintf("Transformed Blaker exact               %7.4f to %7.4f   %7.3f\n", L, U, log(U) - log(L))

  cat_sprintf("--------------------------------------------------------------------")
  invisible(NULL)
}
