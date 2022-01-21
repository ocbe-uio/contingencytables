#' @title The Paired 2x2 table CIs OR
#' @param n frequency matrix
#' @param alpha type I error
#' @examples
#'
#' # Floppy eyelid syndrome vs obstructive sleep apnea (Ezra et al., 2010)
#' n <- rbind(c(7, 25), c(2, 68))
#' the_paired_2x2_table_CIs_OR(n)
#'
#' @export
#' @return A string of "-". This function should be called for its printed output.
the_paired_2x2_table_CIs_OR <- function(n, alpha = 0.05) {
  thetacondhat <- n[1, 2] / n[2, 1]

  .print("\nEstimate of theta_cond = n_12/n_21: %5.3f\n\n", thetacondhat)
  cat("\n")

  .print("Interval method                              %i%% CI        log width\n", 100 * (1 - alpha))
  print("--------------------------------------------------------------------", quote = F)

  tmp <- Wald_CI_OR_paired_2x2(n, alpha, F)
  L <- tmp[[1]]
  U <- tmp[[2]]
  .print("Wald                                   %7.4f to %7.4f   %7.3f\n", L, U, log(U) - log(L))

  tmp <- Wald_CI_OR_Laplace_paired_2x2(n, alpha, F)
  L <- tmp[[1]]
  U <- tmp[[2]]
  .print("Wald with Laplace adjustment           %7.4f to %7.4f   %7.3f\n", L, U, log(U) - log(L))

  tmp <- Transformed_Wilson_score_CI_paired_2x2(n, alpha, F)
  L <- tmp[[1]]
  U <- tmp[[2]]
  .print("Transformed Wilson score               %7.4f to %7.4f   %7.3f\n", L, U, log(U) - log(L))

  tmp <- Transformed_Clopper_Pearson_exact_CI_paired_2x2(n, alpha, F)
  L <- tmp[[1]]
  U <- tmp[[2]]
  .print("Transformed Clopper-Pearson exact      %7.4f to %7.4f  %7.3f\n", L, U, log(U) - log(L))

  tmp <- Transformed_Clopper_Pearson_midP_CI_paired_2x2(n, alpha, F)
  L <- tmp[[1]]
  U <- tmp[[2]]
  .print("Transformed Clopper-Pearson mid-P      %7.4f to %7.4f   %7.3f\n", L, U, log(U) - log(L))

  tmp <- Transformed_Blaker_exact_CI_paired_2x2(n, alpha, F)
  L <- tmp[[1]]
  U <- tmp[[2]]
  .print("Transformed Blaker exact               %7.4f to %7.4f   %7.3f\n", L, U, log(U) - log(L))

  print("--------------------------------------------------------------------", quote = F)
}

.print <- function(s, ...) {
  print(sprintf(gsub("\n", "", s), ...), quote = F)
}
