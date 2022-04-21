#' @title The 2x2 table CIs difference
#' @description Wrapper for \code{_CI_2x2} functions on Chapter 4.
#' @param n frequency matrix
#' @param alpha type I error
#' @examples
#' # An RCT of high vs standard dose of epinephrine (Perondi et al., 2004)
#' n <- matrix(c(7, 27, 1, 33), nrow = 2, byrow = TRUE)
#' the_2x2_table_CIs_difference(n)
#'
#' # The association between CHRNA4 genotype and XFS (Ritland et al., 2007)
#' n <- matrix(c(0, 16, 15, 57), nrow = 2, byrow = TRUE)
#' the_2x2_table_CIs_difference(n)
#'
#' @export
#' @return A string of "-". This function should be called for its printed output
the_2x2_table_CIs_difference <- function(n, alpha = 0.05) {
  pi1hat <- n[1, 1] / (n[1, 1] + n[1, 2])
  pi2hat <- n[2, 1] / (n[2, 1] + n[2, 2])
  deltahat <- pi1hat - pi2hat

  print(sprintf("Estimate of pi_1: %i / %i = %5.3f", n[1, 1], n[1, 1] + n[1, 2], pi1hat), quote = FALSE)
  print(sprintf("Estimate of pi_2: %i / %i = %5.3f", n[2, 1], n[2, 1] + n[2, 2], pi2hat), quote = FALSE)
  print(sprintf("Estimate of delta = pi_1 - pi_2: %5.3f", deltahat), quote = FALSE)

  print(sprintf("Interval method                           %i%% CI         width", 100 * (1 - alpha)), quote = FALSE)
  print("--------------------------------------------------------------", quote = FALSE)

  res <- Wald_CI_2x2(n, alpha, printresults = FALSE)
  print(sprintf("Wald                                %7.4f to %7.4f %7.3f", res$lower, res$upper, res$upper - res$lower), quote = FALSE)

  res <- Wald_CI_CC_2x2(n, alpha, printresults = FALSE)
  print(sprintf("Wald with continuity correction     %7.4f to %7.4f %7.3f", res$lower, res$upper, res$upper - res$lower), quote = FALSE)

  res <- AgrestiCaffo_CI_2x2(n, alpha, printresults = FALSE)
  print(sprintf("Agresti-Caffo                       %7.4f to %7.4f %7.3f", res$lower, res$upper, res$upper - res$lower), quote = FALSE)

  res <- Newcombe_hybrid_score_CI_2x2(n, alpha, printresults = FALSE)
  print(sprintf("Newcombe hybrid score               %7.4f to %7.4f %7.3f", res$lower, res$upper, res$upper - res$lower), quote = FALSE)

  res <- Mee_asymptotic_score_CI_2x2(n, alpha, printresults = FALSE)
  print(sprintf("Mee asymptotic score                %7.4f to %7.4f %7.3f", res$lower, res$upper, res$upper - res$lower), quote = FALSE)

  res <- MiettinenNurminen_asymptotic_score_CI_difference_2x2(n, alpha, printresults = FALSE)
  print(sprintf("Miettinen-Nurminen asymptotic score %7.4f to %7.4f %7.3f", res$lower, res$upper, res$upper - res$lower), quote = FALSE)

  print("--------------------------------------------------------------", quote = FALSE)
}
