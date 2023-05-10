#' @title The 2x2 table CIs ratio
#' @description Wrapper for `_CI_2x2` functions on Chapter 4.
#' @param n frequency matrix
#' @param alpha type I error
#' @seealso the_2x2_table_CIs_difference the_2x2_table_CIs_OR the_2x2_table_tests
#' @examples
#' # An RCT of high vs standard dose of epinephrine (Perondi et al., 2004)
#' the_2x2_table_CIs_ratio(perondi_2004)
#'
#' # The association between CHRNA4 genotype and XFS (Ritland et al., 2007)
#' the_2x2_table_CIs_ratio(ritland_2007)
#'
#' @export
#' @return NULL. This function should be called for its printed output
the_2x2_table_CIs_ratio <- function(n, alpha = 0.05) {
  validateArguments(mget(ls()))

  pi1hat <- n[1, 1] / (n[1, 1] + n[1, 2])
  pi2hat <- n[2, 1] / (n[2, 1] + n[2, 2])
  phihat <- pi1hat / pi2hat

  my_sprintf_cat("Estimate of pi_1: %i / %i = %5.3f\n", n[1, 1], n[1, 1] + n[1, 2], pi1hat)
  my_sprintf_cat("Estimate of pi_2: %i / %i = %5.3f\n", n[2, 1], n[2, 1] + n[2, 2], pi2hat)
  my_sprintf_cat("Estimate of phi = pi_1 / pi_2: %5.3f\n\n", phihat)

  my_sprintf_cat("Interval method                            %i%% CI      Log width\n", 100 * (1 - alpha))
  cat("----------------------------------------------------------------\n")

  res <- Katz_log_CI_2x2(n, alpha)
  L <- res$lower
  U <- res$upper
  my_sprintf_cat("Katz log                             %6.3f to %6.3f  %7.3f\n", L, U, log(U) - log(L))

  res <- Adjusted_log_CI_2x2(n, alpha)
  L <- res$lower
  U <- res$upper
  my_sprintf_cat("Adjusted log                         %6.3f to %6.3f  %7.3f\n", L, U, log(U) - log(L))

  res <- PriceBonett_approximate_Bayes_CI_2x2(n, 1.25, 2.5, alpha)
  L <- res$lower
  U <- res$upper
  my_sprintf_cat("Price-Bonett approximate Bayes       %6.3f to %6.3f  %7.3f\n", L, U, log(U) - log(L))

  res <- Inv_sinh_CI_ratio_2x2(n, alpha)
  L <- res$lower
  U <- res$upper
  my_sprintf_cat("Inverse sinh                         %6.3f to %6.3f  %7.3f\n", L, U, log(U) - log(L))

  res <- Adjusted_inv_sinh_CI_ratio_2x2(n, 0, 0, 0, 1, alpha)
  L <- res$lower
  U <- res$upper
  my_sprintf_cat("Adjusted inverse sinh                %6.3f to %6.3f  %7.3f\n", L, U, log(U) - log(L))

  res <- MOVER_R_Wilson_CI_ratio_2x2(n, alpha)
  L <- res$lower
  U <- res$upper
  my_sprintf_cat("MOVER-R Wilson                       %6.3f to %6.3f  %7.3f\n", L, U, log(U) - log(L))

  res <- MiettinenNurminen_asymptotic_score_CI_ratio_2x2(n, alpha)
  L <- res$lower
  U <- res$upper
  my_sprintf_cat("Miettinen-Nurminen asymptotic score  %6.3f to %6.3f  %7.3f\n", L, U, log(U) - log(L))

  res <- Koopman_asymptotic_score_CI_2x2(n, alpha)
  L <- res$lower
  U <- res$upper
  my_sprintf_cat("Koopman asymptotic score             %6.3f to %6.3f  %7.3f\n", L, U, log(U) - log(L))

  cat("----------------------------------------------------------------\n")
  invisible(NULL)
}
