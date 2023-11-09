#' @title The 2x2 table CIs odds ratio
#' @description Wrapper for `_CI_OR_2x2` functions on Chapter 4.
#' @param n frequency matrix
#' @param alpha type I error
#' @examples
#' # Example: A lady tasting a cup of tea
#' the_2x2_table_CIs_OR(tea)
#'
#' # Example: Perondi et al. (2004)
#' the_2x2_table_CIs_OR(perondi_2004)
#'
#' # Example: Lampasona et al. (2013)
#' the_2x2_table_CIs_OR(lampasona_2013)
#'
#' # Example: Ritland et al. (2007)
#' the_2x2_table_CIs_OR(ritland_2007)
#'
#' @export
#' @return NULL. This function should be called for its printed output
the_2x2_table_CIs_OR <- function(n, alpha = 0.05) {
  validateArguments(mget(ls()))

  pi1hat <- n[1, 1] / (n[1, 1] + n[1, 2])
  pi2hat <- n[2, 1] / (n[2, 1] + n[2, 2])
  thetahat <- n[1, 1] * n[2, 2] / (n[1, 2] * n[2, 1])

  cat_sprintf("Estimate of pi_1: %i / %i = %5.3f\n", n[1, 1], n[1, 1] + n[1, 2], pi1hat)
  cat_sprintf("Estimate of pi_2: %i / %i = %5.3f\n", n[2, 1], n[2, 1] + n[2, 2], pi2hat)
  cat_sprintf("Estimate of theta = (pi_1 / (1-pi_1)) / (pi_2 / (1-pi_2)): %5.3f\n\n", thetahat)

cat_sprintf("Interval method                            %i%% CI      Log width\n", 100 * (1 - alpha))
  cat("----------------------------------------------------------------\n")

  res <- Woolf_logit_CI_2x2(n, alpha)
  L <- res$lower
  U <- res$upper
  cat_sprintf("Woolf logit                           %6.3f to %6.3f  %7.3f\n", L, U, log(U) - log(L))

  res <- Gart_adjusted_logit_CI_2x2(n, alpha)
  L <- res$lower
  U <- res$upper
  cat_sprintf("Gart adjusted logit                   %6.3f to %6.3f  %7.3f\n", L, U, log(U) - log(L))

  res <- Independence_smoothed_logit_CI_2x2(n, alpha)
  L <- res$lower
  U <- res$upper
  cat_sprintf("Independence-smoothed logit           %6.3f to %6.3f  %7.3f\n", L, U, log(U) - log(L))

  res <- Inv_sinh_CI_OR_2x2(n, alpha)
  L <- res$lower
  U <- res$upper
  cat_sprintf("Inverse sinh                          %6.3f to %6.3f  %7.3f\n", L, U, log(U) - log(L))

  res <- Adjusted_inv_sinh_CI_OR_2x2(n, 0.45, 0.25, alpha)
  L <- res$lower
  U <- res$upper
  cat_sprintf("Adjusted inverse sinh (0.45, 0.25)    %6.3f to %6.3f  %7.3f\n", L, U, log(U) - log(L))

  res <- Adjusted_inv_sinh_CI_OR_2x2(n, 0.6, 0.4, alpha)
  L <- res$lower
  U <- res$upper
  cat_sprintf("Adjusted inverse sinh (0.6, 0.4)      %6.3f to %6.3f  %7.3f\n", L, U, log(U) - log(L))

  res <- MOVER_R_Wilson_CI_OR_2x2(n, alpha)
  L <- res$lower
  U <- res$upper
  cat_sprintf("MOVER-R Wilson                        %6.3f to %6.3f  %7.3f\n", L, U, log(U) - log(L))

  res <- MiettinenNurminen_asymptotic_score_CI_OR_2x2(n, alpha)
  L <- res$lower
  U <- res$upper
  cat_sprintf("Miettinen-Nurminen asymptotic score   %6.3f to %6.3f  %7.3f\n", L, U, log(U) - log(L))

  res <- Uncorrected_asymptotic_score_CI_2x2(n, alpha)
  L <- res$lower
  U <- res$upper
  cat_sprintf("Uncorrected asymptotic score          %6.3f to %6.3f  %7.3f\n", L, U, log(U) - log(L))

  res <- Cornfield_exact_conditional_CI_2x2(n, alpha)
  L <- res$lower
  U <- res$upper
  cat_sprintf("Cornfield exact conditional           %6.3f to %6.3f  %7.3f\n", L, U, log(U) - log(L))

  res <- BaptistaPike_exact_conditional_CI_2x2(n, alpha)
  L <- res$lower
  U <- res$upper
  cat_sprintf("Baptista-Pike exact conditional       %6.3f to %6.3f  %7.3f\n", L, U, log(U) - log(L))

  res <- Cornfield_midP_CI_2x2(n, alpha)
  L <- res$lower
  U <- res$upper
  cat_sprintf("Cornfield mid-P                       %6.3f to %6.3f  %7.3f\n", L, U, log(U) - log(L))

  res <- BaptistaPike_midP_CI_2x2(n, alpha)
  L <- res$lower
  U <- res$upper
  cat_sprintf("Baptista-Pike mid-P                   %6.3f to %6.3f  %7.3f\n", L, U, log(U) - log(L))

  cat("----------------------------------------------------------------\n")
  invisible(NULL)
}
