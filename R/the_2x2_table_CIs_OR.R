#' @title The 2x2 table CIs odds ratio
#' @description Wrapper for \code{_CI_OR_2x2} functions on Chapter 4.
#' @param n frequency matrix
#' @param alpha type I error
#' @examples
#' # Example: A lady tasting a cup of tea
#' n <- rbind(c(3, 1), c(1, 3))
#' the_2x2_table_CIs_OR(n)
#'
#' # Example: Perondi et al. (2004)
#' n <- rbind(c(7, 27), c(1, 33))
#' the_2x2_table_CIs_OR(n)
#'
#' # Example: Lampasona et al. (2013)
#' n <- rbind(c(9, 4), c(4, 10))
#' the_2x2_table_CIs_OR(n)
#'
#' # Example: Ritland et al. (2007)
#' n <- rbind(c(0, 16), c(15, 57))
#' the_2x2_table_CIs_OR(n)
#'
#' @export
#' @return A string of "-". This function should be called for its printed output
the_2x2_table_CIs_OR <- function(n, alpha = 0.05) {
  pi1hat <- n[1, 1] / (n[1, 1] + n[1, 2])
  pi2hat <- n[2, 1] / (n[2, 1] + n[2, 2])
  thetahat <- n[1, 1] * n[2, 2] / (n[1, 2] * n[2, 1])

  print(sprintf("Estimate of pi_1: %i / %i = %5.3f", n[1, 1], n[1, 1] + n[1, 2], pi1hat), quote = FALSE)
  print(sprintf("Estimate of pi_2: %i / %i = %5.3f", n[2, 1], n[2, 1] + n[2, 2], pi2hat), quote = FALSE)
  print(sprintf("Estimate of theta = (pi_1 / (1-pi_1)) / (pi_2 / (1-pi_2)): %5.3f", thetahat), quote = FALSE)

  print(sprintf("Interval method                            %i%% CI      Log width", 100 * (1 - alpha)), quote = FALSE)
  print("----------------------------------------------------------------", quote = FALSE)

  res <- Woolf_logit_CI_2x2(n, alpha, printresults = FALSE)
  L <- res$lower
  U <- res$upper
  print(sprintf("Woolf logit                           %6.3f to %6.3f  %7.3f", L, U, log(U) - log(L)), quote = FALSE)

  res <- Gart_adjusted_logit_CI_2x2(n, alpha, printresults = FALSE)
  L <- res$lower
  U <- res$upper
  print(sprintf("Gart adjusted logit                   %6.3f to %6.3f  %7.3f", L, U, log(U) - log(L)), quote = FALSE)

  res <- Independence_smoothed_logit_CI_2x2(n, alpha, printresults = FALSE)
  L <- res$lower
  U <- res$upper
  print(sprintf("Independence-smoothed logit           %6.3f to %6.3f  %7.3f", L, U, log(U) - log(L)), quote = FALSE)

  res <- Inv_sinh_CI_OR_2x2(n, alpha, printresults = FALSE)
  L <- res$lower
  U <- res$upper
  print(sprintf("Inverse sinh                          %6.3f to %6.3f  %7.3f", L, U, log(U) - log(L)), quote = FALSE)

  res <- Adjusted_inv_sinh_CI_OR_2x2(n, 0.45, 0.25, alpha, printresults = FALSE)
  L <- res$lower
  U <- res$upper
  print(sprintf("Adjusted inverse sinh (0.45, 0.25)    %6.3f to %6.3f  %7.3f", L, U, log(U) - log(L)), quote = FALSE)

  res <- Adjusted_inv_sinh_CI_OR_2x2(n, 0.6, 0.4, alpha, printresults = FALSE)
  L <- res$lower
  U <- res$upper
  print(sprintf("Adjusted inverse sinh (0.6, 0.4)      %6.3f to %6.3f  %7.3f", L, U, log(U) - log(L)), quote = FALSE)

  res <- MOVER_R_Wilson_CI_OR_2x2(n, alpha, printresults = FALSE)
  L <- res$lower
  U <- res$upper
  print(sprintf("MOVER-R Wilson                        %6.3f to %6.3f  %7.3f", L, U, log(U) - log(L)), quote = FALSE)

  res <- MiettinenNurminen_asymptotic_score_CI_OR_2x2(n, alpha, printresults = FALSE)
  L <- res$lower
  U <- res$upper
  print(sprintf("Miettinen-Nurminen asymptotic score   %6.3f to %6.3f  %7.3f", L, U, log(U) - log(L)), quote = FALSE)

  res <- Uncorrected_asymptotic_score_CI_2x2(n, alpha, printresults = FALSE)
  L <- res$lower
  U <- res$upper
  print(sprintf("Uncorrected asymptotic score          %6.3f to %6.3f  %7.3f", L, U, log(U) - log(L)), quote = FALSE)

  res <- Cornfield_exact_conditional_CI_2x2(n, alpha, printresults = FALSE)
  L <- res$lower
  U <- res$upper
  print(sprintf("Cornfield exact conditional           %6.3f to %6.3f  %7.3f", L, U, log(U) - log(L)), quote = FALSE)

  res <- BaptistaPike_exact_conditional_CI_2x2(n, alpha, printresults = FALSE)
  L <- res$lower
  U <- res$upper
  print(sprintf("Baptista-Pike exact conditional       %6.3f to %6.3f  %7.3f", L, U, log(U) - log(L)), quote = FALSE)

  res <- Cornfield_midP_CI_2x2(n, alpha, printresults = FALSE)
  L <- res$lower
  U <- res$upper
  print(sprintf("Cornfield mid-P                       %6.3f to %6.3f  %7.3f", L, U, log(U) - log(L)), quote = FALSE)

  res <- BaptistaPike_midP_CI_2x2(n, alpha, printresults = FALSE)
  L <- res$lower
  U <- res$upper
  print(sprintf("Baptista-Pike mid-P                   %6.3f to %6.3f  %7.3f", L, U, log(U) - log(L)), quote = FALSE)

  print("----------------------------------------------------------------", quote = FALSE)
}
