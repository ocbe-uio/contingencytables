#' @title The Transformed Wilson score confidence interval for the conditional
#' odds ratio
#' @description The Transformed Wilson score confidence interval for the
#' conditional odds ratio
#' @description Described in Chapter 8 "The Paired 2x2 Table"
#' @param n the observed counts (a 2x2 matrix)
#' @param alpha the nominal level, e.g. 0.05 for 95% CIs
#' @param printresults display results (FALSE = no, TRUE = yes)
#' @examples
#' Transformed_Wilson_score_CI_paired_2x2(ezra_2010)
#' @export
#' @return A list containing lower, upper and point estimates of the statistic
Transformed_Wilson_score_CI_paired_2x2 <- function(
  n, alpha = 0.05, printresults = TRUE
) {
  validateArguments(mget(ls()))

  # Estimate of the conditional odds ratio (thetacondhat)
  estimate <- n[1, 2] / n[2, 1]

  # The Wilson score interval for mu <- pi_12 / (pi_12 + pi_21)
  tmp <- Wilson_score_CI_1x2(n[1, 2], n[1, 2] + n[2, 1], alpha, FALSE)
  L_mu <- tmp[[1]]
  U_mu <- tmp[[2]]

  # Transform the confidence limits back to the conditional odds ratio scale
  L <- L_mu / (1 - L_mu)
  U <- U_mu / (1 - U_mu)

  if (printresults) {
    my_sprintf(
      paste(
        "The transformed Wilson score CI: estimate =",
        "%6.4f (%g%% CI %6.4f to %6.4f)\n"
      ),
      estimate, 100 * (1 - alpha), L, U
    )
  }

  invisible(list(L = L, U = U, estimate = estimate))
}
