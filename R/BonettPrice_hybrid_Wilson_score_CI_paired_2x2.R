#' @title The Bonett-Price hybrid Wilson score confidence interval for the ratio of paired probabilities
#' @description The Bonett-Price hybrid Wilson score confidence interval for the ratio of paired probabilities
#' @description Described in Chapter 8 "The Paired 2x2 Table"
#' @param n the observed counts (a 2x2 matrix)
#' @param alpha the nominal level, e.g. 0.05 for 95# CIs
#' @return An object of the [contingencytables_result] class,
#' basically a subclass of [base::list()]. Use the [utils::str()] function
#' to see the specific elements returned.
#' @examples
#' BonettPrice_hybrid_Wilson_score_CI_paired_2x2(bentur_2009)
#' BonettPrice_hybrid_Wilson_score_CI_paired_2x2(cavo_2012)
#' @export
BonettPrice_hybrid_Wilson_score_CI_paired_2x2 <- function(n, alpha = 0.05) {
  validateArguments(mget(ls()))
  nstar <- n[1, 1] + n[1, 2] + n[2, 1]
  n1p <- n[1, 1] + n[1, 2]
  np1 <- n[1, 1] + n[2, 1]

  # Estimate of the ratio of probabilities (phihat)
  estimate <- (n[1, 1] + n[1, 2]) / (n[1, 1] + n[2, 1])

  A <- sqrt((n[1, 2] + n[2, 1] + 2) / ((n1p + 1) * (np1 + 1)))
  B <- sqrt((1 - (n1p + 1) / (nstar + 2)) / (n1p + 1))
  C <- sqrt((1 - (np1 + 1) / (nstar + 2)) / (np1 + 1))
  z <- (A / (B + C)) * qnorm(1 - alpha / 2, 0, 1)

  # The Wilson score interval for pi_1+ (with adjustment to z)
  l1 <- (2 * n1p + z^2 - z * sqrt(z^2 + 4 * n1p * (1 - n1p / nstar))) / (2 * (nstar + z^2))
  u1 <- (2 * n1p + z^2 + z * sqrt(z^2 + 4 * n1p * (1 - n1p / nstar))) / (2 * (nstar + z^2))

  # The Wilson score interval for pi_ + 1 (with adjustment to z)
  l2 <- (2 * np1 + z^2 - z * sqrt(z^2 + 4 * np1 * (1 - np1 / nstar))) / (2 * (nstar + z^2))
  u2 <- (2 * np1 + z^2 + z * sqrt(z^2 + 4 * np1 * (1 - np1 / nstar))) / (2 * (nstar + z^2))

  # Combine the Wilson score limits for pi_1+ and pi_ + 1 to obtain the
  # confidence limits for the ratio pi_1 + /pi_ + 1
  L <- l1 / u2
  U <- u1 / l2

  if (nstar == 0) {
    L <- 0
    U <- Inf
  }

  # Output
  printresults <- function() {
    cat_sprintf(
      "The Bonett-Price hybrid Wilson score CI: estimate = %6.4f (%g%% CI %6.4f to %6.4f)",
      estimate, 100 * (1 - alpha), L, U
    )
  }
  return(
    contingencytables_result(
      list(lower = L, upper = U, estimate = estimate),
      printresults
    )
  )
}
