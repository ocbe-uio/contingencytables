#' @title The MOVER-R Wilson confidence interval for the ratio of probabilities
#' @description The MOVER-R Wilson confidence interval for the ratio of probabilities
#' @description Described in Chapter 4 "The 2x2 Table"
#' @param n the observed counts (a 2x2 matrix)
#' @param alpha the nominal level, e.g. 0.05 for 95% CIs
#' @examples
#' # An RCT of high vs standard dose of epinephrine (Perondi et al., 2004)
#' MOVER_R_Wilson_CI_ratio_2x2(perondi_2004)
#'
#' # The association between CHRNA4 genotype and XFS (Ritland et al., 2007)
#' MOVER_R_Wilson_CI_ratio_2x2(ritland_2007)
#'
#' @export
#' @return An object of the [contingencytables_result] class,
#' basically a subclass of [base::list()]. Use the [utils::str()] function
#' to see the specific elements returned.
MOVER_R_Wilson_CI_ratio_2x2 <- function(n, alpha = 0.05) {
  validateArguments(mget(ls()))

  n1p <- n[1, 1] + n[1, 2]
  n2p <- n[2, 1] + n[2, 2]

  # Estimates of the two probabilities of success
  pi1hat <- n[1, 1] / n1p
  pi2hat <- n[2, 1] / n2p

  # Estimate of the ratio of probabilities (phihat)
  estimate <- pi1hat / pi2hat

  # Use Wilson score CIs for the two probabilities of success
  res1 <- Wilson_score_CI_1x2(n[1, 1], n1p, alpha)
  res2 <- Wilson_score_CI_1x2(n[2, 1], n2p, alpha)
  L <- (pi1hat * pi2hat - sqrt((pi1hat * pi2hat)^2 - res1[["lower"]] * res2[["upper"]] * (2 * pi1hat - res1[["lower"]]) * (2 * pi2hat - res2[["upper"]]))) / (res2[["upper"]] * (2 * pi2hat - res2[["upper"]]))
  U <- (pi1hat * pi2hat + sqrt((pi1hat * pi2hat)^2 - res1[["upper"]] * res2[["lower"]] * (2 * pi1hat - res1[["upper"]]) * (2 * pi2hat - res2[["lower"]]))) / (res2[["lower"]] * (2 * pi2hat - res2[["lower"]]))

  # Fix limits for some special cases
  if (is.na(L)) {
    L <- 0
  }
  if (is.na(U) || U < 0) {
    U <- Inf
  }
  L <- max(c(0, L))

  return(
    contingencytables_result(
      list(lower = L, upper = U, estimate = estimate),
      sprintf(
        "The MOVER-R Wilson CI: estimate = %6.4f (%g%% CI %6.4f to %6.4f)",
        estimate, 100 * (1 - alpha), L, U
      )
    )
  )
}
