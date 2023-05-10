#' @title The Agresti-Caffo confidence interval for the difference between probabilities
#' @description The Agresti-Caffo confidence interval for the difference between probabilities
#' @description Described in Chapter 4 "The 2x2 Table"
#' @param n the observed counts (a 2x2 matrix)
#' @param alpha the nominal level, e.g. 0.05 for 95% CIs
#' @return An object of the [contingencytables_result] class,
#' basically a subclass of [base::list()]. Use the [utils::str()] function
#' to see the specific elements returned.
#' @examples
#' AgrestiCaffo_CI_2x2(perondi_2004)
#' AgrestiCaffo_CI_2x2(ritland_2007)
#' @export
AgrestiCaffo_CI_2x2 <- function(n, alpha = 0.05) {
  validateArguments(mget(ls()))
  # Estimate of the difference between probabilities (deltahat)
  estimate <- n[1, 1] / (n[1, 1] + n[1, 2]) - n[2, 1] / (n[2, 1] + n[2, 2])

  # Add one success and one failure in each group and calculate the Wald CI
  res.wald <- Wald_CI_2x2(n + 1, alpha)

  # Output
  printresults <- function() {
    my_sprintf_cat(
      "The Agresti-Caffo CI: estimate = %6.4f (%g%% CI %6.4f to %6.4f)",
      estimate, 100 * (1 - alpha), res.wald$lower, res.wald$upper
    )
  }

  return(
    contingencytables_result(
      list(lower = res.wald$lower, upper = res.wald$upper, estimate = estimate),
      printresults
    )
  )
}
