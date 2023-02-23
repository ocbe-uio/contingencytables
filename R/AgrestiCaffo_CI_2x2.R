#' @title The Agresti-Caffo confidence interval for the difference between probabilities
#' @description The Agresti-Caffo confidence interval for the difference between probabilities
#' @description Described in Chapter 4 "The 2x2 Table"
#' @param n the observed counts (a 2x2 matrix)
#' @param alpha the nominal level, e.g. 0.05 for 95% CIs
#' @return A data frame containing lower, upper and point estimates of the statistic
#' @examples
#' AgrestiCaffo_CI_2x2(perondi_2004)
#' AgrestiCaffo_CI_2x2(ritland_2007)
#' @export
AgrestiCaffo_CI_2x2 <- function(n, alpha = 0.05) {
  validateArguments(mget(ls()))
  # Estimate of the difference between probabilities (deltahat)
  estimate <- n[1, 1] / (n[1, 1] + n[1, 2]) - n[2, 1] / (n[2, 1] + n[2, 2])

  # Add one success and one failure in each group and calculate the Wald CI
  res.wald <- Wald_CI_2x2(n + 1, alpha, printresults = FALSE)

  # Output
  res <- list(
    name = "The Agresti-Caffo CI",
    statistics = list(
      "lower" = res.wald$lower, "upper" = res.wald$upper, "estimate" = estimate,
      "alpha" = alpha, "statname" = "estimate"
    )
  )
  return(contingencytables_result2(res$statistics, fetch_print_format(res)))
}
