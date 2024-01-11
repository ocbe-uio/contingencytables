#' @title The Cochran-Mantel-Haenszel test of a common odds ratio
#' @description The Cochran-Mantel-Haenszel test of a common odds ratio
#' @description Described in Chapter 10 "Stratified 2x2 Tables and Meta-Analysis"
#' @param n the observed table (a 2x2xk matrix, where k is the number of strata)
#' @examples
#' CochranMantelHaenszel_test_stratified_2x2(doll_hill_1950)
#' CochranMantelHaenszel_test_stratified_2x2(hine_1989)
#' @export
#' @return An object of the [contingencytables_result] class,
#' basically a subclass of [base::list()]. Use the [utils::str()] function
#' to see the specific elements returned.
CochranMantelHaenszel_test_stratified_2x2 <- function(n) {
  validateArguments(mget(ls()))
  n1pk <- apply(n[1, , ], 2, sum)
  np1k <- apply(n[, 1, ], 2, sum)
  n2pk <- apply(n[2, , ], 2, sum)
  np2k <- apply(n[, 2, ], 2, sum)
  nppk <- apply(n, 3, sum)

  # The Cochran-Mantel-Haenszel test statistic
  numerator <- (sum((n[1, 1, ] * n[2, 2, ] - n[1, 2, ] * n[2, 1, ]) / nppk))^2
  denominator <- sum((n1pk * n2pk * np1k * np2k) / ((nppk^2) * (nppk - 1)))
  T0 <- numerator / denominator

  # The two-sided P-value (reference distribution: chi-squared with one
  # degree of freedom)
  df <- 1
  P <- 1 - pchisq(T0, df)

  # Output
  printresults <- function() {
    cat_sprintf(
      "The Cochran-Mantel-Haenszel test: P = %7.6f, T0 = %5.3f (df = %g)",
      P, T0, df
    )
  }
  return(
    contingencytables_result(
      list("pvalue" = P, "T" = T0, "df" = df),
      printresults
    )
  )
}
