#' @title The Peto estimate of the common odds ratio across strata
#' @description The Peto estimate of the common odds ratio across strata
#' @description Described in Chapter 10 "Stratified 2x2 Tables and Meta-Analysis"
#' @param n the observed table (a 2x2xk matrix, where k is the number of strata)
#' @examples
#' # Smoking and lung cancer (Doll and Hill, 1950)
#' Peto_OR_estimate_stratified_2x2(doll_hill_1950)
#'
#' # Prophylactice use of Lidocaine in myocardial infarction (Hine et al., 1989)
#' Peto_OR_estimate_stratified_2x2(hine_1989)
#'
#' @export
#' @return An object of the [contingencytables_result] class,
#' basically a subclass of [base::list()]. Use the [utils::str()] function
#' to see the specific elements returned.
Peto_OR_estimate_stratified_2x2 <- function(n) {
  validateArguments(mget(ls()))

  n1pk <- apply(n[1, , ], 2, sum)
  np1k <- apply(n[, 1, ], 2, sum)
  n2pk <- apply(n[2, , ], 2, sum)
  np2k <- apply(n[, 2, ], 2, sum)
  nppk <- apply(n, 3, sum)

  # The conditional expectation (from the hypergeomtric distribution)
  expectation <- n1pk * np1k / nppk

  # The variance (from the hypergeomtric distribution)
  variance <- n1pk * n2pk * np1k * np2k / ((nppk^2) * (nppk - 1))

  # The Peto odds ratio estimate
  estimate <- exp(sum(n[1, 1, ] - expectation) / sum(variance))

  return(
    contingencytables_result(
      list(estimate = estimate, expectation = expectation, variance = variance),
      sprintf("The Peto OR estimate = %7.4f", estimate)
    )
  )
}
