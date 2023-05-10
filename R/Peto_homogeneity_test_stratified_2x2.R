#' @title The Peto test for homogeneity of odds ratios over strata
#' @description The Peto test for homogeneity of odds ratios over strata
#' @description Described in Chapter 10 "Stratified 2x2 Tables and Meta-Analysis"
#' @param n the observed table (a 2x2xk matrix, where k is the number of strata)
#' @examples
#' # Smoking and lung cancer (Doll and Hill, 1950)
#' Peto_homogeneity_test_stratified_2x2(doll_hill_1950)
#'
#' # Prophylactice use of Lidocaine in myocardial infarction (Hine et al., 1989)
#' Peto_homogeneity_test_stratified_2x2(hine_1989)
#'
#' @export

Peto_homogeneity_test_stratified_2x2 <- function(n) {
  validateArguments(mget(ls()))

  n1pk <- apply(n[1, , ], 2, sum)
  np1k <- apply(n[, 1, ], 2, sum)
  n2pk <- apply(n[2, , ], 2, sum)
  np2k <- apply(n[, 2, ], 2, sum)
  nppk <- apply(n, 3, sum)
  K <- dim(n)[3]

  # The conditional expectation (from the hypergeomtric distribution)
  expectation <- n1pk * np1k / nppk

  # The variance (from the hypergeomtric distribution)
  variance <- n1pk * n2pk * np1k * np2k / ((nppk^2) * (nppk - 1))

  # The Peto odds ratio estimate in each stratum (on the log scale)
  log_theta_k <- (n[1, 1, ] - expectation) / variance

  # The overall Peto odds ratio estimate (on the log scale)
  log_estimate <- sum(n[1, 1, ] - expectation) / sum(variance)

  # The Peto test statistic
  T0 <- sum(variance * (log_theta_k - log_estimate)^2)

  # The two-sided P-value (reference distribution: chi-squared with K - 1
  # degrees of freedom)
  df <- K - 1
  P <- 1 - pchisq(T0, df)

  return(
    contingencytables_result(
      list(P = P, T = T0, df = df),
      sprintf("The Peto test: P = %7.5f, T0 = %5.3f (df = %i)", P, T0, df)
    )
  )
}
