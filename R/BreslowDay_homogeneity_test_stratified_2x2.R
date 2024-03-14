#' @title The Breslow-Day test of homogeneity of odds ratios over strata
#' @description The Breslow-Day test of homogeneity of odds ratios over strata with
#' @description Tarone correction
#' @description Described in Chapter 10 "Stratified 2x2 Tables and Meta-Analysis"
#' @param n the observed table (a 2x2xk matrix, where k is the number of strata)
#' @return An object of the [contingencytables_result] class,
#' basically a subclass of [base::list()]. Use the [utils::str()] function
#' to see the specific elements returned.
#' @examples
#' BreslowDay_homogeneity_test_stratified_2x2(doll_hill_1950)
#' BreslowDay_homogeneity_test_stratified_2x2(hine_1989)
#' @export
BreslowDay_homogeneity_test_stratified_2x2 <- function(n) {
  validateArguments(mget(ls()))
  if (length(dim(n)) != 3) {
    stop("n must have 3 dimensions")
  }
  n11k <- n[1, 1, ]
  n1pk <- apply(n[1, , ], 2, sum)
  np1k <- apply(n[, 1, ], 2, sum)
  n2pk <- apply(n[2, , ], 2, sum)
  K <- dim(n)[3]

  # Get the Mantel-Haenszel overall estimate
  thetahatMH <- MantelHaenszel_estimate_stratified_2x2(n, "logit")[[1]]

  # Find the expected cell counts in cell [1, 1] in each stratum (m11k) by
  # solving a quadratic equation
  sk <- n2pk - np1k + thetahatMH * (np1k + n1pk)
  r <- 1 - thetahatMH
  tk <- -thetahatMH * n1pk * np1k
  solution1 <- (-sk + sqrt(sk^2 - 4 * r * tk)) / (2 * r)
  solution2 <- (-sk - sqrt(sk^2 - 4 * r * tk)) / (2 * r)
  m11k <- rep(0, K)
  for (k in 1:K) {
    m11k[k] <- ifelse(
      test = solution1[k] > 0 && solution1[k] < n1pk[k] && solution1[k] < np1k[k],
      yes = solution1[k],
      no = solution2[k]
    )
  }

  # Estimate of the variance of n11k under the assumption of a common odds ratio
  v11k <- 1 / (1 / m11k + 1 / (np1k - m11k) + 1 / (n1pk - m11k) + 1 / (n2pk - np1k + m11k))

  # The Breslow-Day test statistic (with Tarone correction)
  T0 <- sum(((n11k - m11k)^2) / v11k) - ((sum(n11k) - sum(m11k))^2) / sum(v11k)

  # The two-sided P-value (reference distribution: chi-squared with K - 1
  # degrees of freedom)
  df <- K - 1
  P <- 1 - pchisq(T0, df)

  # Output
  printresults <- function() {
    cat_sprintf(
      "The Breslow-Day test: P = %7.6f, T0 = %5.3f (df = %g)", P, T0, df
    )
  }
  return(
    contingencytables_result(
      list("Pvalue" = P, "T" = T0, "df" = df),
      printresults
    )
  )
}
