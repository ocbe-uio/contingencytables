#' @title The Breslow-Day test of homogeneity of odds ratios over strata
#' @description The Breslow-Day test of homogeneity of odds ratios over strata with
#' @description Tarone correction
#' @description Described in Chapter 10 "Stratified 2x2 Tables and Meta-Analysis"
#' @param n the observed table (a 2x2xk matrix, where k is the number of strata)
#' @param printresults display results (FALSE = no, TRUE = yes)
#' @return A list containing lower bound, upper bound and differences of the statistic
#' @examples
#' # Smoking and lung cancer (Doll and Hill, 1950)
#' n <- array(dim = c(2, 2, 2))
#' n[, , 1] <- matrix(c(647, 622, 2, 27), 2, byrow = TRUE)
#' n[, , 2] <- matrix(c(41, 28, 19, 32), 2, byrow = TRUE)
#' BreslowDay_homogeneity_test_stratified_2x2(n)
#'
#' # Prophylactice use of Lidocaine in myocardial infarction (Hine et al., 1989)
#' n <- array(0, dim = c(2, 2, 6))
#' n[, , 1] <- rbind(c(2, 37), c(1, 42))
#' n[, , 2] <- rbind(c(4, 40), c(4, 40))
#' n[, , 3] <- rbind(c(6, 101), c(4, 106))
#' n[, , 4] <- rbind(c(7, 96), c(5, 95))
#' n[, , 5] <- rbind(c(7, 103), c(3, 103))
#' n[, , 6] <- rbind(c(11, 143), c(4, 142))
#' BreslowDay_homogeneity_test_stratified_2x2(n)
#'
#' @export
BreslowDay_homogeneity_test_stratified_2x2 <- function(n, printresults = TRUE) {
  n11k <- n[1, 1, ]
  n1pk <- apply(n[1, , ], 2, sum)
  np1k <- apply(n[, 1, ], 2, sum)
  n2pk <- apply(n[2, , ], 2, sum)
  K <- dim(n)[3]

  # Get the Mantel-Haenszel overall estimate
  thetahatMH <- MantelHaenszel_estimate_stratified_2x2(n, "logit", FALSE)[[1]]

  # Find the expected cell counts in cell [1, 1] in each stratum (m11k) by
  # solving a quadratic equation
  sk <- n2pk - np1k + thetahatMH * (np1k + n1pk)
  r <- 1 - thetahatMH
  tk <- -thetahatMH * n1pk * np1k
  solution1 <- (-sk + sqrt(sk^2 - 4 * r * tk)) / (2 * r)
  solution2 <- (-sk - sqrt(sk^2 - 4 * r * tk)) / (2 * r)
  m11k <- rep(0, K)
  for (k in 1:K) {
    if (solution1[k] > 0 && solution1[k] < n1pk[k] && solution1[k] < np1k[k]) {
      m11k[k] <- solution1[k]
    } else {
      m11k[k] <- solution2[k]
    }
  }

  # Estimate of the variance of n11k under the assumption of a common odds ratio
  v11k <- 1 / (1 / m11k + 1 / (np1k - m11k) + 1 / (n1pk - m11k) + 1 / (n2pk - np1k + m11k))

  # The Breslow-Day test statistic (with Tarone correction)
  T0 <- sum(((n11k - m11k)^2) / v11k) - ((sum(n11k) - sum(m11k))^2) / sum(v11k)

  # The two-sided P-value (reference distribution: chi-squared with K - 1
  # degrees of freedom)
  df <- K - 1
  P <- 1 - pchisq(T0, df)

  if (printresults) {
    .print("The Breslow-Day test: P = %7.5f, T0 = %5.3f (df = %i)\n", P, T0, df)
  }

  invisible(list(P = P, T = T0, df = df))
}


.print <- function(s, ...) {
  print(sprintf(gsub("\n", "", s), ...), quote = FALSE)
}
