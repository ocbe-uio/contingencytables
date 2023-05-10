#' @title The Kruskal-Wallis asymptotic test for singly ordered rxc tables
#' @description The Kruskal-Wallis asymptotic test for singly ordered rxc tables
#' @description Described in Chapter 7 "The rxc Table"
#' @param n the observed counts (an rxc matrix)
#' @examples
#' KruskalWallis_asymptotic_test_rxc(table_7.5)
#' KruskalWallis_asymptotic_test_rxc(table_7.6)
#' @export
#' @return An object of the [contingencytables_result] class,
#' basically a subclass of [base::list()]. Use the [utils::str()] function
#' to see the specific elements returned.
KruskalWallis_asymptotic_test_rxc <- function(n) {
  validateArguments(mget(ls()))

  r <- nrow(n)
  c <- ncol(n)
  nip <- apply(n, 1, sum)
  npj <- apply(n, 2, sum)
  N <- sum(n)

  # The midranks
  midranks <- rep(0, c)
  for (j in 1:c) {
    if (j > 1) {
      midranks[j] <- sum(npj[1:(j - 1)]) + (1 + npj[j]) / 2
    } else {
      midranks[j] <- (1 + npj[j]) / 2
    }
  }

  # The rank sum in each row
  W <- n %*% midranks

  # Correction term for ties
  CorrectionTerm <- 1 - sum(npj^3 - npj) / (N^3 - N)

  # The Kruskal-Wallis test statistic
  T0 <- 0
  for (i in 1:r) {
    T0 <- T0 + nip[i] * (W[i] / nip[i] - (N + 1) / 2)^2
  }

  # The two-sided P-value (reference distribution: chi-squared with (r-1) degrees of freedom)
  T0 <- T0 * 12 / (N * (N + 1) * CorrectionTerm)
  df <- r - 1
  P <- 1 - pchisq(T0, df)

  printresults <- function() {
    for (i in 1:r) {
      my_sprintf_cat("(average rank in row %i: W_%i = %7.4f)\n", i, i, W[i] / nip[i])
    }
    my_sprintf_cat("(correction term for ties: C_ties = %6.4f)\n", CorrectionTerm)
    my_sprintf_cat("Asymptotic Kruskal-Wallis test: T = %6.3f, df = %g, P = %7.5f", T0, df, P)
  }

  return(contingencytables_result(list(P = P, T = T0, df = df), printresults))
}
