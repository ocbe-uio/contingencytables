#' @title The Fisher-Freeman-Halton asymptotic test for unordered rxc tables
#' @description The Fisher-Freeman-Halton asymptotic test for unordered rxc tables
#' @description Described in Chapter 7 "The rxc Table"
#' @param n the observed counts (an rxc matrix)
#' @examples
#' FisherFreemanHalton_asymptotic_test_rxc(table_7.3)
#' @note May not give results for all tables, due to overflow
#' @export
#' @return An object of the [contingencytables_result] class,
#' basically a subclass of [base::list()]. Use the [utils::str()] function
#' to see the specific elements returned.
FisherFreemanHalton_asymptotic_test_rxc <- function(n) {
  validateArguments(mget(ls()))
  r <- nrow(n)
  c <- ncol(n)
  nip <- apply(n, 1, sum)
  npj <- apply(n, 2, sum)
  N <- sum(n)

  # Point probability of the observed table
  f <- multiple_hypergeomtric_pdf(n, N, r, c, nip, npj)

  gamma <- sqrt(((2 * pi)^((r - 1) * (c - 1))) * (N^(-((r * c) - 1))) * prod(nip^(c - 1)) * prod(npj^(r - 1)))
  if (sum(npj == 0) > 0) {
    gamma <- 1
  }

  # Test statistic and P-value from the chi-squared distribution with
  # (r-1)(c-1) degrees of freedom
  T0 <- -2 * log(gamma * f)
  df <- (r - 1) * (c - 1)
  P <- 1 - pchisq(T0, df)

  return(
    contingencytables_result(
      list("P" = P, "T" = T0, "df" = df),
      sprintf(
        "Fisher-Freeman-Halton asymptotic test: P = %6.4f, T = %5.3f (df=%g)",
        P, T0, df
      )
    )
  )
}
