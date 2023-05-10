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

multiple_hypergeomtric_pdf <- function(x, N, r, c, nip, npj) {
  if (any(max(x) > 170, nip > 170, npj > 170)) {
    return(NA)
  }
  # Somewhat messy code to avoid overflow
  if (N > 170) {
    cutoff <- 170
  } else {
    cutoff <- floor(N / 2)
  }
  Nfact1 <- factorial(cutoff)
  Nfact2 <- 1
  for (i in (cutoff + 1):N) {
    Nfact2 <- Nfact2 * i
  }
  terms1 <- factorial(npj)
  terms2 <- factorial(nip)
  f <- 1 / Nfact1
  f <- f * terms1[1]
  f <- f * prod(terms1[-1])
  f <- f / Nfact2
  f <- f * terms2[1]
  f <- f * prod(terms2[-1])
  for (i in 1:r) {
    for (j in 1:c) {
      f <- f / factorial(x[i, j])
    }
  }
  return(f)
}
