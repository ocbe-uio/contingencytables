#' @title The Pearson chi-squared test for association in 2x2 tables
#' @description The Pearson chi-squared test for association in 2x2 tables
#' @description Described in Chapter 4 "The 2x2 Table"
#' @param n the observed counts (a 2x2 matrix)
#' @examples
#' # Example: A lady tasting a cup of tea
#' Pearson_chi_squared_test_2x2(tea)
#'
#' # Example: Perondi et al. (2004)
#' Pearson_chi_squared_test_2x2(perondi_2004)
#'
#' # Example: Lampasona et al. (2013)
#' Pearson_chi_squared_test_2x2(lampasona_2013)
#'
#' # Example: Ritland et al. (2007)
#' Pearson_chi_squared_test_2x2(ritland_2007)
#'
#' @export
#' @return An object of the [contingencytables_result] class,
#' basically a subclass of [base::list()]. Use the [utils::str()] function
#' to see the specific elements returned.
Pearson_chi_squared_test_2x2 <- function(n) {
  validateArguments(mget(ls()))

  n1p <- n[1, 1] + n[1, 2]
  n2p <- n[2, 1] + n[2, 2]
  np1 <- n[1, 1] + n[2, 1]
  np2 <- n[1, 2] + n[2, 2]
  N <- sum(n)

  # The Pearson chi-squared statistic
  T0 <- (N * (n[1, 1] * n[2, 2] - n[1, 2] * n[2, 1])^2) / (n1p * n2p * np1 * np2)

  # The two-sided P-value (reference distribution: chi-squared with 1 degree of freedom)
  df <- 1
  P <- 1 - pchisq(T0, df)

  # Handle cases where the P-value is not computable
  if (is.na(P)) {
    P <- 1.0
  }

  return(
    contingencytables_result(
      list(p.value = P, statistic = T0, df = df),
      sprintf("The Pearson chi-squared test: P = %7.5f, T = %5.3f (df = %i)", P, T0, df)
    )
  )

}
