#' @title The Pearson chi-squared test for multinomial probabilities
#' @description The Pearson chi-squared test for multinomial probabilities
#' @description Described in Chapter 3 "The 1xc Table and the Multinomial Distribution"
#' @param n the observed counts (a 1xc vector, where c is the number of categories)
#' @param pi0 given probabilities (a 1xc vector)
#' @examples
#' # Genotype counts for SNP rs 6498169 in RA patients
#' Pearson_chi_squared_test_1xc(n = snp6498169$complete$n, pi0 = snp6498169$complete$pi0)
#' # subset of 10 patients
#' Pearson_chi_squared_test_1xc(n = snp6498169$subset$n, pi0 = snp6498169$subset$pi0)
#' @export
#' @return An object of the [contingencytables_result] class,
#' basically a subclass of [base::list()]. Use the [utils::str()] function
#' to see the specific elements returned.
Pearson_chi_squared_test_1xc <- function(n, pi0) {
  validateArguments(mget(ls()))

  c0 <- length(n)
  N <- sum(n)

  # The Pearson chi-squared test statistic
  T0 <- sum(((n - N * pi0)^2) / (N * pi0))

  # The two-sided P-value
  # (reference distribution: chi-squared with c-1 degrees of freedom)
  df <- c0 - 1
  P <- 1 - pchisq(T0, df)

  return(
    contingencytables_result(
      list("Pvalue" = P, "T" = T0, "df" = df),
      sprintf(
        "The Pearson chi-squared test: P = %7.5f, T = %5.3f (df = %i)",
        P, T0, df
      )
    )
  )
}
