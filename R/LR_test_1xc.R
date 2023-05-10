#' @title The likelihood ratio test for multinomial probabilities
#' @description The likelihood ratio test for multinomial probabilities
#' @description Described in Chapter 3 "The 1xc Table and the Multinomial Distribution"
#' @param n the observed counts (a 1xc vector, where c is the number of categories)
#' @param pi0 given probabilities (a 1xc vector)
#' @examples
#' # Genotype counts for SNP rs 6498169 in RA patients
#' LR_test_1xc(n = snp6498169$complete$n, pi0 = snp6498169$complete$pi0)
#' # subset of 10 patients
#' LR_test_1xc(n = snp6498169$subset$n, pi0 = snp6498169$subset$pi0)
#' @export
#' @return An object of the [contingencytables_result] class,
#' basically a subclass of [base::list()]. Use the [utils::str()] function
#' to see the specific elements returned.
LR_test_1xc <- function(n, pi0) {
  validateArguments(mget(ls()))

  c0 <- length(n)
  N <- sum(n)

  # The likelihood ratio test statistic
  T0 <- 0
  for (i in 1:c0) {
    if (n[i] > 0) {
      T0 <- T0 + n[i] * log(n[i] / (N * pi0[i]))
    }
  }
  T0 <- 2 * T0

  # The two-sided P-value (reference distribution: chi-squared with c - 1 degrees of freedom)
  df <- c0 - 1
  P <- 1 - pchisq(T0, df)

  return(
    contingencytables_result(
      list("P" = P, "T" = T0, "df" = df),
      sprintf("The likelihood ratio test: P = %7.5f, T = %5.3f (df = %i)", P, T0, df)
    )
  )
}
