#' @title The Pearson chi-squared test for multinomial probabilities
#' @description The Pearson chi-squared test for multinomial probabilities
#' @description Described in Chapter 3 "The 1xc Table and the Multinomial Distribution"
#' @param n the observed counts (a 1xc vector, where c is the number of categories)
#' @param pi0 given probabilities (a 1xc vector)
#' @param printresults display results (F = no, T = yes)
#' @examples
#' # Genotype counts for SNP rs 6498169 in RA patients
#' Pearson_chi_squared_test_1xc(n = c(276, 380, 118), pi0 = c(0.402, 0.479, 0.119))
#' # subset of 10 patients
#' Pearson_chi_squared_test_1xc(n = c(6, 1, 3), pi0 = c(0.402, 0.479, 0.119))
#' @export
#' @return A data frame containing the two-sided p-value, the statistic and the degrees of freedom
Pearson_chi_squared_test_1xc <- function(n, pi0, printresults = TRUE) {
  c0 <- length(n)
  N <- sum(n)

  # The Pearson chi-squared test statistic
  T0 <- sum(((n - N * pi0)^2) / (N * pi0))

  # The two-sided P-value
  # (reference distribution: chi-squared with c-1 degrees of freedom)
  df <- c0 - 1
  P <- 1 - pchisq(T0, df)

  if (printresults) {
    print(
      sprintf(
        "The Pearson chi-squared test: P = %7.5f, T = %5.3f (df = %i)",
        P, T0, df
      ),
      quote = FALSE
    )
  }

  res <- data.frame(P = P, T = T0, df = df)
  invisible(res)
}
