#' @title The exact multinomial test for multinomial probabilities
#' @description The exact multinomial test for multinomial probabilities
#' @description Described in Chapter 3 "The 1xc Table and the Multinomial Distribution"
#' @param n the observed counts (a 1xc vector, where c is the number of categories)
#' @param pi0 given probabilities (a 1xc vector)
#' @importFrom stats dmultinom
#' @examples
#' # Genotype counts for SNP rs 6498169 in RA patients
#' Exact_multinomial_test_1xc(n = snp6498169$complete$n, pi0 = snp6498169$complete$pi0)
#'
#' # subset of 10 patients
#' Exact_multinomial_test_1xc(n = snp6498169$subset$n, pi0 = snp6498169$subset$pi0)
#' @export
#' @return An object of the [contingencytables_result] class,
#' basically a subclass of [base::list()]. Use the [utils::str()] function
#' to see the specific elements returned.
Exact_multinomial_test_1xc <- function(n, pi0) {
  validateArguments(mget(ls()))
  c0 <- length(n)
  N <- sum(n)

  # Identify all possible tables with N observations (with 3,4,...,7 categories)
  x <- all_tables_X(N, c0)

  P <- 0
  Tobs <- sum(((n - N * pi0)^2) / (N * pi0))
  for (i in seq_len(nrow(x))) {
    T0 <- sum(((x[i, ] - N * pi0)^2) / (N * pi0)) # Pearson chi-squared
    if (T0 >= Tobs) {
      P <- P + dmultinom(x[i, ], prob = pi0)
    }
  }

  res <- contingencytables_result(
    list("P" = P), sprintf("The exact multinomial test: P = %7.5f", P)
  )
  return(res)
}
