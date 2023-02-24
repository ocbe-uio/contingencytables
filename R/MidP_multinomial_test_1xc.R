#' @title The mid-P multinomial test for multinomial probabilities
#' @description The mid-P multinomial test for multinomial probabilities
#' @description Described in Chapter 3 "The 1xc Table and the Multinomial Distribution"
#' @param n the observed counts (a 1xc vector, where c is the number of categories)
#' @param pi0 given probabilities (a 1xc vector)
#' @examples
#' # Genotype counts for SNP rs 6498169 in RA patients
#' MidP_multinomial_test_1xc(n = snp6498169$complete$n, pi0 = snp6498169$complete$pi0)
#'
#' # subset of 10 patients
#' MidP_multinomial_test_1xc(n = snp6498169$subset$n, pi0 = snp6498169$subset$pi0)
#' @export
#' @return probability value
MidP_multinomial_test_1xc <- function(n, pi0) {
  validateArguments(mget(ls()))

  c0 <- length(n)
  N <- sum(n)

  # Identify all possible tables with N observations (with 3,4,...,7 categories)
  if (c0 == 3) {
    x <- all.tables3(N)
  } else if (c0 == 4) {
    x <- all.tables4(N)
  } else if (c0 == 5) {
    x <- all.tables5(N)
  } else if (c0 == 6) {
    x <- all.tables6(N)
  } else if (c0 == 7) {
    x <- all.tables7(N)
  }

  P <- 0
  Tobs <- sum(((n - N * pi0)^2) / (N * pi0))
  for (i in seq_len(nrow(x))) {
    T0 <- sum(((x[i, ] - N * pi0)^2) / (N * pi0)) # Pearson chi-squared
    if (T0 > Tobs) {
      P <- P + dmultinom(x[i, ], prob = pi0)
    } else if (T0 == Tobs) {
      P <- P + 0.5 * dmultinom(x[i, ], prob = pi0)
    }
  }

  return(
    contingencytables_result(
      c("P" = P), sprintf("The mid-P multinomial test: P = %7.5f", P)
    )
  )
}
