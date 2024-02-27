#' @title The Chacko test for order-restriction
#' @description Described in Chapter 3, "The 1xc Table and the Multinomial
#' Distribution", Chacko (1966) derived a test based on the Pearson chi-square
#' statistic to test the hypothesis that the categories of a multinomial
#' variable with `c` possible outcomes have a natural ordering. The test
#' statistic is asymptotically chi-squared distributed.
#' @param n the observed counts (a 1xc vector, where c is the number of categories)
#' @return An object of the [contingencytables_result] class,
#' basically a subclass of [base::list()]. Use the [utils::str()] function
#' to see the specific elements returned.
#' @references
#' Chacko, V. J. (1966). Modified chi-square test for ordered alternatives.
#' Sankhyā: The Indian Journal of Statistics, Series B, 185-190.
#'
#' Fagerland MW, Lydersen S, Laake P (2017) Statistical Analysis of Contingency
#' Tables. Chapman & Hall/CRC, Boca Raton, FL.
#' @examples
#' Chacko_test_1xc(hypothetical)
#' @export
#' @importFrom stats weighted.mean
Chacko_test_1xc <- function(n) {
  validateArguments(mget(ls()))
  inclination <- sum(diff(n))
  # The ordering process (Chacko, 1966)
  c <- length(n)
  N <- sum(n)
  is_ordered <- all(n == sort(n))
  t <- rep.int(1L, c)

  while (!is_ordered) {
    for (i in seq(to = length(n) - 1L)) {
      if (n[i] > n[i + 1L]) {
        n[i] <- weighted.mean(c(n[i], n[i + 1L]), c(t[i], t[i + 1L]))
        t[i] <- 2L
        t[i + 1L] <- 0L
        break
      }
    }
    to_keep <- t > 0L
    t <- t[to_keep]
    n <- n[to_keep]
    is_ordered <- all(n == sort(n))
  }
  m <- length(n)

  # The Chacko test statistic
  T0 <- sum(t * ((n - N / c) ^ 2L)) * c / N

  # The two-sided P-value (reference distribution: chi-squared with m-1
  # degrees of freedom)
  df <- m - 1L
  P <- pchisq(T0, df, lower.tail = FALSE)

  if (inclination < 0 || m <= 1) {
    warning(
      "Apparently non-decreasing sample may not fit the alternative hypothesis",
      " (p_1 <= p_2 <= ... <= p_c). Consider reversing the input."
    )
  }

  # Output
  printresults <- function() {
    cat_sprintf("The Chacko test: P = %7.6f, T = %5.3f (df = %g)", P, T0, df)
  }
  return(
    contingencytables_result(
      list("Pvalue" = P, "T" = T0, "df" = df),
      printresults
    )
  )

}
