#' @title The Chacko test for order-restriction
#' @description The Chacko test for order-restriction
#' @description Described in Chapter 3 "The 1xc Table and the Multinomial Distribution"
#' @param n the observed counts (a 1xc vector, where c is the number of categories)
#' @param printresults display results (F = no, T = yes)
#' @return A data frame containing the two-sided p-value, the statistic and the degrees of freedom
#' @examples
#' # Hypothetical experiment
#' Chacko_test_1xc(n = c(1, 4, 3, 11, 9))
#' @export
Chacko_test_1xc <- function(n, printresults = TRUE) {
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
  T0 <- 0
  for (i in 1:m) {
    T0 <- T0 + t0[i] * ((nt[i] - N / c0)^2)
  }
  T0 <- T0 * c0 / N

  # The two-sided P-value (reference distribution: chi-squared with m-1
  # degrees of freedom)
  df <- m - 1
  P <- 1 - pchisq(T0, df)

  if (printresults) {
    print(
      sprintf(
        "The Chacko test: P = %7.5f, T = %5.3f (df = %i)", P, T0, df
      ),
      quote = FALSE
    )
  }

  res <- data.frame(P = P, T = T0, df = df)
  invisible(res)
}
