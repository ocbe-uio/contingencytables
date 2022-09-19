#' @title The Chacko test for order-restriction
#' @description The Chacko test for order-restriction
#' @description Described in Chapter 3 "The 1xc Table and the Multinomial Distribution"
#' @param n the observed counts (a 1xc vector, where c is the number of categories)
#' @return A data frame containing the two-sided p-value, the statistic and the degrees of freedom
#' @examples
#' Chacko_test_1xc(hypothetical)
#' @export
Chacko_test_1xc <- function(n) {
  validateArguments(mget(ls()))
  c0 <- length(n)
  N <- sum(n)

  # The ordering process
  nt <- n
  t0 <- rep(1, c0)
  m <- c0
  notordered <- 1
  while (notordered == 1) {
    for (i in 1:(m - 1)) {
      if (nt[i] > nt[i + 1]) {
        nt[i] <- (nt[i] + nt[i + 1]) / 2
        t0[i] <- t0[i] + 1
        m <- m - 1
        nt[(i + 1):m] <- nt[(i + 2):(m + 1)]
        break
      }
    }
    if (i == m - 1) {
      notordered <- 0
    }
  }

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

  # Output
  res <- list(
    name = "The Chacko test",
    statistics = list(
      "pvalue" = P, "df" = df, "estimate" = T0, statname = "T"
    )
  )
  class(res) <- "contingencytables_output"
  return(res)
}
