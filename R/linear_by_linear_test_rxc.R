#' @title The linear-by-linear test for association
#' @description The linear-by-linear test for association
#' @description Described in Chapter 7 "The rxc Table"
#' @param n the observed table (an rxc matrix)
#' @param a scores assigned to the rows
#' @param b scores assigned to the columns
#' @examples
#' linear_by_linear_test_rxc(table_7.7)
#' linear_by_linear_test_rxc(table_7.8)
#' linear_by_linear_test_rxc(table_7.9)
#' @export
#' @return An object of the [contingencytables_result] class,
#' basically a subclass of [base::list()]. Use the [utils::str()] function
#' to see the specific elements returned.
linear_by_linear_test_rxc <- function(n, a = seq_len(ncol(n)), b = seq_len(nrow(n))) {
  validateArguments(mget(ls()))

  # If no scores are given, use equally spaced scores
  r <- nrow(n)
  c <- ncol(n)

  N <- sum(n)

  # Put the observed data into long format
  Y1 <- rep(0, N)
  Y2 <- rep(0, N)
  id <- 0
  for (i in 1:r) {
    for (j in 1:c) {
      for (k in 1:n[i, j]) {
        id <- id + 1
        Y1[id] <- a[i]
        Y2[id] <- b[j]
      }
    }
  }

  # The Pearson correlation coefficient
  r <- cor(Y1, Y2)

  # The linear-by-linear test statistic
  Z <- sqrt(N - 1) * r
  P <- 2 * (1 - pnorm(abs(Z), 0, 1))

  return(
    contingencytables_result(
      list(P = P, Z = Z),
      sprintf("The linear-by-linear test for association: P = %8.6f, Z = %6.3f", P, Z)
    )
  )
}
