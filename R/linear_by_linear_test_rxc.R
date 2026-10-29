#' @title The linear-by-linear test for association
#' @description The linear-by-linear test for association
#' @description Described in Chapter 7 "The rxc Table"
#' @param n the observed table (an rxc matrix)
#' @param a scores assigned to the rows
#' @param b scores assigned to the columns
#' @examples
#' # Colorectal cancer (Jullumstrø et al., 2009), Table 7.20
#' linear_by_linear_test_rxc(table_7.7)
#'
#' # Breast Tumor (Bofin et al., 2004), Table 7.21
#' linear_by_linear_test_rxc(table_7.8)
#' linear_by_linear_test_rxc(table_7.8)$Pvalue
#'
#' # Self-rated health (Breidablik et al., 2008)
#' linear_by_linear_test_rxc(table_7.9)
#'
#' @export
#' @return An object of the [contingencytables_result] class,
#' basically a subclass of [base::list()]. Use the [utils::str()] function
#' to see the specific elements returned.
linear_by_linear_test_rxc <- function(n, a = seq_len(nrow(n)), b = seq_len(ncol(n))) {
  validateArguments(mget(ls()))
  dat <- to_long(n, a, b)
  N <- nrow(dat)

  # The Pearson correlation coefficient
  r <- cor(dat$Y1, dat$Y2)

  # The linear-by-linear test statistic
  Z <- sqrt(N - 1) * r
  P <- 2 * pnorm(abs(Z), 0, 1, lower.tail = FALSE)

  return(
    contingencytables_result(
      list("Pvalue" = P, "Z" = Z),
      sprintf("The linear-by-linear test for association: P = %8.6f, Z = %6.3f", P, Z)
    )
  )
}
