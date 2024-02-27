#' @title The Z-unpooled test for association in 2x2 tables
#' @description The Z-unpooled test for association in 2x2 tables
#' @description Described in Chapter 4 "The 2x2 Table"
#' @param n the observed counts (a 2x2 matrix)
#' @examples
#' # Example: A lady tasting a cup of tea
#' Z_unpooled_test_2x2(tea)
#'
#' # Example: Perondi et al. (2004)
#' Z_unpooled_test_2x2(perondi_2004)
#'
#' # Example: Lampasona et al. (2013)
#' Z_unpooled_test_2x2(lampasona_2013)
#'
#' # Example: Ritland et al. (2007)
#' Z_unpooled_test_2x2(ritland_2007)
#'
#' @export
#' @return An object of the [contingencytables_result] class,
#' basically a subclass of [base::list()]. Use the [utils::str()] function
#' to see the specific elements returned.
Z_unpooled_test_2x2 <- function(n) {
  validateArguments(mget(ls()))

  n1p <- n[1, 1] + n[1, 2]
  n2p <- n[2, 1] + n[2, 2]

  # The unpooled Z statistic
  Z <- (n[1, 1] / n1p - n[2, 1] / n2p) /
    sqrt(n[1, 1] * n[1, 2] / n1p^3 + n[2, 1] * n[2, 2] / n2p^3)

  # The two-sided P-value (reference distribution: standard normal)
  P <- 2 * (1 - pnorm(abs(Z), 0, 1))

  # Handle cases where the P-value is not computable
  if (is.na(P)) {
    P <- 1.0
  }

  printresults <- function() {
    sprintf("The Z-unpooled test: P = %7.5f, Z = %6.3f", P, Z)
  }

  res <- list("Pvalue" = P, "Z" = Z)
  return(contingencytables_result(res, printresults))
}
