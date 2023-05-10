#' @title The McNemar exact conditional test
#' @description The McNemar exact conditional test
#' @description Described in Chapter 8 "The Paired 2x2 Table"
#' @param n the observed table (a 2x2 matrix)
#' @importFrom stats pbinom
#' @examples
#' McNemar_exact_cond_test_paired_2x2(bentur_2009)
#' McNemar_exact_cond_test_paired_2x2(cavo_2012)
#' McNemar_exact_cond_test_paired_2x2(ezra_2010)
#' @export
#' @return An object of the [contingencytables_result] class,
#' basically a subclass of [base::list()]. Use the [utils::str()] function
#' to see the specific elements returned.
McNemar_exact_cond_test_paired_2x2 <- function(n) {
  validateArguments(mget(ls()))

  # Exact p-value based on the binomial distribution
  P <- 2 * pbinom(min(n[1, 2], n[2, 1]), n[1, 2] + n[2, 1], 0.5)
  P <- min(P, 1)
  return(contingencytables_result(list("P" = P), sprintf("The McNemar exact conditional test: P = %8.6f", P)))
}
