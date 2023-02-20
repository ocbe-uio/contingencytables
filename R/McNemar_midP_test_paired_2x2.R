#' @title The McNemar mid-P test
#' @description The McNemar mid-P test
#' @description Described in Chapter 8 "The Paired 2x2 Table"
#' @param n the observed table (a 2x2 matrix)
#' @examples
#' McNemar_midP_test_paired_2x2(bentur_2009)
#' McNemar_midP_test_paired_2x2(cavo_2012)
#' McNemar_midP_test_paired_2x2(ezra_2010)
#' @export
#' @return probability value
McNemar_midP_test_paired_2x2 <- function(n) {
  validateArguments(mget(ls()))

  if (n[1, 2] == n[2, 1]) {
    midP <- 1 - 0.5 * dbinom(n[1, 2], n[1, 2] + n[2, 1], 0.5)
  } else {
    P <- 2 * pbinom(min(n[1, 2], n[2, 1]), n[1, 2] + n[2, 1], 0.5)
    P <- min(P, 1)
    midP <- P - dbinom(n[1, 2], n[1, 2] + n[2, 1], 0.5)
  }

  return(
    contingencytables_result(
      midP,
      sprintf("The McNemar mid-P test: P = %8.6f\n", midP)
    )
  )
}
