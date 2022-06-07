#' @title The McNemar asymptotic test
#' @description The McNemar asymptotic test
#' @description Described in Chapter 8 "The Paired 2x2 Table"
#' @param n the observed table (a 2x2 matrix)
#' @param printresults display results (FALSE = no, TRUE = yes)
#' @examples
#' McNemar_asymptotic_test_paired_2x2(bentur_2009)
#' McNemar_asymptotic_test_paired_2x2(cavo_2012)
#' McNemar_asymptotic_test_paired_2x2(ezra_2010)
#' @export
#' @return The McNemar test statistic with continuity correction (\code{Z}) and the reference distribution (standard normal, \code{P})
McNemar_asymptotic_test_paired_2x2 <- function(n, printresults = TRUE) {
  # The number of discordant pairs
  nd <- n[1, 2] + n[2, 1]
  if (nd == 0) {
    if (printresults) {
      .print("No discordant pairs\n")
      .print("P = 1.0\n")
    }
    P <- 1
    return(P)
  }

  # The McNemar test statistic
  Z <- (n[1, 2] - n[2, 1]) / sqrt(nd)

  # Reference distribution: standard normal
  P <- 2 * (1 - pnorm(abs(Z), 0, 1))

  if (printresults) {
    .print("The McNemar asymptotic test: P = %8.6f, Z = %6.3f\n", P, Z)
  }

  return(list(P = P, Z = Z))
}

.print <- function(s, ...) {
  print(sprintf(gsub("\n", "", s), ...), quote = FALSE)
}
