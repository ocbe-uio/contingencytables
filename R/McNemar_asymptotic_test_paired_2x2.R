#' @title The McNemar asymptotic test
#' @description The McNemar asymptotic test
#' @description Described in Chapter 8 "The Paired 2x2 Table"
#' @param n the observed table (a 2x2 matrix)
#' @examples
#' McNemar_asymptotic_test_paired_2x2(bentur_2009)
#' McNemar_asymptotic_test_paired_2x2(cavo_2012)
#' McNemar_asymptotic_test_paired_2x2(ezra_2010)
#' @export
#' @return An object of the [contingencytables_result] class,
#' basically a subclass of [base::list()]. Use the [utils::str()] function
#' to see the specific elements returned.
McNemar_asymptotic_test_paired_2x2 <- function(n) {
  validateArguments(mget(ls()))

  # The number of discordant pairs
  nd <- n[1, 2] + n[2, 1]

  if (nd == 0) {
    P <- 1
    res <- c("Pvalue" = P)
    printresults <- function() {
      cat("No discordant pairs\nP = 1.0")
    }
  } else {
    # The McNemar test statistic
    Z <- (n[1, 2] - n[2, 1]) / sqrt(nd)

    # Reference distribution: standard normal
    P <- 2 * (1 - pnorm(abs(Z), 0, 1))

    res <- list("Pvalue" = P, "Z" = Z)
    printresults <- function() {
      sprintf("The McNemar asymptotic test: P = %8.6f, Z = %6.3f", P, Z)
    }
  }

  return(contingencytables_result(res, printresults))
}
