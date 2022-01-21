#' @title The Cochran-Mantel-Haenszel test of a common odds ratio
#' @description The Cochran-Mantel-Haenszel test of a common odds ratio
#' @description Described in Chapter 10 "Stratified 2x2 Tables and Meta-Analysis"
#' @param n the observed table (a 2x2xk matrix, where k is the number of strata)
#' @param printresults display results (FALSE = no, TRUE = yes)
#' @examples
#' # Smoking and lung cancer (Doll and Hill, 1950)
#' n <- array(dim = c(2, 2, 2))
#' n[, , 1] <- matrix(c(647, 622, 2, 27), 2, byrow = TRUE)
#' n[, , 2] <- matrix(c(41, 28, 19, 32), 2, byrow = TRUE)
#' CochranMantelHaenszel_test_stratified_2x2(n)
#'
#' # Prophylactice use of Lidocaine in myocardial infarction (Hine et al., 1989)
#' n <- array(0, dim = c(2, 2, 6))
#' n[, , 1] <- rbind(c(2, 37), c(1, 42))
#' n[, , 2] <- rbind(c(4, 40), c(4, 40))
#' n[, , 3] <- rbind(c(6, 101), c(4, 106))
#' n[, , 4] <- rbind(c(7, 96), c(5, 95))
#' n[, , 5] <- rbind(c(7, 103), c(3, 103))
#' n[, , 6] <- rbind(c(11, 143), c(4, 142))
#' CochranMantelHaenszel_test_stratified_2x2(n)
#'
#' @export
#' @return A list containing the two-sided p-value, the statistic and the degrees of freedom
CochranMantelHaenszel_test_stratified_2x2 <- function(n, printresults = TRUE) {
  n1pk <- apply(n[1, , ], 2, sum)
  np1k <- apply(n[, 1, ], 2, sum)
  n2pk <- apply(n[2, , ], 2, sum)
  np2k <- apply(n[, 2, ], 2, sum)
  nppk <- apply(n, 3, sum)

  # The Cochran-Mantel-Haenszel test statistic
  numerator <- (sum((n[1, 1, ] * n[2, 2, ] - n[1, 2, ] * n[2, 1, ]) / nppk))^2
  denominator <- sum((n1pk * n2pk * np1k * np2k) / ((nppk^2) * (nppk - 1)))
  T0 <- numerator / denominator

  # The two-sided P-value (reference distribution: chi-squared with one
  # degree of freedom)
  df <- 1
  P <- 1 - pchisq(T0, df)

  if (printresults) {
    .print("The Cochran-Mantel-Haenszel test: P = %7.5f, T0 = %5.3f (df = %i)\n", P, T0, df)
  }

  invisible(list(P = P, T = T0, df = df))
}


.print <- function(s, ...) {
  print(sprintf(gsub("\n", "", s), ...), quote = FALSE)
}
