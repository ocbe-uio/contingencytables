#' @title The McNemar exact conditional test
#' @description The McNemar exact conditional test
#' @description Described in Chapter 8 "The Paired 2x2 Table"
#' @param n the observed table (a 2x2 matrix)
#' @param printresults display results (FALSE = no, TRUE = yes)
#' @importFrom stats pbinom
#' @examples
#' # Airway hyper-responsiveness before and after stem cell transplantation
#' # (Bentur et al., 2009)
#' n <- rbind(c(1, 1), c(7, 12))
#' McNemar_exact_cond_test_paired_2x2(n)
#'
#' # Complete response before and after consolidation therapy
#' # (Cavo et al., 2012)
#' n <- rbind(c(59, 6), c(16, 80))
#' McNemar_exact_cond_test_paired_2x2(n)
#'
#' # Floppy eyelid syndrome vs obstructive sleep apnea (Ezra et al., 2010)
#' n <- rbind(c(7, 25), c(2, 68))
#' McNemar_exact_cond_test_paired_2x2(n)
#'
#' @export
#' @return The exact p-value based on the binomial distribution
McNemar_exact_cond_test_paired_2x2 <- function(n, printresults = TRUE) {
  # Exact p-value based on the binomial distribution
  P <- 2 * pbinom(min(n[1, 2], n[2, 1]), n[1, 2] + n[2, 1], 0.5)
  P <- min(P, 1)
  if (printresults) {
    .print("The McNemar exact conditional test: P = %8.6f\n", P)
  }
  return(P)
}

.print <- function(s, ...) {
  print(sprintf(gsub("\n", "", s), ...), quote = FALSE)
}
