#' @title The McNemar mid-P test
#' @description The McNemar mid-P test
#' @description Described in Chapter 8 "The Paired 2x2 Table"
#' @param n the observed table (a 2x2 matrix)
#' @param printresults display results (FALSE = no, TRUE = yes)
#' @examples
#' # Airway hyper-responsiveness before and after stem cell transplantation
#' # (Bentur et al., 2009)
#' n <- rbind(c(1, 1), c(7, 12))
#' McNemar_midP_test_paired_2x2(n)
#'
#' # Complete response before and after consolidation therapy
#' # (Cavo et al., 2012)
#' n <- rbind(c(59, 6), c(16, 80))
#' McNemar_midP_test_paired_2x2(n)
#'
#' # Floppy eyelid syndrome vs obstructive sleep apnea (Ezra et al., 2010)
#' n <- rbind(c(7, 25), c(2, 68))
#' McNemar_midP_test_paired_2x2(n)
#' @export
#' @return probability value
McNemar_midP_test_paired_2x2 <- function(n, printresults = TRUE) {
  if (n[1, 2] == n[2, 1]) {
    midP <- 1 - 0.5 * dbinom(n[1, 2], n[1, 2] + n[2, 1], 0.5)
  } else {
    P <- 2 * pbinom(min(n[1, 2], n[2, 1]), n[1, 2] + n[2, 1], 0.5)
    P <- min(P, 1)
    midP <- P - dbinom(n[1, 2], n[1, 2] + n[2, 1], 0.5)
  }

  if (printresults) {
    .print("The McNemar mid-P test: P = %8.6f\n", midP)
  }

  invisible(midP)
}

.print <- function(s, ...) {
  print(sprintf(gsub("\n", "", s), ...), quote = FALSE)
}
