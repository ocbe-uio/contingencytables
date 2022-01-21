#' @title The McNemar asymptotic test with continuity correction
#' @description The McNemar asymptotic test with continuity correction
#' @description Described in Chapter 8 "The Paired 2x2 Table"
#' @param n the observed table (a 2x2 matrix)
#' @param printresults display results (FALSE = no, TRUE = yes)
#' @examples
#' # Airway hyper-responsiveness before and after stem cell transplantation
#' # (Bentur et al., 2009)
#' n <- rbind(c(1, 1), c(7, 12))
#' McNemar_asymptotic_test_CC_paired_2x2(n)
#'
#' # Complete response before and after consolidation therapy
#' # (Cavo et al., 2012)
#' n <- rbind(c(59, 6), c(16, 80))
#' McNemar_asymptotic_test_CC_paired_2x2(n)
#'
#' # Floppy eyelid syndrome vs obstructive sleep apnea (Ezra et al., 2010)
#' n <- rbind(c(7, 25), c(2, 68))
#' McNemar_asymptotic_test_CC_paired_2x2(n)
#'
#' @export
#' @return The McNemar test statistic with continuity correction (\code{Z}) and the reference distribution (standard normal, \code{P})
McNemar_asymptotic_test_CC_paired_2x2 <- function(n, printresults = TRUE) {
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

  # The McNemar test statistic with continuity correction
  Z <- (abs(n[1, 2] - n[2, 1]) - 1) / sqrt(nd)

  # Reference distribution: standard normal
  P <- 2 * (1 - pnorm(abs(Z), 0, 1))

  if (printresults) {
    .print("The asymptotic McNemar test with continuity correction: P = %8.6f, Z = %6.3f\n", P, Z)
  }

  return(list(P = P, Z = Z))
}


.print <- function(s, ...) {
  print(sprintf(gsub("\n", "", s), ...), quote = FALSE)
}
