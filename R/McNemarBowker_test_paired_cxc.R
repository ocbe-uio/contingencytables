#' @title The McNemar-Bowker test for marginal symmetry
#' @description The McNemar-Bowker test for marginal symmetry
#' @description Described in Chapter 9 "The Paired cxc Table"
#' @param n the observed table (a cxc matrix)
#' @param printresults display results (FALSE = no, TRUE = yes)
#' @examples
#' # Pretherapy susceptability of pathogens (Peterson et al., 2007)
#' n <- rbind(c(596, 18, 6, 5), c(0, 2, 0, 0), c(0, 0, 42, 0), c(11, 0, 0, 0))
#' McNemarBowker_test_paired_cxc(n)
#' @export
#' @return A list containing the probability, the statistic and the degrees of freedom
McNemarBowker_test_paired_cxc <- function(n, printresults = TRUE) {
  c <- nrow(n)

  # The McNemar-Bowker test statistic
  T0 <- 0
  for (i in 2:c) {
    for (j in 1:(i - 1)) {
      if (n[i, j] + n[j, i] > 0) {
        T0 <- T0 + ((n[i, j] - n[j, i])^2) / (n[i, j] + n[j, i])
      }
    }
  }

  # Reference distribution: chi-squared with c * (c-1) / 2 degrees of freedom
  df <- c * (c - 1) / 2
  P <- 1 - pchisq(T0, df)

  if (printresults) {
    .print("The McNemar-Bowker test for symmetry: P = %8.6f, T0 = %6.3f (df=%g)\n", P, T0, df)
  }

  invisible(list(P = P, T = T0, df = df))
}


.print <- function(s, ...) {
  print(sprintf(gsub("\n", "", s), ...), quote = FALSE)
}
