#' @title The Fleiss-Levin-Paik test for three-level ordinal outcomes
#' @description The Fleiss-Levin-Paik test for three-level ordinal outcomes
#' @description Described in Chapter 9 "The Paired cxc Table"
#' @param n the observed table (a cxc matrix)
#' @param printresults display results (FALSE = no, TRUE = yes)
#' @examples
#' # Pretherapy susceptability of pathogens *without the N / A category*
#' # (Peterson et al., 2007)
#' n <- rbind(c(596, 18, 6), c(0, 2, 0), c(0, 0, 42))
#' FleissLevinPaik_test_paired_cxc(n)
#' @export
#' @return A list containing the probability, the statistic and the degrees of freedom
FleissLevinPaik_test_paired_cxc <- function(n, printresults = TRUE) {
  c <- nrow(n)
  nip <- apply(n, 1, sum)
  npi <- apply(n, 2, sum)

  if (c != 3) {
    P <- NA
    T0 <- NA
    df <- NA
    if (printresults) {
      .print("This method can only be used for c=3 categories\n")
    }
    return()
  }

  # Compute the differences between the marginal sums
  d <- nip - npi
  if (sum(d^2) == 0) {
    P <- 1
    T0 <- 0
    df <- 1
    if (printresults) {
      .print("No differences between the marginal sums\n")
      .print("P = 1.0\n")
    }
    return()
  }

  n12 <- (n[1, 2] + n[2, 1]) / 2
  n13 <- (n[1, 3] + n[3, 1]) / 2
  n23 <- (n[2, 3] + n[3, 2]) / 2

  # The Fleiss-Levin-Paik test statistic
  T0 <- ((d[1] - d[3])^2) / (2 * (n12 + 4 * n13 + n23))

  # Reference distribution: chi-squared with 1 degree of freedom
  df <- 1
  P <- 1 - pchisq(T0, df)

  if (printresults) {
    .print("The Fleiss-Levin-Paik test: P = %8.6f, T = %6.3f (df=%g)\n", P, T0, df)
  }

  invisible(list(P = P, T = T0, df = df))
}


.print <- function(s, ...) {
  print(sprintf(gsub("\n", "", s), ...), quote = FALSE)
}
