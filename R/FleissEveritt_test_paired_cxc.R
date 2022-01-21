#' @title The Fleiss-Everitt version of the Stuart test for marginal homogeneity
#' @description The Fleiss-Everitt version of the Stuart test for marginal homogeneity
#' @description Described in Chapter 9 "The Paired cxc Table"
#' @param n the observed table (a cxc matrix)
#' @param printresults display results (FALSE = no, TRUE = yes)
#' @examples
#' # From Table 13.6, page 382, of Fleiss et al. (2003)
#' n <- rbind(c(35, 5, 0), c(15, 20, 5), c(10, 5, 5))
#' FleissEveritt_test_paired_cxc(n)
#' @export
#' @return A list containing the probability, the statistic and the degrees of freedom
FleissEveritt_test_paired_cxc <- function(n, printresults = TRUE) {
  # This is the version with c=3 categories
  c <- nrow(n)
  nip <- apply(n, 1, sum)
  npi <- apply(n, 2, sum)

  if (c != 3) {
    P <- NA
    T <- NA
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
    df <- c - 1
    if (printresults) {
      .print("No differences between the marginal sums\n")
      .print("P = 1.0\n")
    }
    return()
  }

  n23 <- (n[2, 3] + n[3, 2]) / 2
  n13 <- (n[1, 3] + n[3, 1]) / 2
  n12 <- (n[1, 2] + n[2, 1]) / 2

  # The Fleiss-Everitt test statistic
  T0 <- (n23 * d[1]^2 + n13 * d[2]^2 + n12 * d[3]^2) / (2 * (n12 * n23 + n12 * n13 + n13 * n23))

  # Reference distribution: chi-squared with 2 degrees of freedom
  df <- 2
  P <- 1 - pchisq(T0, df)

  if (printresults) {
    .print("The Fleiss-Everitt version of the Stuart test: P = %8.6f, T = %6.3f (df=%g)\n", P, T0, df)
  }

  invisible(list(P = P, T = T0, df = df))
}

.print <- function(s, ...) {
  print(sprintf(gsub("\n", "", s), ...), quote = FALSE)
}
