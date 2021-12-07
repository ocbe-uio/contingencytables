#' @title The Bhapkar test for marginal homogeneity
#' @description The Bhapkar test for marginal homogeneity
#' @description Described in Chapter 9 "The Paired cxc Table"
#' @param n the observed table (a cxc matrix)
#' @param printresults display results (FALSE = no, TRUE = yes)
#' @examples load_chapter(9)
#' # Pretherapy susceptability of pathogens (Peterson et al., 2007)
#' n <- rbind(c(596, 18, 6, 5), c(0, 2, 0, 0), c(0, 0, 42, 0), c(11, 0, 0, 0))
#' Bhapkar_test_paired_cxc(n)
#' unload_chapter(9)
Bhapkar_test_paired_cxc <- function(n, printresults = TRUE) {
  c <- nrow(n)
  nip <- apply(n, 1, sum)
  npi <- apply(n, 2, sum)
  N <- sum(n)

  # Compute the differences between the marginal sums
  d <- nip[1:(c - 1)] - npi[1:(c - 1)]

  if (sum(d) == 0) {
    P <- 1
    T0 <- 0
    df <- c - 1
    if (printresults) {
      .print("No differences between the marginal sums\n")
      .print("P = 1.0\n")
    }
    return()
  }

  # Form the sample covariance matrix
  Sigmahat <- matrix(0, c - 1, c - 1)
  for (i in 1:(c - 1)) {
    Sigmahat[i, i] <- nip[i] + npi[i] - 2 * n[i, i] - ((nip[i] - npi[i])^2) / N
    for (j in 1:(c - 1)) {
      if (i != j) {
        Sigmahat[i, j] <- -(n[i, j] + n[j, i]) - ((npi[i] - nip[i]) * (npi[j] - nip[j])) / N
      }
    }
  }

  # The Bhapkar test statistic
  T0 <- sum(d * solve(Sigmahat, d))
  if (is.na(T0)) {
    P <- 1
    df <- c - 1
    if (printresults) {
      .print("The Bhapkar test statistic is not computable\n")
      .print("P = 1.0\n")
      print(d)
      print(Sigmahat)
    }
    return()
  }

  # Reference distribution: chi-squared with c-1 degrees of freedom
  df <- c - 1
  P <- 1 - pchisq(T0, df)

  if (printresults) {
    .print("The Bhapkar test for marginal homogenity: P = %8.6f, T = %6.3f (df=%g)\n", P, T0, df)
  }

  invisible(list(P = P, T = T0, df = df))
}


.print <- function(s, ...) {
  print(sprintf(gsub("\n", "", s), ...), quote = FALSE)
}