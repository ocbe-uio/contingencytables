#' @title The Fisher exact test for association in 2x2 tables
#' @description The Fisher exact test for association in 2x2 tables
#' @description Described in Chapter 4 "The 2x2 Table"
#' @param n the observed counts (a 2x2 matrix)
#' @param statistic 'hypergeometric' (i.e. Fisher-Irwin; default), 'Pearson', or 'LR' (likelihood ratio)
#' @examples
#' Fisher_exact_test_2x2(tea)
#' Fisher_exact_test_2x2(perondi_2004)
#' Fisher_exact_test_2x2(lampasona_2013)
#' Fisher_exact_test_2x2(ritland_2007)
#' @export
#' @return An object of the [contingencytables_result] class,
#' basically a subclass of [base::list()]. Use the [utils::str()] function
#' to see the specific elements returned.
Fisher_exact_test_2x2 <- function(n, statistic = "Pearson") {
validateArguments(
    x = mget(ls()),
    types = list(
      n = "counts", statistic = c("Pearson", "hypergeometric", "LR")
    )
  )
  n1p <- n[1, 1] + n[1, 2]
  n2p <- n[2, 1] + n[2, 2]
  np1 <- n[1, 1] + n[2, 1]
  N <- sum(n)

  # The possible tables, expressed by their x_11 counts
  x11values <- 0:min(c(n1p, np1))

  # The probabilities of the possible tables (from the hypergeomtric distribution)
  fvalues <- dhyper(x11values, np1, N - np1, n1p)

  # Values of the test statistic for all possible tables
  Tvalues <- test_statistic_fisher_exact_test_2x2(
    x11values, n1p - x11values, np1 - x11values, n2p - (np1 - x11values),
    statistic
  )

  # Observed value of the test statistic
  Tobs <- Tvalues[n[1, 1] + 1]

  # Two-sided P-value
  P <- sum(fvalues[Tvalues >= Tobs])

  # Handle cases where the P-value is not computable
  if (sum(is.na(Tvalues)) || is.na(Tobs)) {
    P <- 1.0
  }

  txt <- switch(
    statistic,
    "hypergeometric" = "The Fisher exact test (Fisher-Irwin): P = %7.5f",
    "Pearson" = "The Fisher exact test (Pearson): P = %7.5f",
    "LR" = "The Fisher exact test (LR): P = %7.5f"
  )

  return(contingencytables_result(list("Pvalue" = P), sprintf(txt, P)))
}

# ========================================================
test_statistic_fisher_exact_test_2x2 <- function(x11, x12, x21, x22, statistic) {
  N <- x11 + x12 + x21 + x22

  if (statistic == "hypergeometric") {
    # The hypergeometric distribution
    # Small values agree less with H0 than large values => "-" sign
    T0 <- -dhyper(x11, x11 + x21, N - x11 - x21, x11 + x12)
  } else if (statistic == "Pearson") {
    # Pearson's chi-squared statistic
    T0 <- (N * (x11 * x22 - x12 * x21)^2) / ((x11 + x12) * (x21 + x22) * (x11 + x21) * (x12 + x22))
  } else if (statistic == "LR") {
    # The likelihood ratio statistic
    n1p <- x11 + x12
    n2p <- x21 + x22
    np1 <- x11 + x21
    np2 <- x12 + x22
    # Make sure 0 is added to T for terms where x_ij = 0 (log(1) = 0)
    x11[x11 == 0] <- 1
    x12[x12 == 0] <- 1
    x21[x21 == 0] <- 1
    x22[x22 == 0] <- 1
    T0 <- 0
    T0 <- T0 + x11 * log(x11 / (n1p * np1 / N))
    T0 <- T0 + x12 * log(x12 / (n1p * np2 / N))
    T0 <- T0 + x21 * log(x21 / (n2p * np1 / N))
    T0 <- T0 + x22 * log(x22 / (n2p * np2 / N))
    T0 <- 2 * T0
  }

  return(T0)
}
