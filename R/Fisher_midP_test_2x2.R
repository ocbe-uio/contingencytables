#' @title The Fisher mid-P test for association in 2x2 tables
#' @description The Fisher mid-P test for association in 2x2 tables
#' @description Described in Chapter 4 "The 2x2 Table"
#' @param n the observed counts (a 2x2 matrix)
#' @param statistic 'hypergeometric' (i.e. Fisher-Irwin default), 'Pearson', or 'LR' (likelihood ratio)
#' @examples
#' Fisher_midP_test_2x2(tea)
#' Fisher_midP_test_2x2(perondi_2004)
#' Fisher_midP_test_2x2(lampasona_2013)
#' Fisher_midP_test_2x2(ritland_2007)
#' @export
#' @inheritSection chap4 Table orientation
#' @return An object of the [contingencytables_result] class,
#' basically a subclass of [base::list()]. Use the [utils::str()] function
#' to see the specific elements returned.
Fisher_midP_test_2x2 <- function(n, statistic = "hypergeometric") {
  validateArguments(
    x = mget(ls()),
    types = list(n = "counts", statistic = c("Pearson", "hypergeometric", "LR"))
  )
  n1p <- n[1, 1] + n[1, 2]
  n2p <- n[2, 1] + n[2, 2]
  np1 <- n[1, 1] + n[2, 1]
  N <- sum(sum(n))

  # The possible tables, expressed by their x_11 counts
  x11values <- max(c(0, np1 - n2p)):min(c(n1p, np1))

  # The probabilities of the possible tables (from the hypergeometric distribution)
  fvalues <- dhyper(x11values, np1, N - np1, n1p)

  # Values of the test statistic for all possible tables
  Tvalues <- test_statistic_fisher_midp_test_2x2(
    x11values, n1p - x11values, np1 - x11values, n2p - (np1 - x11values),
    statistic
  )

  # Observed value of the test statistic
  Tobs <- test_statistic_fisher_midp_test_2x2(
    n[1, 1], n[1, 2], n[2, 1], n[2, 2], statistic
  )

  # Two-sided P-value
  # Statistics that agree to a relative 1e-7 are ties, as in stats::fisher.test()
  tol <- if (is.finite(Tobs)) 1e-7 * abs(Tobs) else 0
  tied <- if (is.finite(Tobs)) {
    abs(Tvalues - Tobs) <= tol
  } else {
    Tvalues == Tobs
  }
  P <- sum(fvalues[Tvalues > Tobs + tol]) + 0.5 * sum(fvalues[tied])

  txt <- switch(
    statistic,
    "hypergeometric" = "The Fisher mid-P test (Fisher-Irwin): P = %7.5f",
    "Pearson" = "The Fisher mid-P test (Pearson): P = %7.5f",
    "LR" = "The Fisher mid-P test (LR): P = %7.5f"
  )

  return(contingencytables_result(list("Pvalue" = P), sprintf(txt, P)))
}

# ========================================================
test_statistic_fisher_midp_test_2x2 <- function(x11, x12, x21, x22, statistic) {
  N <- x11 + x12 + x21 + x22

  if (statistic == "hypergeometric") {
    # The hypergeomtric distribution distribution
    # Small values agree less with H0 than large values => "-" sign
    T0 <- -dhyper(x11, x11 + x21, N - x11 - x21, x11 + x12)
  } else if (statistic == "Pearson") {
    # Pearson's chi-squared statistic
    denominator <- (x11 + x12) * (x21 + x22) * (x11 + x21) * (x12 + x22)
    T0 <- numeric(length(x11))
    valid <- denominator > 0
    T0[valid] <- (N[valid] * (x11[valid] * x22[valid] - x12[valid] * x21[valid])^2) / denominator[valid]
  } else if (statistic == "LR") {
    # The likelihood ratio statistic
    n1p <- x11 + x12
    n2p <- x21 + x22
    np1 <- x11 + x21
    np2 <- x12 + x22
    expected11 <- n1p * np1 / N
    expected12 <- n1p * np2 / N
    expected21 <- n2p * np1 / N
    expected22 <- n2p * np2 / N
    T0 <- numeric(length(x11))
    positive <- x11 > 0
    T0[positive] <- T0[positive] + x11[positive] * log(x11[positive] / expected11[positive])
    positive <- x12 > 0
    T0[positive] <- T0[positive] + x12[positive] * log(x12[positive] / expected12[positive])
    positive <- x21 > 0
    T0[positive] <- T0[positive] + x21[positive] * log(x21[positive] / expected21[positive])
    positive <- x22 > 0
    T0[positive] <- T0[positive] + x22[positive] * log(x22[positive] / expected22[positive])
    T0 <- 2 * T0
  }

  return(T0)
}
