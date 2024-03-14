#' @title The exact conditional and mid-P tests for unspecific ordering
#' @description The exact conditional and mid-P tests for unspecific ordering.
#' May also be used for 2xc tables, after flipping rows and columns (i.e. if
#' n is a 2xc table, call this function with n' (the transpose of n) as
#' the first argument).
#' @description Described in Chapter 5 "The Ordered rx2 Table"
#' @param n the observed counts (an rx2 matrix)
#' @param direction the direction of the success probabilities ("increasing"
#' or "decreasing")
#' @param statistic the Pearson test statistic ("Pearson") or the likelihood
#' ratio test statistic ("LR"). Can also be used for cumulative ORs in
#' 2xc tables with "PearsonCumOR" or "LRCumOR".
#' @examples
#' # Chapter 6: Postoperative nausea (Lydersen et al., 2012a)
#' n <- t(lydersen_2012a)
#' Exact_cond_midP_unspecific_ordering_rx2(n, "decreasing")
#' Exact_cond_midP_unspecific_ordering_rx2(n, "decreasing", "PearsonCumOR")
#' @export
#' @return An object of the [contingencytables_result] class,
#' basically a subclass of [base::list()]. Use the [utils::str()] function
#' to see the specific elements returned.
Exact_cond_midP_unspecific_ordering_rx2 <- function(n, direction, statistic = "Pearson") {
  validateArguments(mget(ls()))
  r <- nrow(n)
  nip <- apply(n, 1, sum)
  npj <- apply(n, 2, sum)
  N <- sum(n)
  np1 <- sum(n[, 1])

  # Calculate all nchoosek beforehand
  nip_choose_xi1 <- matrix(0, r, max(nip) + 1)
  for (i in 1:r) {
    for (xi1 in 0:nip[i]) {
      nip_choose_xi1[i, xi1 + 1] <- choose(nip[i], xi1)
    }
  }
  N_choose_np1 <- choose(N, np1)

  # The observed value of the test statistic
  Tobs <- test_statistic(n, r, nip, npj, N, direction, statistic)

  # Calculate the two-sided exact P-value and the mid-P value
  # Need separate functions for different values of r (the number of rows)
  if (r == 4) {
    tmp <- calc_Pvalue_4x2(
      Tobs, nip, np1, npj, N, N_choose_np1, nip_choose_xi1, direction, statistic
    )
  } else if (r == 5) {
    tmp <- calc_Pvalue_5x2(
      Tobs, nip, np1, npj, N, N_choose_np1, nip_choose_xi1, direction, statistic
    )
  } else {
    stop("n must have either 4 or 5 rows. Consider transposing it.")
  }
  P <- tmp$P
  midP <- tmp$midP

  # Output
  printresults <- function() {
    cat_sprintf("Exact conditional test:    P = %8.5f\n", P)
    cat_sprintf("Mid-P test:             midP = %8.5f\n", midP)
  }

  return(contingencytables_result(list("Pvalue" = P, "midP" = midP), printresults))
}

# Slightly different calculations are needed for cumulative odds ratios in 2xc tables

test_statistic_cum_OR <- function(n, c, npj, statistic) {
  r <- rep(0, c)
  for (j in 1:c) {
    r[j] <- sum(n[1, 1:j]) / sum(n[2, 1:j])
  }
  J <- list()
  index1 <- 1
  while (index1 < c + 1) {
    v <- which(r[index1:length(r)] == min(r[index1:length(r)]))[1]
    J[[length(J) + 1]] <- n[, index1:(index1 + v - 1), drop = FALSE]
    index1 <- index1 + v
  }

  m <- array(0, dim = c(length(J), 2, c))
  T0 <- 0
  if (statistic == "PearsonCumOR") {
    for (h in seq_along(J)) {
      nJ <- J[[h]]
      cols <- ncol(nJ)
      for (i in 1:2) {
        for (j in 1:cols) {
          m[h, i, j] <- npj[j] * sum(nJ[i, ]) / sum(nJ)
          if (m[h, i, j] > 0) {
            T0 <- T0 + ((n[i, j] - m[h, i, j])^2) / m[h, i, j]
          }
        }
      }
    }
  } else if (statistic == "LRCumOR") {
    for (h in seq_along(J)) {
      nJ <- J[[h]]
      cols <- ncol(nJ)
      for (i in 1:2) {
        for (j in 1:cols) {
          m[h, i, j] <- npj[j] * sum(nJ[i, ]) / sum(nJ)
          if (m[h, i, j] > 0 && n[i, j] > 0) {
            T0 <- T0 + n[i, j] * log(n[i, j] / m[h, i, j])
          }
        }
      }
    }
    T0 <- 2 * T0
  }

  return(T0)
}

# Calculate the test statistics
test_statistic <- function(n, r, nip, npj, N, direction, statistic) {
  # These are used for cumulative odds ratios in 2xc tables
  if (statistic %in% c("PearsonCumOR", "LRCumOR")) {
    n <- t(n)
    n[c(1, 2), ] <- n[c(2, 1), ]
    T0 <- test_statistic_cum_OR(n, r, nip, statistic)
    return(T0)
  }

  # Common calculations for the Pearson and LR statistics
  rowsums_n <- rowSums(n)
  nhat <- n[, 1] / rowsums_n
  nhatstar <- nhat
  for (i in 1:(r - 1)) {
    if ((direction == "increasing" && nhatstar[i] > nhatstar[i + 1]) ||
      (direction == "decreasing" && nhatstar[i] < nhatstar[i + 1])) {
      pooled_proportion <- (n[i, 1] + n[i + 1, 1]) / (n[i, 1] + n[i, 2] + n[i + 1, 1] + n[i + 1, 2])
      nhatstar[i] <- pooled_proportion
      nhatstar[i + 1] <- pooled_proportion
    }
  }
  nstar <- matrix(0, r, 2)
  nstar[, 1] <- rowsums_n * nhatstar
  nstar[, 2] <- rowsums_n * (1 - nhatstar)

  m <- matrix(0, r, 2)
  T0 <- 0
  if (statistic == "Pearson") {
    for (i in 1:r) {
      for (j in 1:2) {
        m[i, j] <- nip[i] * npj[j] / N
        if (m[i, j] > 0) {
          T0 <- T0 + ((nstar[i, j] - m[i, j])^2) / m[i, j]
        }
      }
    }
  } else if (statistic == "LR") {
    for (i in 1:r) {
      for (j in 1:2) {
        m[i, j] <- nip[i] * npj[j] / N
        if (m[i, j] > 0 && nstar[i, j] != 0) {
          T0 <- T0 + nstar[i, j] * log(nstar[i, j] / m[i, j])
        }
      }
    }
    T0 <- 2 * T0
  }

  return(T0)
}
