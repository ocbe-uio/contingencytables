#' @title The Pearson chi-squared and likelihood ratio tests for cumulative ORs in 2xc tables
#' @description The Pearson chi-squared and likelihood ratio tests for cumulative ORs in 2xc tables
#' @description Described in Chapter 6 "The Ordered 2xc Table"
#' @param n the observed counts (a 2xc matrix)
#' @param direction the direction of column probabilities ("increasing" or "decreasing")
#' @examples
#' # Postoperative nausea (Lydersen et al., 2012a)
#' Pearson_LR_tests_cum_OR_2xc(lydersen_2012a)
#' @export
#' @return An object of the [contingencytables_result] class,
#' basically a subclass of [base::list()]. Use the [utils::str()] function
#' to see the specific elements returned.
Pearson_LR_tests_cum_OR_2xc <- function(n, direction = "decreasing") {
  validateArguments(mget(ls()))

  # Swap the order of the two rows if probabilities are increasing
  if (identical(direction, "increasing")) {
    n <- n[seq(nrow(n), 1, -1), ]
  }

  # Calculate the probabilities for the chi-bar-squared distribution
  c <- ncol(n)
  rho <- calc_rho(c)

  # Create a subset (J) of columns to obtain an ordering
  r <- rep(0, c)
  for (j in 1:c) {
    r[j] <- sum(n[1, 1:j]) / sum(n[2, 1:j])
  }
  J <- list()
  index1 <- 1
  while (index1 < c + 1) {
    z0 <- r[index1:length(r)]
    v <- which(z0 == min(z0))[1]
    J[[length(J) + 1]] <- n[, index1:(index1 + v - 1)]
    index1 <- index1 + v
  }

  # The Pearson chi-squared and likelihood ratio statistics
  m <- array(0, dim = c(length(J), 2, c))
  T_LR <- 0
  T_Pearson <- 0
  for (h in seq_along(J)) {
    nJ <- J[[h]]
    npj <- apply(nJ, 2, sum)
    cols <- ncol(nJ)
    mij <- matrix(0, 2, cols)
    for (i in 1:2) {
      for (j in 1:cols) {
        m[h, i, j] <- npj[j] * sum(nJ[i, ]) / sum(nJ)
        mij[i, j] <- m[h, i, j]
        if (m[h, i, j] > 0) {
          T_Pearson <- T_Pearson + ((nJ[i, j] - m[h, i, j])^2) / m[h, i, j]
        }
        if (m[h, i, j] > 0 && n[i, j] > 0) {
          T_LR <- T_LR + nJ[i, j] * log(nJ[i, j] / m[h, i, j])
        }
      }
    }
  }
  T_LR <- 2 * T_LR


  # Assume total column sums in the following (Grove, 1980), and use
  # the rho probabilities as weights
  P_Pearson <- 0
  P_LR <- 0
  for (i in 1:c) {
    prob_Pearson <- 1 - pchisq(T_Pearson, i - 1)
    P_Pearson <- P_Pearson + rho[i] * prob_Pearson
    prob_LR <- 1 - pchisq(T_LR, i - 1)
    P_LR <- P_LR + rho[i] * prob_LR
  }

  # Output arguments (observed statistics and P-values)
  results <- list()
  results$T_Pearson <- T_Pearson
  results$P_Pearson <- P_Pearson
  results$T_LR <- T_LR
  results$P_LR <- P_LR

  printresults <- function() {
    my_sprintf_cat("Pearson chi-squared test: T = %6.3f,  P = %7.5f\n", T_Pearson, P_Pearson)
    my_sprintf_cat("Likelihood ratio test:    T = %6.3f,  P = %7.5f", T_LR, P_LR)
  }

  return(contingencytables_result(results, printresults))
}

# ========================
calc_rho <- function(r) {
  # The probabilities for the chi-bar-squared distribution
  # Iterative algorithm by Barlow et al. (1972)
  rho <- rep(0, r)
  rho[1] <- 1 / r
  rho[r] <- 1 / factorial(r)
  if (r > 2) {
    for (i in 2:(r - 1)) {
      rho_minus1 <- calc_rho(r - 1)
      rho[i] <- (1 / r) * rho_minus1[i - 1] + ((r - 1) / r) * rho_minus1[i]
    }
  }
  return(rho)
}
