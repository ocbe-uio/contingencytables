#' @title The Baptista-Pike exact conditional confidence interval for the odds
#' ratio
#' @description The Baptista-Pike exact conditional confidence interval for the
#' odds ratio
#' @description Described in Chapter 4 "The 2x2 Table"
#' @param n the observed table (a 2x2 matrix)
#' @param alpha the nominal level, e.g. 0.05 for 95# CIs
#' @return An object of the [contingencytables_result] class,
#' basically a subclass of [base::list()]. Use the [utils::str()] function
#' to see the specific elements returned.
#' @examples
#' BaptistaPike_exact_conditional_CI_2x2(tea)
#' BaptistaPike_exact_conditional_CI_2x2(perondi_2004)
#' BaptistaPike_exact_conditional_CI_2x2(lampasona_2013)
#' BaptistaPike_exact_conditional_CI_2x2(ritland_2007)
#' @export
BaptistaPike_exact_conditional_CI_2x2 <- function(n, alpha = 0.05) {
  validateArguments(mget(ls()))
  # global n11 n1p n2p np1 alphaglobal
  n11 <- n[1, 1]
  n1p <- n[1, 1] + n[1, 2]
  n2p <- n[2, 1] + n[2, 2]
  np1 <- n[1, 1] + n[2, 1]

  # Estimate of the odds ratio (thetahat)
  estimate <- n[1, 1] * n[2, 2] / (n[1, 2] * n[2, 1])

  # Options for Matlab's fzero command
  tol <- 0.0000001
  theta0 <- 0.00001
  theta1 <- 100000

  # Lower CI limit
  if (is.na(estimate) || estimate == Inf) {
    L <- uniroot(calculate_limit, c(theta0, theta1), n11 = n11, np1 = np1, n1p = n1p, n2p = n2p, alpha = alpha, tol = tol)$root
  } else if (estimate == 0) {
    L <- 0
  } else {
    L <- uniroot(calculate_limit, c(theta0, estimate), n11 = n11, np1 = np1, n1p = n1p, n2p = n2p, alpha = alpha, tol = tol)$root
  }

  # Upper CI limit
  if (n[2, 1] == 0 || n[1, 2] == 0) {
    U <- Inf
  } else if (estimate == 0) {
    U <- uniroot(calculate_limit, c(theta0, theta1), n11 = n11, np1 = np1, n1p = n1p, n2p = n2p, alpha = alpha, tol = tol)$root
  } else {
    U <- uniroot(calculate_limit, c(estimate, theta1), n11 = n11, np1 = np1, n1p = n1p, n2p = n2p, alpha = alpha, tol = tol)$root
  }

  # Output
  printresults <- function() {
    cat_sprintf(
      "Baptista-Pike exact conditional CI: estimate = %6.4f (%g%% CI %6.4f to %6.4f)",
      estimate, 100 * (1 - alpha), L, U
    )
  }

  return(
    contingencytables_result(
      list("lower" = L, "upper" = U, "estimate" = estimate),
      printresults
    )
  )
}

# ==================================
calculate_limit <- function(theta0, n11, np1, n1p, n2p, alpha) {
  f <- 0
  Pobs <- noncentralhyge(n11, theta0, n1p, n2p, np1)
  for (x11 in max(c(0, np1 - n2p)):min(c(np1, n1p))) {
    P <- noncentralhyge(x11, theta0, n1p, n2p, np1)
    if (P <= Pobs) {
      f <- f + P
    }
  }
  f <- f - alpha
  return(f)
}

# ======================================
noncentralhyge <- function(x11, theta0, n1p, n2p, np1) {
  numerator <- choose(n1p, x11) * choose(n2p, np1 - x11) * (theta0^x11)
  denominator <- 0
  for (i in max(c(0, np1 - n2p)):min(c(n1p, np1))) {
    denominator <- denominator + choose(n1p, i) * choose(n2p, np1 - i) * (theta0^i)
  }
  f <- numerator / denominator
  return(f)
}
