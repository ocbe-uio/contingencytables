#' @title The Baptista-Pike mid-P confidence interval for the odds ratio
#' @description The Baptista-Pike mid-P confidence interval for the odds ratio
#' @description Described in Chapter 4 "The 2x2 Table"
#' @param n the observed table (a 2x2 matrix)
#' @param alpha the nominal level, e.g. 0.05 for 95% CIs
#' @param printresults display results (F = no, T = yes)
#' @return A data frame containing lower, upper and point estimates of the statistic
#' @examples
#' n1 <- rbind(c(3, 1), c(1, 3)) # Example: A lady tasting a cup of tea
#' n2 <- rbind(c(7, 27), c(1, 33)) # Example: Perondi et al. (2004)
#' n3 <- rbind(c(9, 4), c(4, 10)) # Example: Lampasona et al. (2013)
#' n4 <- rbind(c(0, 16), c(15, 57)) # Example: Ritland et al. (2007)
#' BaptistaPike_midP_CI_2x2(n1)
#' BaptistaPike_midP_CI_2x2(n2)
#' BaptistaPike_midP_CI_2x2(n3)
#' BaptistaPike_midP_CI_2x2(n4)
#' @export
BaptistaPike_midP_CI_2x2 <- function(n, alpha = 0.05, printresults = TRUE) {
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
    L <- uniroot(
      calculate_limit,
      c(theta0, theta1),
      n11 = n11, np1 = np1, n1p = n1p, n2p = n2p, alpha = alpha, tol = tol
    )$root
  } else if (estimate == 0) {
    L <- 0
  } else {
    L <- uniroot(
      calculate_limit,
      c(theta0, estimate),
      n11 = n11, np1 = np1, n1p = n1p, n2p = n2p, alpha = alpha,
      tol = tol
    )$root
  }

  # Upper CI limit
  if (n[2, 1] == 0 || n[1, 2] == 0) {
    U <- Inf
  } else if (estimate == 0) {
    U <- uniroot(
      calculate_limit,
      c(theta0, theta1),
      n11 = n11, np1 = np1, n1p = n1p, n2p = n2p, alpha = alpha,
      tol = tol
    )$root
  } else {
    U <- uniroot(
      calculate_limit,
      c(estimate, theta1),
      n11 = n11, np1 = np1, n1p = n1p, n2p = n2p, alpha = alpha,
      tol = tol
    )$root
  }

  if (printresults) {
    print(
      sprintf(
        "Baptista-Pike mid-P CI: estimate = %6.4f (%g%% CI %6.4f to %6.4f)",
        estimate, 100 * (1 - alpha), L, U
      ),
      quote = FALSE
    )
  }

  res <- data.frame(lower = L, upper = U, estimate = estimate)
  invisible(res)
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
  f <- f - 0.5 * Pobs - alpha
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
