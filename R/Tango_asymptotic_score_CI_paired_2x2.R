#' @title The Tango asymptotic score confidence interval for the difference between paired probabilities
#' @description The Tango asymptotic score confidence interval for the difference between paired probabilities
#' @description Described in Chapter 8 "The Paired 2x2 Table"
#' @param n the observed counts (a 2x2 matrix)
#' @param alpha the nominal level, e.g. 0.05 for 95# CIs
#' @param printresults display results (FALSE = no, TRUE = yes)
#' @examples
#' # Airway hyper-responsiveness before and after stem cell transplantation
#' # (Bentur et al., 2009)
#' n <- rbind(c(1, 1), c(7, 12))
#' Tango_asymptotic_score_CI_paired_2x2(n)
#'
#' # Complete response before and after consolidation therapy
#' # (Cavo et al., 2012)
#' n <- rbind(c(59, 6), c(16, 80))
#' Tango_asymptotic_score_CI_paired_2x2(n)
#'
#' @export
#' @return A list containing lower, upper and point estimates of the statistic
Tango_asymptotic_score_CI_paired_2x2 <- function(n, alpha = 0.05, printresults = TRUE) {
  n12 <- n[1, 2]
  n21 <- n[2, 1]
  N <- sum(n)
  z <- qnorm(1 - alpha / 2, 0, 1)

  # Estimate of the difference between probabilities (deltahat)
  estimate <- (n[1, 2] - n[2, 1]) / N

  tol <- 1e-7
  param <- list(n12 = n12, n21 = n21, N = N, z = z)

  # * * ** Lower CI limit * * **
  if (estimate == -1) {
    L <- -1
  } else {
    L <- uniroot(calculate_lower_limit.4, c(-1 + tol, 1 - tol), .param = param, tol = tol)$root
  }

  # * * ** Upper CI limit * * **
  if (estimate == 1) {
    U <- 1
  } else {
    U <- uniroot(calculate_upper_limit.4, c(-1 + tol, 1 - tol), .param = param, tol = tol)$root
  }

  if (printresults) {
    .print("Tango asymptotic score CI: estimate = %6.4f (%g%% CI %6.4f to %6.4f)\n", estimate, 100 * (1 - alpha), L, U)
  }

  invisible(list(L = L, U = U, estimate = estimate))
}


# ========================================
calculate_lower_limit.4 <- function(delta0, .param) {
  z <- .param$z
  T0 <- test_statistic.4(delta0, .param)
  f <- T0 - z
  return(f)
}


# ========================================
calculate_upper_limit.4 <- function(delta0, .param) {
  z <- .param$z
  T0 <- test_statistic.4(delta0, .param)
  f <- T0 + z
  return(f)
}


# =================================
test_statistic.4 <- function(delta0, .param) {
  n12 <- .param$n12
  n21 <- .param$n21
  N <- .param$N
  p21tilde <- ML_estimate.4(delta0, .param)
  T0 <- (n12 - n21 - N * delta0) / sqrt(N * (2 * p21tilde + delta0 * (1 - delta0)))
  return(T0)
}


# =====================================================
ML_estimate.4 <- function(delta0, .param) {
  n12 <- .param$n12
  n21 <- .param$n21
  N <- .param$N
  A <- 2 * N
  B <- -n12 - n21 + (2 * N - n12 + n21) * delta0
  C <- -n21 * delta0 * (1 - delta0)
  p21tilde <- (sqrt(B^2 - 4 * A * C) - B) / (2 * A)
  return(p21tilde)
}

.print <- function(s, ...) {
  print(sprintf(gsub("\n", "", s), ...), quote = FALSE)
}
