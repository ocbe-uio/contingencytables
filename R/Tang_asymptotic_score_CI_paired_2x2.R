#' @title The Tang asymptotic score confidence interval for the ratio of paired probabilities
#' @description The Tang asymptotic score confidence interval for the ratio of paired probabilities
#' @description Described in Chapter 8 "The Paired 2x2 Table"
#' @param n the observed table (a 2x2 matrix)
#' @param alpha the nominal level, e.g. 0.05 for 95% CIs
#' @examples
#' # Airway hyper-responsiveness before and after stem cell transplantation
#' # (Bentur et al., 2009)
#' Tang_asymptotic_score_CI_paired_2x2(bentur_2009)
#'
#' # Complete response before and after consolidation therapy
#' # (Cavo et al., 2012)
#' Tang_asymptotic_score_CI_paired_2x2(cavo_2012)
#'
#' @export
#' @return An object of the [contingencytables_result] class,
#' basically a subclass of [base::list()]. Use the [utils::str()] function
#' to see the specific elements returned.
Tang_asymptotic_score_CI_paired_2x2 <- function(n, alpha = 0.05) {
  validateArguments(mget(ls()))

  # Define global variables that are needed in the functions below

  n11 <- n[1, 1]
  n12 <- n[1, 2]
  n21 <- n[2, 1]
  N <- sum(n)
  z <- qnorm(1 - alpha / 2, 0, 1)

  # Estimate of the ratio of probabilities (phihat)
  estimate <- (n[1, 1] + n[1, 2]) / (n[1, 1] + n[2, 1])

  # Use Matlab's fzero function to solve the equations: T - z = 0 and T + z = 0,
  # where T is the score test statistic
  tol <- 0.00000001
  param <- list(n11 = n11, n12 = n12, n21 = n21, N = N, z = z)
  phimin <- tol
  phimax <- as.double(.Machine$integer.max)

  # * * ** Lower CI limit * * **
  if (estimate == 0 || is.na(estimate)) {
    L <- 0
  } else {
    L <- uniroot(calculate_lower_limit.3, c(phimin, phimax), .param = param, tol = tol)$root
  }

  # * * ** Upper CI limit * * **
  if (abs(estimate) == Inf || is.na(estimate)) {
    U <- Inf
  } else {
    U <- uniroot(calculate_upper_limit.3, c(phimin, phimax), .param = param, tol = tol)$root
  }

  printresults <- function() {
    cat_sprintf("The Tang asymptotic score CI: estimate = %6.4f (%g%% CI %6.4f to %6.4f)", estimate, 100 * (1 - alpha), L, U)
  }

  return(
    contingencytables_result(
      list(L = L, U = U, estimate = estimate),
      printresults
    )
  )
}


# ======================================
calculate_lower_limit.3 <- function(phi0, .param) {
  z <- .param$z
  T0 <- score_test_statistic_3(phi0, .param)
  f <- T0 - z
  return(f)
}


# ======================================
calculate_upper_limit.3 <- function(phi0, .param) {
  z <- .param$z
  T0 <- score_test_statistic_3(phi0, .param)
  f <- T0 + z
  return(f)
}


# =====================================
score_test_statistic_3 <- function(phi0, .param) {
  n11 <- .param$n11
  n12 <- .param$n12
  n21 <- .param$n21
  N <- .param$N
  p21tilde <- ML_estimate.3(phi0, .param)
  n1p <- n11 + n12
  np1 <- n11 + n21
  T0 <- (n1p - np1 * phi0) / sqrt(N * (1 + phi0) * p21tilde + (n11 + n12 + n21) * (phi0 - 1))
  return(T0)
}

# ==================================
ML_estimate.3 <- function(phi0, .param) {
  n11 <- .param$n11
  n12 <- .param$n12
  n21 <- .param$n21
  N <- .param$N
  A <- N * (1 + phi0)
  B <- (n11 + n21) * phi0^2 - (n11 + n12 + 2 * n21)
  C <- n21 * (1 - phi0) * (n11 + n12 + n21) / N
  p21tilde <- (-B + sqrt(B^2 - 4 * A * C)) / (2 * A)
  return(p21tilde)
}
