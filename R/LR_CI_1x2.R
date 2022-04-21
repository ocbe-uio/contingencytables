#' @title The likelihood ratio confidence interval for the binomial probability
#' @description The likelihood ratio confidence interval for the binomial
#' probability. Described in Chapter 2 "The 1x2 Table and the Binomial
#' Distribution"
#' @param X the number of successes
#' @param n the total number of observations
#' @param alpha the nominal level, e.g. 0.05 for 95# CIs
#' @param printresults display results (0 = no, 1 = yes)
#' @examples
#'
#' # The number of 1st order male births (Singh et al. 2010)
#' LR_CI_1x2(X = 250, n = 533)
#' # The number of 2nd order male births (Singh et al. 2010)
#' LR_CI_1x2(X = 204, n = 412)
#' # The number of 3rd order male births (Singh et al. 2010)
#' LR_CI_1x2(X = 103, n = 167)
#' # The number of 4th order male births (Singh et al. 2010)
#' LR_CI_1x2(X = 33, n = 45)
#' # Ligarden et al. (2010)
#' LR_CI_1x2(X = 13, n = 16)
#' @export
#' @return A vector containing lower, upper and point estimates of the statistic
LR_CI_1x2 <- function(X, n, alpha = 0.05, printresults = TRUE) {

  # Define global variables that are needed in the LR test statistic function
  # below
  z <- qnorm(1 - alpha / 2, 0, 1)

  # Estimate of the binomial probability (pihat)
  estimate <- X / n

  # Use Matlabs fzero function to solve the equations T - z = 0 and T + z = 0,
  # where T is the LR test statistic
  tol <- 0.00000001

  # Find the lower CI limit
  if (estimate == 0) {
    L <- 0
  } else {
    L <- uniroot(
      LR_test_statistic,
      interval = c(tol, X / n), X = X, n = n, z = z, tol = tol
    )$root
  }

  # Find the upper CI limit
  if (estimate == 1) {
    U <- 1
  } else {
    U <- uniroot(
      LR_test_statistic,
      interval = c(max(tol, X / n), 1 - tol), X = X, n = n,
      z = z, tol = tol
    )$root
  }

  if (printresults) {
    print(
      sprintf(
        "The likelihood ratio CI: estimate = %6.4f (%g%% CI %6.4f to %6.4f)",
        estimate, 100 * (1 - alpha), L, U
      )
    )
  }

  res <- c(L, U, estimate)
  names(res) <- c("lower", "upper", "estimate")
  invisible(res)
}


# =================================

LR_test_statistic <- function(pi0, X, n, z) {
  if (X == 0) {
    lhs <- n * log(1 - pi0)
    rhs <- -(z^2) / 2
  } else if (X == n) {
    lhs <- X * log(pi0)
    rhs <- -(z^2) / 2
  } else {
    lhs <- X * log(pi0) + (n - X) * log(1 - pi0)
    rhs <- X * log(X / n) + (n - X) * log(1 - X / n) - (z^2) / 2
  }
  T0 <- lhs - rhs
  return(T0)
}
