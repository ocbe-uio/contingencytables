#' @title The likelihood ratio confidence interval for the binomial probability
#' @description The likelihood ratio confidence interval for the binomial
#' probability. Described in Chapter 2 "The 1x2 Table and the Binomial
#' Distribution"
#' @param X the number of successes
#' @param n the total number of observations
#' @param alpha the nominal level, e.g. 0.05 for 95# CIs
#' @examples
#' LR_CI_1x2(singh_2010["1st", "X"], singh_2010["1st", "n"])
#' LR_CI_1x2(singh_2010["2nd", "X"], singh_2010["2nd", "n"])
#' LR_CI_1x2(singh_2010["3rd", "X"], singh_2010["3rd", "n"])
#' with(singh_2010["4th", ], LR_CI_1x2(X, n)) # alternative syntax
#' LR_CI_1x2(ligarden_2010["X"], ligarden_2010["n"])
#' @export
#' @return An object of the [contingencytables_result] class,
#' basically a subclass of [base::list()]. Use the [utils::str()] function
#' to see the specific elements returned.
LR_CI_1x2 <- function(X, n, alpha = 0.05) {
  validateArguments(mget(ls()))

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

  return(
    contingencytables_result(
      list(lower = L, upper = U, estimate = estimate),
      sprintf(
        "The likelihood ratio CI: estimate = %6.4f (%g%% CI %6.4f to %6.4f)",
        estimate, 100 * (1 - alpha), L, U
      )
    )
  )
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
