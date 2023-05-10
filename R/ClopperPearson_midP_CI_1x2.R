#' @title The Clopper-Pearson mid-P confidence interval
#' @description The Clopper-Pearson mid-P confidence interval for the binomial probability
#' Described in Chapter 2 "The 1x2 Table and the Binomial Distribution"
#'
#' @param X the number of successes
#' @param n the total number of observations
#' @param alpha the nominal level, e.g. 0.05 for 95% CIs
#' @return An object of the [contingencytables_result] class,
#' basically a subclass of [base::list()]. Use the [utils::str()] function
#' to see the specific elements returned.
#' @examples
#' ClopperPearson_midP_CI_1x2(singh_2010["1st", "X"], singh_2010["1st", "n"])
#' ClopperPearson_midP_CI_1x2(singh_2010["2nd", "X"], singh_2010["2nd", "n"])
#' ClopperPearson_midP_CI_1x2(singh_2010["3rd", "X"], singh_2010["3rd", "n"])
#' with(singh_2010["4th", ], ClopperPearson_midP_CI_1x2(X, n)) # alternative syntax
#' ClopperPearson_midP_CI_1x2(ligarden_2010["X"], ligarden_2010["n"])
#' @export
ClopperPearson_midP_CI_1x2 <- function(X, n, alpha = 0.05) {
  validateArguments(mget(ls()))
  # Estimate of the binomial probability (pihat)
  estimate <- X / n

  # Use Matlabs fzero function to solve the equations for the confidence limits
  tol <- 0.00000001

  # Find the lower CI limit
  if (estimate == 0) {
    L <- 0
  } else {
    L <- uniroot(calculate_L_CP2, interval = c(0, 1), X = X, n = n, alpha = alpha, tol = tol)$root
  }

  # Find the upper CI limit
  if (estimate == 1) {
    U <- 1
  } else {
    U <- uniroot(calculate_U_CP2, interval = c(0, 1), X = X, n = n, alpha = alpha, tol = tol)$root
  }

  # Output
  printresults <- function() {
    my_sprintf_cat(
      "The Clopper Pearson mid-P CI: estimate = %6.4f (%g%% CI %6.4f to %6.4f)",
      estimate, 100 * (1 - alpha), L, U
    )
  }
  return(
    contingencytables_result(
      list(lower = L, upper = U, estimate = estimate),
      printresults
    )
  )
}

calculate_L_CP2 <- function(L0, X, n, alpha) {
  # global Xglobal nglobal alphaglobal
  T0 <- sum(dbinom(X:n, n, L0))
  Tobs <- dbinom(X, n, L0)
  L <- T0 - Tobs / 2 - alpha / 2
  return(L)
}

calculate_U_CP2 <- function(U0, X, n, alpha) {
  # global Xglobal nglobal alphaglobal
  T0 <- sum(dbinom(0:X, n, U0))
  Tobs <- dbinom(X, n, U0)
  U <- T0 - Tobs / 2 - alpha / 2
  return(U)
}
