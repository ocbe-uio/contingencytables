#' @title Jeffreys confidence interval for the binomial probability
#' @description Jeffreys confidence interval for the binomial probability
#' @description Described in Chapter 2 "The 1x2 Table and the Binomial Distribution"
#' @param X the number of successes
#' @param n the total number of observations
#' @param alpha the nominal level, e.g. 0.05 for 95% CIs
#' @importFrom stats qbeta
#' @examples
#' Jeffreys_CI_1x2(singh_2010["1st", "X"], singh_2010["1st", "n"])
#' Jeffreys_CI_1x2(singh_2010["2nd", "X"], singh_2010["2nd", "n"])
#' Jeffreys_CI_1x2(singh_2010["3rd", "X"], singh_2010["3rd", "n"])
#' with(singh_2010["4th", ], Jeffreys_CI_1x2(X, n)) # alternative syntax
#' Jeffreys_CI_1x2(ligarden_2010["X"], ligarden_2010["n"])
#' @export
#' @return An object of the [contingencytables_result] class,
#' basically a subclass of [base::list()]. Use the [utils::str()] function
#' to see the specific elements returned.
Jeffreys_CI_1x2 <- function(X, n, alpha = 0.05) {
  validateArguments(mget(ls()))

  # Estimate of the binomial probability (pihat)
  estimate <- X / n

  # Calculate the confidence limits with Jeffreys noninformative prior, B(0.5, 0.5)
  L <- qbeta(alpha / 2, X + 0.5, n - X + 0.5)
  U <- qbeta(1 - alpha / 2, X + 0.5, n - X + 0.5)

  return(
    contingencytables_result(
      list("lower" = L, "upper" = U, "estimate" = estimate),
      sprintf(
        "The Jeffreys CI: estimate = %6.4f (%g%% CI %6.4f to %6.4f)",
        estimate, 100 * (1 - alpha), L, U
      )
    )
  )
}
