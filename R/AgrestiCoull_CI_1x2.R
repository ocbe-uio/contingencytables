#' @title The Agresti-Coull confidence interval for the binomial probability
#' @description Described in Chapter 2 "The 1x2 Table and the Binomial
#' Distribution"
#' @references Agresti A, Coull BA (1998) Approximate is better than "exact"
#' for interval estimation of binomial proportions. The American
#' Statistician; 52:119-126
#' @seealso Wald_CI_1x2
#' @param X the number of successes
#' @param n the total number of observations
#' @param alpha the nominal level, e.g. 0.05 for 95% CIs
#' @return An object of the [contingencytables_result] class,
#' basically a subclass of [base::list()]. Use the [utils::str()] function
#' to see the specific elements returned.
#' @examples
#' AgrestiCoull_CI_1x2(singh_2010["1st", "X"], singh_2010["1st", "n"])
#' AgrestiCoull_CI_1x2(singh_2010["2nd", "X"], singh_2010["2nd", "n"])
#' AgrestiCoull_CI_1x2(singh_2010["3rd", "X"], singh_2010["3rd", "n"])
#' with(singh_2010["4th", ], AgrestiCoull_CI_1x2(X, n)) # alternative syntax
#' AgrestiCoull_CI_1x2(ligarden_2010["X"], ligarden_2010["n"])
#' @export
AgrestiCoull_CI_1x2 <- function(X, n, alpha = 0.05) {
  validateArguments(mget(ls()))
  # Estimate of the binomial probability (pihat)
  estimate <- X / n

  # Add two successes and two failures and calculate the Wald CI
  res <- Wald_CI_1x2(X + 2, n + 4, alpha)
  estimate <- res[3]
  L <- res[1]
  U <- res[2]

  # Output
  printresults <- function() {
    cat_sprintf(
      "The Agresti-Coull CI: estimate = %6.4f (%g%% CI %6.4f to %6.4f)",
      estimate, 100 * (1 - alpha), L, U
    )
  }

  return(contingencytables_result(res, printresults))
}
