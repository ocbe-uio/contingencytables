#' @title The Clopper-Pearson exact confidence interval for the binomial probability (beta version)
#' @description The Clopper-Pearson exact confidence interval for the binomial probability
#' @description (defined via the beta distribution)
#' @description Described in Chapter 2 "The 1x2 Table and the Binomial Distribution"
#' @references Brown LD, Cai T, DasGupta A (2001) Interval estimation for a
#' binomial proportion. Statistical Science; 16:101-133
#' @param X the number of successes
#' @param n the total number of observations
#' @param alpha the nominal level, e.g. 0.05 for 95# CIs
#' @seealso ClopperPearson_exact_CI_1x2
#' @return An object of the [contingencytables_result] class,
#' basically a subclass of [base::list()]. Use the [utils::str()] function
#' to see the specific elements returned.
#' @examples
#' ClopperPearson_exact_CI_1x2_beta_version(singh_2010["1st", "X"], singh_2010["1st", "n"])
#' ClopperPearson_exact_CI_1x2_beta_version(singh_2010["2nd", "X"], singh_2010["2nd", "n"])
#' ClopperPearson_exact_CI_1x2_beta_version(singh_2010["3rd", "X"], singh_2010["3rd", "n"])
#' with(singh_2010["4th", ], ClopperPearson_exact_CI_1x2_beta_version(X, n)) # alternative syntax
#' ClopperPearson_exact_CI_1x2_beta_version(ligarden_2010["X"], ligarden_2010["n"])
#' @export
ClopperPearson_exact_CI_1x2_beta_version <- function(X, n, alpha = 0.05) {
  validateArguments(mget(ls()))
  # Estimate of the binomial probability (pihat)
  estimate <- X / n

  L <- qbeta(alpha / 2, X, n - X + 1)
  U <- qbeta(1 - alpha / 2, X + 1, n - X)

  # L is not defined (through the expression above) for X = 0
  if (X == 0) {
    L <- 0
  }

  # U is not defined (through the expression above) for X = n
  if (X == n) {
    U <- 1
  }

  # Output
  printresults <- function() {
    my_sprintf_cat(
      "The Clopper Pearson exact CI: estimate = %6.4f (%g%% CI %6.4f to %6.4f)",
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
