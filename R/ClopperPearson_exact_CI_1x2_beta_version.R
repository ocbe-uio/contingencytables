#' @title The Clopper-Pearson exact confidence interval for the binomial probability (beta version)
#' @description The Clopper-Pearson exact confidence interval for the binomial probability
#' @description (defined via the beta distribution)
#' @description Described in Chapter 2 "The 1x2 Table and the Binomial Distribution"
#' @references Brown LD, Cai T, DasGupta A (2001) Interval estimation for a
#' binomial proportion. Statistical Science; 16:101-133
#' @param X the number of successes
#' @param n the total number of observations
#' @param alpha the nominal level, e.g. 0.05 for 95# CIs
#' @param printresults display results (FALSE = no, TRUE = yes)
#' @seealso ClopperPearson_exact_CI_1x2
#' @return A list containing lower, upper and point estimates of the statistic
#' @examples
#' # The number of 1st order male births (Singh et al. 2010)
#' ClopperPearson_exact_CI_1x2_beta_version(X = 250, n = 533)
#' # The number of 2nd order male births (Singh et al. 2010)
#' ClopperPearson_exact_CI_1x2_beta_version(X = 204, n = 412)
#' # The number of 3rd order male births (Singh et al. 2010)
#' ClopperPearson_exact_CI_1x2_beta_version(X = 103, n = 167)
#' # The number of 4th order male births (Singh et al. 2010)
#' ClopperPearson_exact_CI_1x2_beta_version(X = 33, n = 45)
#' # Ligarden et al. (2010)
#' ClopperPearson_exact_CI_1x2_beta_version(X = 13, n = 16)
#' @export
ClopperPearson_exact_CI_1x2_beta_version <- function(X, n, alpha = 0.05, printresults = TRUE) {
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

  if (printresults) {
    .print("The Clopper Pearson exact CI: estimate = %6.4f (%g%% CI %6.4f to %6.4f)\n", estimate, 100 * (1 - alpha), L, U)
  }

  invisible(list(L = L, U = U, estimate = estimate))
}

.print <- function(s, ...) {
  print(sprintf(gsub("\n", "", s), ...), quote = FALSE)
}
