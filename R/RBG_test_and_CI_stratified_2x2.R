#' @title The RBG test and CI for a common odds ratio
#' @description The RBG test and CI for a common odds ratio
#' @description (A Wald-type test and CI based on the Mantel-Haenszel estimate)
#' @description Described in Chapter 10 "Stratified 2x2 Tables and Meta-Analysis"
#' @param n the observed table (a 2x2xk matrix, where k is the number of strata)
#' @param alpha the nominal level, e.g. 0.05 for 95# CIs
#' @examples
#' # Smoking and lung cancer (Doll and Hill, 1950)
#' RBG_test_and_CI_stratified_2x2(doll_hill_1950)
#'
#' # Prophylactice use of Lidocaine in myocardial infarction (Hine et al., 1989)
#' RBG_test_and_CI_stratified_2x2(hine_1989)
#'
#' @export
#' @return An object of the [contingencytables_result] class,
#' basically a subclass of [base::list()]. Use the [utils::str()] function
#' to see the specific elements returned.
RBG_test_and_CI_stratified_2x2 <- function(n, alpha = 0.05) {
  validateArguments(mget(ls()))

  nppk <- apply(n, 3, sum)

  # Get the Mantel-Haenszel overall estimate
  thetahatMH <- MantelHaenszel_estimate_stratified_2x2(n, "logit")[[1]]

  # Estimate the standard error
  A <- sum(n[1, 1, ] * n[2, 2, ] / nppk)
  B <- sum(n[1, 2, ] * n[2, 1, ] / nppk)
  C <- sum((n[1, 1, ] + n[2, 2, ]) * n[1, 1, ] * n[2, 2, ] / (nppk^2))
  D <- sum((n[1, 1, ] + n[2, 2, ]) * n[1, 2, ] * n[2, 1, ] / (nppk^2))
  E <- sum((n[1, 2, ] + n[2, 1, ]) * n[1, 1, ] * n[2, 2, ] / (nppk^2))
  F <- sum((n[1, 2, ] + n[2, 2, ]) * n[1, 2, ] * n[2, 1, ] / (nppk^2))
  SElog <- sqrt(C / (2 * A^2) + (D + E) / (2 * A * B) + F / (2 * B^2))

  # The Wald test statistic
  Z <- log(thetahatMH) / SElog

  # The two-sided P-value (reference distribution: standard normal)
  P <- 2 * (1 - pnorm(abs(Z), 0, 1))

  # The upper alpha / 2 percentile of the standard normal distribution
  z_alpha <- qnorm(1 - alpha / 2, 0, 1)

  # Calculate the confidence limits
  L <- thetahatMH * exp(-z_alpha * SElog)
  U <- thetahatMH * exp(z_alpha * SElog)

  printresults <- function() {
    my_sprintf_cat("The RBG test: P = %7.5f, Z = %6.3f\n", P, Z)
    my_sprintf_cat("The RBG CI: thetahatMH = %6.4f (%g%% CI %6.4f to %6.4f)", thetahatMH, 100 * (1 - alpha), L, U)
  }

  return(
    contingencytables_result(
      list(P = P, Z = Z, L = L, U = U, thetahatMH = thetahatMH, SElog = SElog),
      printresults
    )
  )
}
