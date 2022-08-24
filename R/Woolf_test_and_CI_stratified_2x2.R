#' @title The Woolf test and CI for a common odds ratio
#' @description The Woolf test and CI for a common odds ratio
#' @description (A Wald-type test and CI based on the inverse variance estimate)
#' @description Described in Chapter 10 "Stratified 2x2 Tables and
#' Meta-Analysis"
#' @param n the observed table (a 2x2xk matrix, where k is the number of strata)
#' @param alpha the nominal level, e.g. 0.05 for 95% CIs
#' @param printresults display results (FALSE = no, TRUE = yes)
#' @examples
#' # Smoking and lung cancer (Doll and Hill, 1950)
#' Woolf_test_and_CI_stratified_2x2(doll_hill_1950)
#'
#' # Prophylactice use of Lidocaine in myocardial infarction (Hine et al., 1989)
#' Woolf_test_and_CI_stratified_2x2(hine_1989)
#'
#' @export
#' @return A list containing the two-sided p-value, the Wald test statistic,
#' and the lower, upper and point estimate thetahatIV.
Woolf_test_and_CI_stratified_2x2 <- function(
  n, alpha = 0.05, printresults = TRUE
) {
  validateArguments(mget(ls()))

  # Get the inverse variance overall estimate and weights
  tmp <- InverseVariance_estimate_stratified_2x2(n, "logit", FALSE)
  thetahatIV <- tmp[[1]]
  v <- tmp[[3]]

  # Estimate the standard error
  SElog <- 1 / sqrt(sum(v))

  # The Wald test statistic
  Z <- log(thetahatIV) / SElog

  # The two-sided P-value (reference distribution: standard normal)
  P <- 2 * (1 - pnorm(abs(Z), 0, 1))

  # The upper alpha / 2 percentile of the standard normal distribution
  z_alpha <- qnorm(1 - alpha / 2, 0, 1)

  # Calculate the confidence limits
  L <- thetahatIV * exp(-z_alpha * SElog)
  U <- thetahatIV * exp(z_alpha * SElog)

  if (printresults) {
    .print("The Woolf test: P = %7.5f, Z = %6.3f\n", P, Z)
    .print(
      "The Woolf CI: thetahatIV = %6.4f (%g%% CI %6.4f to %6.4f)\n",
      thetahatIV, 100 * (1 - alpha), L, U
    )
  }

  invisible(list(P = P, Z = Z, L = L, U = U, thetahatIV = thetahatIV))
}
