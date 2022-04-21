#' @title The RBG test and CI for a common odds ratio
#' @description The RBG test and CI for a common odds ratio
#' @description (A Wald-type test and CI based on the Mantel-Haenszel estimate)
#' @description Described in Chapter 10 "Stratified 2x2 Tables and Meta-Analysis"
#' @param n the observed table (a 2x2xk matrix, where k is the number of strata)
#' @param alpha the nominal level, e.g. 0.05 for 95# CIs
#' @param printresults display results (FALSE = no, TRUE = yes)
#' @examples
#' # Smoking and lung cancer (Doll and Hill, 1950)
#' n <- array(dim = c(2, 2, 2))
#' n[, , 1] <- matrix(c(647, 622, 2, 27), 2, byrow = TRUE)
#' n[, , 2] <- matrix(c(41, 28, 19, 32), 2, byrow = TRUE)
#' RBG_test_and_CI_stratified_2x2(n)
#'
#' # Prophylactice use of Lidocaine in myocardial infarction (Hine et al., 1989)
#' n <- array(0, dim = c(2, 2, 6))
#' n[, , 1] <- rbind(c(2, 37), c(1, 42))
#' n[, , 2] <- rbind(c(4, 40), c(4, 40))
#' n[, , 3] <- rbind(c(6, 101), c(4, 106))
#' n[, , 4] <- rbind(c(7, 96), c(5, 95))
#' n[, , 5] <- rbind(c(7, 103), c(3, 103))
#' n[, , 6] <- rbind(c(11, 143), c(4, 142))
#' RBG_test_and_CI_stratified_2x2(n)
#'
#' @export
#' @return A list containing the two-sided p-value (\code{P}), the Wald test
#' statistic (\code{Z}), the confidence limits (\code{L} and \code{U}), the
#' Mantel-Haenszel overall estimate (\code{thetahatMH}) and the standard error
#' (\code{SElog}).
RBG_test_and_CI_stratified_2x2 <- function(n, alpha = 0.05, printresults = TRUE) {
  nppk <- apply(n, 3, sum)

  # Get the Mantel-Haenszel overall estimate
  thetahatMH <- MantelHaenszel_estimate_stratified_2x2(n, "logit", FALSE)[[1]]

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

  if (printresults) {
    .print("The RBG test: P = %7.5f, Z = %6.3f\n", P, Z)
    .print("The RBG CI: thetahatMH = %6.4f (%g%% CI %6.4f to %6.4f)\n", thetahatMH, 100 * (1 - alpha), L, U)
  }

  invisible(list(P = P, Z = Z, L = L, U = U, thetahatMH = thetahatMH, SElog = SElog))
}

.print <- function(s, ...) {
  print(sprintf(gsub("\n", "", s), ...), quote = FALSE)
}
