#' @title The Cochran-Armitage confidence interval for trend in the linear model
#' @description The Cochran-Armitage confidence interval for trend in the linear model
#' @description Described in Chapter 5 "The Ordered rx2 Table"
#' @param n the observed counts (an rx2 matrix)
#' @param a scores assigned to the rows
#' @param alpha the nominal level, e.g. 0.05 for 95% CIs
#' @return An object of the [contingencytables_result] class,
#' basically a subclass of [base::list()]. Use the [utils::str()] function
#' to see the specific elements returned.
#' @examples
#' CochranArmitage_CI_rx2(mills_graubard_1987, c(1, 2, 3, 4, 5))
#' CochranArmitage_CI_rx2(indredavik_2008, c(1, 2, 3, 4, 5))
#' @export
CochranArmitage_CI_rx2 <- function(n, a = seq_len(nrow(n)), alpha = 0.05) {
  validateArguments(mget(ls()))
  r <- nrow(n)
  nip <- apply(n, 1, sum)
  N <- sum(n)
  abar <- sum(nip * a) / N
  pihat <- n[, 1] / nip

  U <- 0
  Saa <- 0
  s2 <- 0
  for (i in 1:r) {
    U <- U + nip[i] * (a[i] - abar) * pihat[i]
    Saa <- Saa + nip[i] * (a[i] - abar)^2
    s2 <- s2 + nip[i] * ((a[i] - abar)^2) * pihat[i] * (1 - pihat[i])
  }

  # Estimate of the linear slope
  betahat <- U / Saa

  # General estimate of the standard error
  SE <- sqrt(s2) / Saa

  # The Cochran-Armitage confidence interval
  z <- qnorm(1 - alpha / 2, 0, 1)
  L <- betahat - z * SE
  U <- betahat + z * SE

  # Output
  printresults <- function() {
    cat_sprintf(
      "Trend estimate and Cochran-Armitage CI:  betahat = %6.4f (%g%% CI %6.4f to %6.4f)",
      betahat, 100 * (1 - alpha), L, U
    )
  }
  return(
    contingencytables_result(
      list(lower = L, upper = U, estimate = betahat),
      printresults
    )
  )
}
