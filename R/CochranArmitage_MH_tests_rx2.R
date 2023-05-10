#' @title The Cochran-Armitage, modified Cochran-Armitage, and Mantel-Haenszel tests for trend
#' @description Described in Chapter 5 "The Ordered rx2 Table"
#' @param n the observed counts (an rx2 matrix)
#' @param a scores assigned to the rows
#' @examples
#' CochranArmitage_MH_tests_rx2(mills_graubard_1987, c(1, 2, 3, 4, 5))
#' CochranArmitage_MH_tests_rx2(indredavik_2008, c(1, 2, 3, 4, 5))
#' @export
#' @return An object of the [contingencytables_result] class,
#' basically a subclass of [base::list()]. Use the [utils::str()] function
#' to see the specific elements returned.
CochranArmitage_MH_tests_rx2 <- function(n, a) {
  validateArguments(mget(ls()))
  r <- nrow(n)
  nip <- apply(n, 1, sum)
  N <- sum(n)
  abar <- sum(nip * a) / N
  pihat <- n[, 1] / nip
  pihatcommon <- sum(n[, 1]) / N

  U <- 0
  Saa <- 0
  s2 <- 0
  for (i in 1:r) {
    U <- U + nip[i] * (a[i] - abar) * pihat[i]
    Saa <- Saa + nip[i] * (a[i] - abar)^2
    s2 <- s2 + nip[i] * ((a[i] - abar)^2) * pihat[i] * (1 - pihat[i])
  }

  # Estimate of the variance of U under H0
  s0 <- sqrt(pihatcommon * (1 - pihatcommon) * Saa)

  # General estimate of the variance of U
  s <- sqrt(s2)

  # The Cochran-Armitage test
  Z_CA <- U / s0
  P_CA <- 2 * (1 - pnorm(abs(Z_CA), 0, 1))

  # The modified Cochran-Armitage test
  Z_CA_mod <- U / s
  P_CA_mod <- 2 * (1 - pnorm(abs(Z_CA_mod), 0, 1))

  # The Mantel-Haenszel test
  Z_MH <- sqrt(N / (N - 1)) * Z_CA
  P_MH <- 2 * (1 - pnorm(abs(Z_MH), 0, 1))

  # Output arguments (observed statistics and P-values)
  results <- list()
  results$Z_CA <- Z_CA
  results$P_CA <- P_CA
  results$Z_CA_mod <- Z_CA_mod
  results$P_CA_mod <- P_CA_mod
  results$Z_MH <- Z_MH
  results$P_MH <- P_MH

  printresults <- function() {
    my_sprintf_cat(
      "Cochran-Armitage test:          T = %6.3f, P = %7.5f\n", Z_CA, P_CA
    )
    my_sprintf_cat(
      "Modified Cochran-Armitage test: T = %6.3f, P = %7.5f\n", Z_CA_mod, P_CA_mod
    )
    my_sprintf_cat(
      "Mantel-Haenszel test:           T = %6.3f, P = %7.5f", Z_MH, P_MH
    )
  }
  return(contingencytables_result(results, printresults))
}
