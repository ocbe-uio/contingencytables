#' @title Kendall's tau-b with confidence interval based on the Fieller standard deviation
#' @description Kendall's tau-b with confidence interval based on the Fieller standard deviation
#' @description Described in Chapter 7 "The rxc Table"
#' @param n the observed table (an rxc matrix)
#' @param alpha the nominal significance level, used to compute a 100(1-alpha)% confidence interval
#' @examples
#' Kendalls_tau_b_rxc(table_7.7)
#' Kendalls_tau_b_rxc(table_7.8)
#' Kendalls_tau_b_rxc(table_7.9)
#' @export
#' @return An object of the [contingencytables_result] class,
#' basically a subclass of [base::list()]. Use the [utils::str()] function
#' to see the specific elements returned.
Kendalls_tau_b_rxc <- function(n, alpha = 0.05) {
  validateArguments(mget(ls()))

  nip <- apply(n, 1, sum)
  npj <- apply(n, 2, sum)
  N <- sum(n)

  # Get the number of concordant and discordant pairs from the gamma
  # coefficient
  tmp <- gamma_coefficient_rxc(n)
  C <- tmp[[2]]
  D <- tmp[[3]]

  # The number of pairs tied on the rows
  T1 <- sum(nip * (nip - 1) / 2)

  # The number of pairs tied on the columns
  T2 <- sum(npj * (npj - 1) / 2)

  # Kendall's tau-b
  tau_b <- (C - D) / sqrt((N * (N - 1) / 2 - T1) * (N * (N - 1) / 2 - T2))

  # Fisher Z transformation
  z <- atanh(tau_b) # Or, equivalently, z = 0.5 * log((1 + tau_b) / (1-tau_b))

  # The 1-alpha percentile of the standard normal distribution
  z_alpha <- qnorm(1 - alpha / 2, 0, 1)

  # Confidence limits with the Fieller standard deviation
  l <- z - z_alpha * sqrt(0.437 / (N - 4))
  u <- z + z_alpha * sqrt(0.437 / (N - 4))

  # Back transform to obtain the CIs for Kendall's tau-b
  L <- (exp(2 * l) - 1) / (exp(2 * l) + 1)
  U <- (exp(2 * u) - 1) / (exp(2 * u) + 1)

  return(
    contingencytables_result(
      list(tau_b = tau_b, L = L, U = U),
      sprintf(
        "Kendalls tau-b w / Fieller CI: tau-b = %7.4f (%g%% CI %7.4f to %7.4f)",
        tau_b, 100 * (1 - alpha), L, U
      )
    )
  )
}
