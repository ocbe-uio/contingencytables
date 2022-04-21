#' @title Kendall's tau-b with confidence interval based on the Fieller standard deviation
#' @description Kendall's tau-b with confidence interval based on the Fieller standard deviation
#' @description Described in Chapter 7 "The rxc Table"
#' @param n the observed table (an rxc matrix)
#' @param alpha the nominal significance level, used to compute a 100(1-alpha)% confidence interval
#' @param printresults display results (FALSE = no, TRUE = yes)
#' @examples
#' # Colorectal cancer (Table 7.7)
#' n <- rbind(
#'   c(2, 4, 29, 19), c(7, 6, 116, 51), c(19, 27, 201, 76), c(18, 22, 133, 54)
#' )
#' Kendalls_tau_b_rxc(n)
#'
#' # Breast Tumor (Table 7.8)
#' n <- matrix(
#'   c(15, 35, 6, 9, 6, 2, 4, 2, 11, 11, 0, 0, 1, 10, 21),
#'   ncol = 5, byrow = TRUE
#' )
#' Kendalls_tau_b_rxc(n)
#'
#' # Self-rated health (Table 7.9)
#' n <- matrix(
#'   c(2, 3, 3, 3, 2, 58, 98, 14, 8, 162, 949, 252, 4, 48, 373, 369),
#'   ncol = 4, byrow = TRUE
#' )
#' Kendalls_tau_b_rxc(n)
#'
#' @export
#' @return A list containing the statistic and the confindence interval limits
Kendalls_tau_b_rxc <- function(n, alpha = 0.05, printresults = TRUE) {
  nip <- apply(n, 1, sum)
  npj <- apply(n, 2, sum)
  N <- sum(n)

  # Get the number of concordant and discordant pairs from the gamma
  # coefficient
  tmp <- gamma_coefficient_rxc(n, 0)
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

  if (printresults) {
    print(sprintf("Kendalls tau-b w / Fieller CI: tau-b = %7.4f (%g%% CI %7.4f to %7.4f)", tau_b, 100 * (1 - alpha), L, U), quote = FALSE)
  }

  invisible(list(tau_b = tau_b, L = L, U = U))
}
