#' @title The Spearman correlation coefficient
#' @description The Spearman correlation coefficient
#' @description Described in Chapter 7 "The rxc Table"
#' @param n the observed table (an rxc matrix)
#' @param alpha the nominal significance level, used to compute a 100(1-alpha)# confidence interval
#' @examples
#' Spearman_correlation_coefficient_rxc(table_7.7)
#' Spearman_correlation_coefficient_rxc(table_7.8)
#' Spearman_correlation_coefficient_rxc(table_7.9)
#' @export
#' @return An object of the [contingencytables_result] class,
#' basically a subclass of [base::list()]. Use the [utils::str()] function
#' to see the specific elements returned.
Spearman_correlation_coefficient_rxc <- function(n, alpha = 0.05) {
  validateArguments(mget(ls()))

  r <- nrow(n)
  c <- ncol(n)
  nip <- apply(n, 1, sum)
  npj <- apply(n, 2, sum)
  N <- sum(n)

  # The midrank in each of the rows
  a <- rep(0, r)
  for (i in 1:r) {
    if (i > 1) {
      a[i] <- sum(nip[1:(i - 1)]) + (1 + nip[i]) / 2
    } else {
      a[i] <- (1 + nip[i]) / 2
    }
  }

  # The midrank in each of the columns
  b <- rep(0, c)
  for (j in 1:c) {
    if (j > 1) {
      b[j] <- sum(npj[1:(j - 1)]) + (1 + npj[j]) / 2
    } else {
      b[j] <- (1 + npj[j]) / 2
    }
  }

  # The Spearman correlation coefficient equals the Pearson correlation
  # coefficient on the midranks
  numerator <- 0
  for (i in 1:r) {
    for (j in 1:c) {
      numerator <- numerator + (a[i] - (N + 1) / 2) * (b[j] - (N + 1) / 2) * n[i, j] / N
    }
  }
  denominator1 <- 0
  for (i in 1:r) {
    denominator1 <- denominator1 + ((a[i] - (N + 1) / 2)^2) * nip[i] / N
  }
  denominator2 <- 0
  for (j in 1:c) {
    denominator2 <- denominator2 + ((b[j] - (N + 1) / 2)^2) * npj[j] / N
  }
  rho <- numerator / sqrt(denominator1 * denominator2)

  # Fisher Z transformation
  z <- atanh(rho) # equivalently, z = 0.5 * log((1 + rho) / (1-rho))

  # The 1-alpha percentile of the standard normal distribution
  z_alpha <- qnorm(1 - alpha / 2, 0, 1)

  # Confidence limits with the Fieller standard deviation
  l <- z - z_alpha * 1.06 / sqrt(N - 3)
  u <- z + z_alpha * 1.06 / sqrt(N - 3)

  # Confidence limits with the Bonett-Wright standard deviation
  l_BW <- z - z_alpha * sqrt((1 + (rho^2) / 2) / (N - 3))
  u_BW <- z + z_alpha * sqrt((1 + (rho^2) / 2) / (N - 3))

  # Back transform to obtain the CIs for the Spearman correlation coefficient
  L <- (exp(2 * l) - 1) / (exp(2 * l) + 1)
  U <- (exp(2 * u) - 1) / (exp(2 * u) + 1)
  L_BW <- (exp(2 * l_BW) - 1) / (exp(2 * l_BW) + 1)
  U_BW <- (exp(2 * u_BW) - 1) / (exp(2 * u_BW) + 1)

  printresults <- function() {
    cat_sprintf("Spearman correlation w / Fieller CI:       rho = %7.4f (%g%% CI %7.4f to %7.4f)\n", rho, 100 * (1 - alpha), L, U)
    cat_sprintf("Spearman correlation w / Bonett-Wright CI: rho = %7.4f (%g%% CI %7.4f to %7.4f)", rho, 100 * (1 - alpha), L_BW, U_BW)
  }

  return(
    contingencytables_result(
      list("rho" = rho, "lower" = L, "upper" = U, "L_BW" = L_BW, "U_BW" = U_BW),
      printresults
    )
  )
}
