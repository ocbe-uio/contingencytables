#' @title The Pearson correlation coefficient
#' @description The Pearson correlation coefficient
#' @description Described in Chapter 7 "The rxc Table"
#' @param n the observed table (an rxc matrix)
#' @param a scores assigned to the rows
#' @param b scores assigned to the columns
#' @param alpha the nominal significance level, used to compute a 100(1-alpha) confidence interval
#' @importFrom stats cov
#' @examples
#'   Pearson_correlation_coefficient_rxc(table_7.7)
#'   Pearson_correlation_coefficient_rxc(table_7.8)
#'   Pearson_correlation_coefficient_rxc(table_7.9)
#' @export
#' @return An object of the [contingencytables_result] class,
#' basically a subclass of [base::list()]. Use the [utils::str()] function
#' to see the specific elements returned.
Pearson_correlation_coefficient_rxc <- function(
  n, a = seq_len(nrow(n)), b = seq_len(ncol(n)), alpha = 0.05
) {
  validateArguments(mget(ls()))

  # If no scores are given, use equally spaced scores
  r <- nrow(n)
  c <- ncol(n)

  N <- sum(n)

  # Put the observed data into long format
  Y1 <- rep(0, N)
  Y2 <- rep(0, N)
  id <- 0
  for (i in 1:r) {
    for (j in 1:c) {
      for (k in 1:n[i, j]) {
        id <- id + 1
        Y1[id] <- a[i]
        Y2[id] <- b[j]
      }
    }
  }

  # The covariance matrix of Y1 and Y2
  covar <- cov(cbind(Y1, Y2))

  # Estimate of the Pearson correlation coefficient
  rP <- covar[1, 2] / sqrt(covar[1, 1] * covar[2, 2])

  # Fisher Z transformation
  z <- atanh(rP) # Or, equivalently, = 0.5 * log((1 + rP) / (1-rP))

  # The 1-alpha percentile of the standard normal distribution
  z_alpha <- qnorm(1 - alpha / 2, 0, 1)

  # Confidence interval for z
  l <- z - z_alpha / sqrt(N - 3)
  u <- z + z_alpha / sqrt(N - 3)

  # Back transform to obtain the CI for the Pearson correlation coefficient
  L <- (exp(2 * l) - 1) / (exp(2 * l) + 1)
  U <- (exp(2 * u) - 1) / (exp(2 * u) + 1)

  return(
    contingencytables_result(
      list(rP = rP, L = L, U = U),
      sprintf("The Pearson correlation coefficient: r = %7.4f (%g%% CI %7.4f to %7.4f)", rP, 100 * (1 - alpha), L, U)
    )
  )
}
