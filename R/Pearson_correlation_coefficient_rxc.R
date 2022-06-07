#' @title The Pearson correlation coefficient
#' @description The Pearson correlation coefficient
#' @description Described in Chapter 7 "The rxc Table"
#' @param n the observed table (an rxc matrix)
#' @param a scores assigned to the rows
#' @param b scores assigned to the columns
#' @param alpha the nominal significance level, used to compute a 100(1-alpha) confidence interval
#' @param printresults display results (0 = no, 1 = yes)
#' @importFrom stats cov
#' @examples
#' # Colorectal cancer (Table 7.7)
#' n <- rbind(
#'   c(2, 4, 29, 19), c(7, 6, 116, 51), c(19, 27, 201, 76), c(18, 22, 133, 54)
#' )
#' Pearson_correlation_coefficient_rxc(n)
#' \dontrun{
#' # Breast Tumor (Table 7.8)
#' n <- matrix(
#'   c(15, 35, 6, 9, 6, 2, 4, 2, 11, 11, 0, 0, 1, 10, 21),
#'   ncol = 5, byrow = TRUE
#' )
#' Pearson_correlation_coefficient_rxc(n)
#'
#' # Self-rated health (Table 7.9)
#' n <- matrix(
#'   c(2, 3, 3, 3, 2, 58, 98, 14, 8, 162, 949, 252, 4, 48, 373, 369),
#'   ncol = 4, byrow = TRUE
#' )
#' Pearson_correlation_coefficient_rxc(n)
#' }
#'
#' @export
#' @return A list containing the statistic and the confindence interval limits
Pearson_correlation_coefficient_rxc <- function(n, a = seq_len(nrow(n)), b = seq_len(ncol(n)), alpha = 0.05,
                                                printresults = TRUE) {
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

  if (printresults) {
    .print("The Pearson correlation coefficient: r = %7.4f (%g%% CI %7.4f to %7.4f)\n", rP, 100 * (1 - alpha), L, U)
  }

  invisible(list(rP = rP, L = L, U = U))
}

.print <- function(s, ...) {
  print(sprintf(gsub("\n", "", s), ...), quote = FALSE)
}
