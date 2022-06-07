#' @title The Spearman correlation coefficient
#' @description The Spearman correlation coefficient
#' @description Described in Chapter 7 "The rxc Table"
#' @param n the observed table (an rxc matrix)
#' @param alpha the nominal significance level, used to compute a 100(1-alpha)# confidence interval
#' @param printresults display results (FALSE = no, TRUE = yes)
#' @examples
#' # Colorectal cancer (Table 7.7)
#' n <- rbind(
#'   c(2, 4, 29, 19), c(7, 6, 116, 51), c(19, 27, 201, 76), c(18, 22, 133, 54)
#' )
#' Spearman_correlation_coefficient_rxc(n)
#' \dontrun{
#' # Breast Tumor (Table 7.8)
#' n <- matrix(
#'   c(15, 35, 6, 9, 6, 2, 4, 2, 11, 11, 0, 0, 1, 10, 21),
#'   ncol = 5, byrow = TRUE
#' )
#' Spearman_correlation_coefficient_rxc(n)
#'
#' # Self-rated health (Table 7.9)
#' n <- matrix(
#'   c(2, 3, 3, 3, 2, 58, 98, 14, 8, 162, 949, 252, 4, 48, 373, 369),
#'   ncol = 4, byrow = TRUE
#' )
#' Spearman_correlation_coefficient_rxc(n)
#' }
#'
#' @export
#' @return A list containing the Spearman correlation coefficient, and the Fieller and Bonett-Wright confidence intervals
Spearman_correlation_coefficient_rxc <- function(n, alpha = 0.05, printresults = TRUE) {
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

  if (printresults) {
    .print("Spearman correlation w / Fieller CI:       rho = %7.4f (%g%% CI %7.4f to %7.4f)\n", rho, 100 * (1 - alpha), L, U)
    .print("Spearman correlation w / Bonett-Wright CI: rho = %7.4f (%g%% CI %7.4f to %7.4f)\n", rho, 100 * (1 - alpha), L_BW, U_BW)
  }

  invisible(list(rho = rho, L = L, U = U, L_BW = L_BW, U_BW = U_BW))
}

.print <- function(s, ...) {
  print(sprintf(gsub("\n", "", s), ...), quote = FALSE)
}
