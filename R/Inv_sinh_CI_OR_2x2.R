#' @title The inverse hyperbolic sine confidence interval for the odds ratio
#' @description The inverse hyperbolic sine confidence interval for the odds ratio
#' @description Described in Chapter 4 "The 2x2 Table"
#' @param n the observed counts (a 2x2 matrix)
#' @param alpha the nominal level, e.g. 0.05 for 95% CIs
#' @param printresults display results (FALSE = no, TRUE = yes)
#' @examples
#' # A case-control study of GADA exposure on IPEX syndrome (Lampasona et al., 2013):
#' n <- matrix(c(9, 4, 4, 10), nrow = 2, byrow = TRUE)
#' Inv_sinh_CI_OR_2x2(n)
#' # The association between CHRNA4 genotype and XFS (Ritland et al., 2007):
#' n <- matrix(c(0, 16, 15, 57), nrow = 2, byrow = TRUE)
#' Inv_sinh_CI_OR_2x2(n)
#' @export
#' @return A data frame containing lower, upper and point estimates of the statistic
Inv_sinh_CI_OR_2x2 <- function(n, alpha = 0.05, printresults = TRUE) {
  # Estimate of the odds ratio (thetahat)
  estimate <- n[1, 1] * n[2, 2] / (n[1, 2] * n[2, 1])

  # The upper alpha / 2 percentile of the standard normal distribution
  z <- qnorm(1 - alpha / 2, 0, 1)

  # Calculate the confidence limits
  tmp <- asinh(0.5 * z * sqrt(1 / n[1, 1] + 1 / n[1, 2] + 1 / n[2, 1] + 1 / n[2, 2]))
  L <- exp(log(estimate) - 2 * tmp)
  U <- exp(log(estimate) + 2 * tmp)

  # Fix zero cell cases
  if (n[1, 1] == 0) {
    U <- (z^2) * n[2, 2] / (n[1, 2] * n[2, 1])
  }
  if (n[2, 2] == 0) {
    U <- n[1, 1] * (z^2) / (n[1, 2] * n[2, 1])
  }
  if (n[1, 1] == 0 && n[2, 2] == 0) {
    U <- ((z^2) * (z^2)) / (n[1, 2] * n[2, 1])
  }
  if (n[1, 2] == 0) {
    L <- n[1, 1] * n[2, 2] / ((z^2) * n[2, 1])
  }
  if (n[2, 1] == 0) {
    L <- n[1, 1] * n[2, 2] / (n[1, 2] * (z^2))
  }
  if (n[1, 2] == 0 && n[2, 1] == 0) {
    L <- n[1, 1] * n[2, 2] / ((z^2) * (z^2))
  }

  if (printresults) {
    print(
      sprintf(
        "The inverse sinh CI: estimate = %6.4f (%g%% CI %6.4f to %6.4f)",
        estimate, 100 * (1 - alpha), L, U
      ),
      quote = FALSE
    )
  }

  res <- data.frame(lower = L, upper = U, estimate = estimate)
  invisible(res)
}
