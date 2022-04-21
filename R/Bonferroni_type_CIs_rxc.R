#' @title The Bonferroni-type simultaneous confidence intervals for the differences pi_1|i - pi_1|j
#' @description The Bonferroni-type simultaneous confidence intervals for the differences pi_1|i - pi_1|j
#' @description Described in Chapter 7 "The rxc Table"
#' @param n the observed counts (an rx2 vector)
#' @param alpha the nominal level, e.g. 0.05 for 95% CIs
#' @param printresults display results (FALSE = no, TRUE = yes)
#' @return A list containing lower, upper and point estimates of the statistic
#' @examples
#' # Example: Treatment for ear infection
#' n <- rbind(c(40, 25), c(54, 7), c(63, 10))
#' Bonferroni_type_CIs_rxc(n)
#' @export
Bonferroni_type_CIs_rxc <- function(n, alpha = 0.05, printresults = TRUE) {
  r <- nrow(n)
  nip <- apply(n, 1, sum)
  C <- r * (r - 1) / 2

  # Estimates of the probabilities of success and their differences
  pihat <- n[, 1] / nip
  differences <- rep(0, C)
  k <- 0
  for (i in 1:r) {
    for (j in min(r, i + 1):r) {
      k <- k + 1
      differences[k] <- pihat[i] - pihat[j]
    }
  }

  # Simultaneous confidence intervals with Scheffe adjustment
  L <- rep(0, C)
  U <- rep(0, C)
  Bonferroni <- qchisq(1 - alpha / C, 1)
  k <- 0
  for (i in 1:r) {
    for (j in min(r, i + 1):r) {
      k <- k + 1
      L[k] <- differences[k] - sqrt(Bonferroni * (pihat[i] * (1 - pihat[i]) / nip[i] + pihat[j] * (1 - pihat[j]) / nip[j]))
      U[k] <- differences[k] + sqrt(Bonferroni * (pihat[i] * (1 - pihat[i]) / nip[i] + pihat[j] * (1 - pihat[j]) / nip[j]))
    }
  }

  if (printresults) {
    print(sprintf("The Bonferroni-type simultaneous intervals"), quote = FALSE)
    k <- 0
    for (i in 1:r) {
      for (j in min(r, i + 1):r) {
        k <- k + 1
        print(sprintf("  pi_1|%i - pi_1|%i: estimate = %6.4f (%6.4f to %6.4f)", i, j, differences[k], L[k], U[k]), quote = FALSE)
      }
    }
  }

  invisible(list(L = L, U = U, differences = differences))
}
