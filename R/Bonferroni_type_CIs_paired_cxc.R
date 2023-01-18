#' @title Bonferroni-type confidence intervals for differences of marginal probabilities
#' @description Bonferroni-type confidence intervals for differences of marginal probabilities
#' @description Described in Chapter 9 "The Paired kxk Table"
#' @param n the observed table (a cxc matrix)
#' @param alpha the nominal level, e.g. 0.05 for 95% CIs
#' @return A list containing lower, upper and point estimates of the statistic
#' @examples
#' Bonferroni_type_CIs_paired_cxc(peterson_2007)
#' @export
Bonferroni_type_CIs_paired_cxc <- function(n, alpha = 0.05) {
  validateArguments(mget(ls()))
  c <- nrow(n)
  nip <- apply(n, 1, sum)
  npi <- apply(n, 2, sum)
  N <- sum(n)

  # Estimates of the differences between the marginal probabilities
  deltahat <- (nip - npi) / N

  # The upper alpha / c percentile of the chi-squared distribution with one
  # degree of freedom
  chisqcminus1 <- qchisq(1 - alpha / c, 1)

  # Constants for the quadratic equation below
  A <- 1 + chisqcminus1 / N
  B <- -2 * deltahat
  C <- deltahat^2 - chisqcminus1 * (nip + npi - 2 * diag(n)) / (N^2)

  # Solve the quadratic equation defined by A, B, and C to find the simultaneous CIs
  L <- rep(0, c)
  U <- rep(0, c)
  for (i in 1:c) {
    if (B[i]^2 - 4 * A * C[i] > 0) {
      L[i] <- (-B[i] - sqrt(B[i]^2 - 4 * A * C[i])) / (2 * A)
      U[i] <- (-B[i] + sqrt(B[i]^2 - 4 * A * C[i])) / (2 * A)
    } else {
      L[i] <- -1
      U[i] <- 1
    }
  }

  # Output
  res <- list(
    name = "Bonferroni-type simultaneous intervals",
    statistics = list("lower" = L, "upper" = U, "deltahat" = deltahat)
  )
  class(res) <- "contingencytables_singletest"
  return(res)
}
