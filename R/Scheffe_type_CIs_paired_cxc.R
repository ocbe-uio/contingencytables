#' @title Scheffe-type confidence intervals for differences of marginal probabilities
#' @description Scheffe-type confidence intervals for differences of marginal probabilities
#' @description Described in Chapter 9 "The Paired kxk Table"
#' @param n the observed table (a cxc matrix)
#' @param alpha the nominal level, e.g. 0.05 for 95% CIs
#' @examples
#' # Pretherapy susceptability of pathogens (Peterson et al., 2007)
#' Scheffe_type_CIs_paired_cxc(peterson_2007)
#' @export
#' @return An object of the [contingencytables_result] class,
#' basically a subclass of [base::list()]. Use the [utils::str()] function
#' to see the specific elements returned.
Scheffe_type_CIs_paired_cxc <- function(n, alpha = 0.05) {
  validateArguments(mget(ls()))

  c <- nrow(n)
  nip <- apply(n, 1, sum)
  npi <- apply(n, 2, sum)
  N <- sum(n)

  # Estimates of the differences between the marginal probabilities
  deltahat <- (nip - npi) / N

  # The upper alpha percentile of the chi-squared distribution with c-1
  # degrees of freedom
  chisqcminus1 <- qchisq(1 - alpha, c - 1)

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

  printresults <- function() {
    cat("Scheffe-type simultaneous intervals\n")
    for (i in 1:c) {
      cat_sprintf("  pi_%g+ vs pi_+%g: delta = %7.4f (%7.4f to %7.4f)\n", i, i, deltahat[i], L[i], U[i])
    }
  }

  return(
    contingencytables_result(
      list("lower" = L, "upper" = U, "deltahat" = deltahat),
      printresults
    )
  )
}
