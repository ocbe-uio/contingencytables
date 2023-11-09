#' @title The Quesenberry-Hurst Wilson score simultaneous intervals for the multinomial probabilities
#' @description The Quesenberry-Hurst Wilson score simultaneous intervals for the multinomial probabilities
#' @description (with Scheffe adjustment)
#' @description Described in Chapter 3 "The 1xc Table and the Multinomial Distribution"
#' @param n the observed counts (a 1xc vector, where c is the number of categories)
#' @param alpha the nominal level, e.g. 0.05 for 95% CIs
#' @examples
#' # Genotype counts for SNP rs 6498169 in RA patients
#' QuesenberryHurst_Wilson_score_CIs_1xc(n = snp6498169$complete$n)
#' @export
#' @return An object of the [contingencytables_result] class,
#' basically a subclass of [base::list()]. Use the [utils::str()] function
#' to see the specific elements returned.
QuesenberryHurst_Wilson_score_CIs_1xc <- function(n, alpha = 0.05) {
  validateArguments(mget(ls()))

  c0 <- length(n)
  N <- sum(n)

  # Estimates of the multinomial probabilities
  pihat <- n / N

  # Simultaneous confidence intervals with Scheffe adjustment
  L <- rep(0, c0)
  U <- rep(0, c0)
  Scheffe <- qchisq(1 - alpha, c0 - 1)
  for (i in 1:c0) {
    L[i] <- (
      Scheffe + 2 * N * pihat[i] - sqrt(
        Scheffe^2 + 4 * N * Scheffe * pihat[i] * (1 - pihat[i])
      )
    ) / (2 * Scheffe + 2 * N)
    U[i] <- (
      Scheffe + 2 * N * pihat[i] + sqrt(
        Scheffe^2 + 4 * N * Scheffe * pihat[i] * (1 - pihat[i])
      )
    ) / (2 * Scheffe + 2 * N)
  }

  printresults <- function() {
    cat("The Quesenberry-Hurst Wilson score simultaneous intervals\n")
    for (i in 1:c0) {
      cat_sprintf(
        "  pi_%i: estimate = %6.4f (%6.4f to %6.4f)\n",
        i, pihat[i], L[i], U[i]
      )
    }
  }

  res <- list(lower = L, upper = U, estimate = pihat)
  return(contingencytables_result(res, printresults))
}
