#' @title The Goodman Wald simultaneous intervals for the multinomial probabilities
#' @description The Goodman Wald simultaneous intervals for the multinomial probabilities
#' @description (with Bonferroni adjustment)
#' @description Described in Chapter 3 "The 1xc Table and the Multinomial Distribution"
#' @param n the observed counts (a 1xc vector, where c is the number of categories)
#' @param alpha the nominal level, e.g. 0.05 for 95# CIs
#' @examples
#' Goodman_Wald_CIs_1xc(n = snp6498169$complete$n)
#' @export
#' @return An object of the [contingencytables_result] class,
#' basically a subclass of [base::list()]. Use the [utils::str()] function
#' to see the specific elements returned.
Goodman_Wald_CIs_1xc <- function(n, alpha = 0.05) {
  validateArguments(mget(ls()))
  c0 <- length(n)
  N <- sum(n)

  # Estimates of the multinomial probabilities
  pihat <- n / N

  # Simultaneous confidence intervals with Bonferroni adjustment
  L <- rep(0, c0)
  U <- rep(0, c0)
  Bonferroni <- qchisq(1 - alpha / c0, 1)
  for (i in 1:c0) {
    L[i] <- pihat[i] - sqrt(Bonferroni * pihat[i] * (1 - pihat[i]) / N)
    U[i] <- pihat[i] + sqrt(Bonferroni * pihat[i] * (1 - pihat[i]) / N)
  }

  printresults <- function() {
    cat("The Goodman Wald simultaneous intervals\n ")
    sprintf("  pi_%i: estimate = %6.4f (%6.4f to %6.4f)\n", seq_len(c0), pihat, L, U)
  }
  res <- list("lower" = L, "upper" = U, "estimate" = pihat)
  return(contingencytables_result(res, printresults))
}
