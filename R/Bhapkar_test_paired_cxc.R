#' @title The Bhapkar test for marginal homogeneity
#' @description The Bhapkar test for marginal homogeneity
#' @description Described in Chapter 9 "The Paired cxc Table"
#' @param n the observed table (a cxc matrix)
#' @return An object of the [contingencytables_result] class,
#' basically a subclass of [base::list()]. Use the [utils::str()] function
#' to see the specific elements returned.
#' @examples
#' Bhapkar_test_paired_cxc(peterson_2007)
#' @export
Bhapkar_test_paired_cxc <- function(n) {
  validateArguments(mget(ls()))
  c <- nrow(n)
  nip <- apply(n, 1, sum)
  npi <- apply(n, 2, sum)
  N <- sum(n)

  # Compute the differences between the marginal sums
  d <- nip[1:(c - 1)] - npi[1:(c - 1)]

  if (sum(d) == 0) {
    return(cat("No differences between the marginal sums\nP = 1.0"))
  }

  # Form the sample covariance matrix
  Sigmahat <- matrix(0, c - 1, c - 1)
  for (i in 1:(c - 1)) {
    Sigmahat[i, i] <- nip[i] + npi[i] - 2 * n[i, i] - ((nip[i] - npi[i])^2) / N
    for (j in 1:(c - 1)) {
      if (i != j) {
        Sigmahat[i, j] <- -(n[i, j] + n[j, i]) - ((npi[i] - nip[i]) * (npi[j] - nip[j])) / N
      }
    }
  }

  # The Bhapkar test statistic
  T0 <- sum(d * solve(Sigmahat, d))
  if (is.na(T0)) {
    return(cat("The Bhapkar test statistic is not computable\nP = 1.0"))
  }

  # Reference distribution: chi-squared with c-1 degrees of freedom
  df <- c - 1
  P <- 1 - pchisq(T0, df)

  # Output
  printresults <- function() {
    cat_sprintf(
      "The Bhapkar test for marginal homogenity: P = %8.6f, T = %6.3f (df = %g)",
      P, T0, df
    )
  }
  return(
    contingencytables_result(
      list("Pvalue" = P, "T" = T0, "df" = df),
      printresults
    )
  )
}
