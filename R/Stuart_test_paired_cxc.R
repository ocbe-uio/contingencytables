#' @title The Stuart test for marginal homogeneity
#' @description The Stuart test for marginal homogeneity
#' @description Described in Chapter 9 "The Paired cxc Table"
#' @param n the observed table (a cxc matrix)
#' @examples
#' # Pretherapy susceptability of pathogens (Peterson et al., 2007)
#' Stuart_test_paired_cxc(peterson_2007)
#' @export
#' @return An object of the [contingencytables_result] class,
#' basically a subclass of [base::list()]. Use the [utils::str()] function
#' to see the specific elements returned.
Stuart_test_paired_cxc <- function(n) {
  validateArguments(mget(ls()))

  c <- nrow(n)
  nip <- apply(n, 1, sum)
  npi <- apply(n, 2, sum)

  # Compute the differences between the marginal sums
  d <- nip[1:(c - 1)] - npi[1:(c - 1)]

  if (all(!is.na(d)) && sum(d) == 0) {
    P <- 1
    T0 <- 0
    df <- c - 1
    printresults <- function() {
      cat_sprintf("No differences between the marginal sums\n")
      cat_sprintf("P = 1.0")
    }
    return(
      contingencytables_result(
        list("Pvalue" = P, "T" = T0, "df" = df), printresults
      )
    )
  }

  # Form the null covariance matrix
  Sigmahat0 <- matrix(0, c - 1, c - 1)
  for (i in 1:(c - 1)) {
    Sigmahat0[i, i] <- nip[i] + npi[i] - 2 * n[i, i]
    for (j in 1:(c - 1)) {
      if (i != j) {
        Sigmahat0[i, j] <- -(n[i, j] + n[j, i])
      }
    }
  }

  # The Stuart test statistic
  T0 <- sum(d * solve(Sigmahat0, d))
  if (is.na(T0)) {
    P <- 1
    df <- c - 1
    printresults <- function() {
      cat_sprintf("The Stuart test statistic is not computable\n")
      cat_sprintf("P = 1.0")
    }
    return(
      contingencytables_result(
        list(P = P, T0 = T0, df = df), printresults
      )
    )
  }

  # Reference distribution: chi-squared with c-1 degrees of freedom
  df <- c - 1
  P <- 1 - pchisq(T0, df)

  printresults <- function() {
    cat_sprintf("The Stuart test for marginal homogenity: P = %8.6f, T0 = %6.3f (df=%g)", P, T0, df)
  }

  return(
    contingencytables_result(
      list(P = P, T0 = T0, df = df), printresults
    )
  )
}
