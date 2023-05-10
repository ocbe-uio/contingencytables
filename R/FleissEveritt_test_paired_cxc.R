#' @title The Fleiss-Everitt version of the Stuart test for marginal homogeneity
#' @description The Fleiss-Everitt version of the Stuart test for marginal homogeneity
#' @description Described in Chapter 9 "The Paired cxc Table"
#' @param n the observed table (a cxc matrix)
#' @examples
#' FleissEveritt_test_paired_cxc(fleiss_2003)
#' @export
#' @return An object of the [contingencytables_result] class,
#' basically a subclass of [base::list()]. Use the [utils::str()] function
#' to see the specific elements returned.
FleissEveritt_test_paired_cxc <- function(n) {
  validateArguments(mget(ls()))
  # This is the version with c=3 categories
  c <- nrow(n)
  nip <- apply(n, 1, sum)
  npi <- apply(n, 2, sum)

  if (c != 3) {
    P <- NA
    T <- NA
    df <- NA
    stop("This method can only be used for c=3 categories")
  }

  # Compute the differences between the marginal sums
  d <- nip - npi
  if (sum(d^2) == 0) {
    P <- 1
    T0 <- 0
    df <- c - 1
    stop("No differences between the marginal sums\nP = 1.0")
  }

  n23 <- (n[2, 3] + n[3, 2]) / 2
  n13 <- (n[1, 3] + n[3, 1]) / 2
  n12 <- (n[1, 2] + n[2, 1]) / 2

  # The Fleiss-Everitt test statistic
  T0 <- (n23 * d[1]^2 + n13 * d[2]^2 + n12 * d[3]^2) / (2 * (n12 * n23 + n12 * n13 + n13 * n23))

  # Reference distribution: chi-squared with 2 degrees of freedom
  df <- 2
  P <- 1 - pchisq(T0, df)

  return(
    contingencytables_result(
      list(P = P, T = T0, df = df),
      sprintf(
        "The Fleiss-Everitt version of the Stuart test: P = %8.6f, T = %6.3f (df=%g)",
        P, T0, df
      )
    )
  )
}
