#' @title The Fleiss-Levin-Paik test for three-level ordinal outcomes
#' @description The Fleiss-Levin-Paik test for three-level ordinal outcomes
#' @description Described in Chapter 9 "The Paired cxc Table"
#' @param n the observed table (a cxc matrix)
#' @examples
#' # Pretherapy susceptability of pathogens *without the N / A category*
#' FleissLevinPaik_test_paired_cxc(peterson_2007[-4, -4])
#' @export
#' @return An object of the [contingencytables_result] class,
#' basically a subclass of [base::list()]. Use the [utils::str()] function
#' to see the specific elements returned.
FleissLevinPaik_test_paired_cxc <- function(n) {
  validateArguments(mget(ls()))
  c <- nrow(n)
  nip <- apply(n, 1, sum)
  npi <- apply(n, 2, sum)

  if (c != 3) {
    P <- NA
    T0 <- NA
    df <- NA
    stop("This method can only be used for c=3 categories")
  }

  # Compute the differences between the marginal sums
  d <- nip - npi
  if (sum(d^2) == 0) {
    P <- 1
    T0 <- 0
    df <- 1
    stop("No differences between the marginal sums\nP = 1.0")
  }

  n12 <- (n[1, 2] + n[2, 1]) / 2
  n13 <- (n[1, 3] + n[3, 1]) / 2
  n23 <- (n[2, 3] + n[3, 2]) / 2

  # The Fleiss-Levin-Paik test statistic
  T0 <- ((d[1] - d[3])^2) / (2 * (n12 + 4 * n13 + n23))

  # Reference distribution: chi-squared with 1 degree of freedom
  df <- 1
  P <- 1 - pchisq(T0, df)

  return(
    contingencytables_result(
      list("Pvalue" = P, "T" = T0, "df" = df),
      sprintf("The Fleiss-Levin-Paik test: P = %8.6f, T = %6.3f (df=%g)",
        P, T0, df
      )
    )
  )
}
