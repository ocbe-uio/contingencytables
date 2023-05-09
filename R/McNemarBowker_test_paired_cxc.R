#' @title The McNemar-Bowker test for marginal symmetry
#' @description The McNemar-Bowker test for marginal symmetry
#' @description Described in Chapter 9 "The Paired cxc Table"
#' @param n the observed table (a cxc matrix)
#' @examples
#' # Pretherapy susceptability of pathogens (Peterson et al., 2007)
#' McNemarBowker_test_paired_cxc(peterson_2007)
#' @export
#' @return An object of the [contingencytables_result] class,
#' basically a subclass of [base::list()]. Use the [utils::str()] function
#' to see the specific elements returned.
McNemarBowker_test_paired_cxc <- function(n) {
  validateArguments(mget(ls()))

  c <- nrow(n)

  # The McNemar-Bowker test statistic
  T0 <- 0
  for (i in 2:c) {
    for (j in 1:(i - 1)) {
      if (n[i, j] + n[j, i] > 0) {
        T0 <- T0 + ((n[i, j] - n[j, i])^2) / (n[i, j] + n[j, i])
      }
    }
  }

  # Reference distribution: chi-squared with c * (c-1) / 2 degrees of freedom
  df <- c * (c - 1) / 2
  P <- 1 - pchisq(T0, df)

  return(
    contingencytables_result(
      list(P = P, T = T0, df = df),
      sprintf("The McNemar-Bowker test for symmetry: P = %8.6f, T0 = %6.3f (df=%g)", P, T0, df)
    )
  )
}
