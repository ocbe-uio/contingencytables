#' @title The gamma coefficient
#' @description The gamma coefficient
#' @description Described in Chapter 7 "The rxc Table"
#' @param n the observed table (an rxc matrix)
#' @examples
#' gamma_coefficient_rxc(table_7.7)
#' gamma_coefficient_rxc(table_7.8)
#' gamma_coefficient_rxc(table_7.9)
#' @export
#' @return An object of the [contingencytables_result] class,
#' basically a subclass of [base::list()]. Use the [utils::str()] function
#' to see the specific elements returned.
gamma_coefficient_rxc <- function(n) {
  validateArguments(mget(ls()))
  r <- nrow(n)
  c <- ncol(n)

  C <- 0
  for (i in 1:(r - 1)) {
    for (j in 1:(c - 1)) {
      for (k in (i + 1):r) {
        for (l in (j + 1):c) {
          C <- C + n[i, j] * n[k, l]
        }
      }
    }
  }

  D <- 0
  for (i in 1:(r - 1)) {
    for (l in 1:(c - 1)) {
      for (k in (i + 1):r) {
        for (j in (l + 1):c) {
          D <- D + n[i, j] * n[k, l]
        }
      }
    }
  }

  # The gamma coefficient
  gamma <- (C - D) / (C + D)

  prntrs <- function() {
    my_sprintf_cat("The number of concordant pairs:      %g\n", C)
    my_sprintf_cat("The number of discordant pairs:      %g\n", D)
    my_sprintf_cat("The proportion of concordant pairs:  %g\n", C / (C + D))
    my_sprintf_cat("The proportion of discordant pairs:  %g\n", D / (C + D))
    my_sprintf_cat("The gamma coefficient:               %6.4f\n", gamma)
  }

  return(contingencytables_result(list(gamma = gamma, C = C, D = D), prntrs))
}
