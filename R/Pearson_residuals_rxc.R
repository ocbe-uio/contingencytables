#' @title The Pearson residuals and the standardized Pearson residuals
#' @description The Pearson residuals and the standardized Pearson residuals
#' @description Described in Chapter 7 "The rxc Table"
#' @param n the observed counts (an rxc matrix)
#' @examples
#' ## Treatment for ear infection (van Balen et al., 2003):
#' Pearson_residuals_rxc(table_7.3)
#'
#' ## Psychiatric diagnoses vs PA (Mangerud et al., 2004):
#' Pearson_residuals_rxc(table_7.4)
#'
#' ## Psychiatric diag. vs BMI (Mangerud et al., 2004):
#' Pearson_residuals_rxc(table_7.5)
#' @export
#' @return An object of the [contingencytables_result] class,
#' basically a subclass of [base::list()]. Use the [utils::str()] function
#' to see the specific elements returned.
Pearson_residuals_rxc <- function(n) {
  validateArguments(mget(ls()))

  r <- nrow(n)
  c <- ncol(n)
  nip <- apply(n, 1, sum)
  npj <- apply(n, 2, sum)
  N <- sum(n)

  # Calculate the expected cell counts
  m <- matrix(0, r, c)
  residuals <- matrix(0, r, c) # the Pearson residuals
  std_residuals <- matrix(0, r, c) # the standardized Pearson residuals
  for (i in 1:r) {
    for (j in 1:c) {
      m[i, j] <- nip[i] * npj[j] / N
      residuals[i, j] <- (n[i, j] - m[i, j]) / sqrt(m[i, j])
      std_residuals[i, j] <- (n[i, j] - m[i, j]) / sqrt(m[i, j] * (1 - nip[i] / N) * (1 - npj[j] / N))
    }
  }

  res <- list("residuals" = residuals, "std_residuals" = std_residuals)

  printresults <- function() {
    my_sprintf_cat("Pearson residuals:\n")
    print(res$residuals)
    my_sprintf_cat("\nStandardized Pearson residuals:\n")
    print(res$std_residuals)
    return(NULL)
  }

  return(contingencytables_result(res, printresults))
}
