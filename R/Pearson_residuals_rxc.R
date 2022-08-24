#' @title The Pearson residuals and the standardized Pearson residuals
#' @description The Pearson residuals and the standardized Pearson residuals
#' @description Described in Chapter 7 "The rxc Table"
#' @param n the observed counts (an rxc matrix)
#' @param printresults display results (FALSE = no, TRUE = yes)
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
#' @return A list containing matrices of the Pearson residuals and the standardized Pearson residuals
Pearson_residuals_rxc <- function(n, printresults = TRUE) {
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

  if (printresults) {
    print(residuals)
    print(std_residuals)
  }

  return(list(residuals = residuals, std_residuals = std_residuals))
}

.print <- function(s, ...) {
  print(sprintf(gsub("\n", "", s), ...), quote = FALSE)
}
