#' @title The Pearson residuals and the standardized Pearson residuals
#' @description The Pearson residuals and the standardized Pearson residuals
#' @description Described in Chapter 7 "The rxc Table"
#' @param n the observed counts (an rxc matrix)
#' @param printresults display results (FALSE = no, TRUE = yes)
#' @examples
#' # Treatment for ear infection (van Balen et al., 2003)
#' n <- matrix(c(40, 25, 54, 7, 63, 10), ncol = 2, byrow = TRUE)
#' Pearson_residuals_rxc(n)
#'
#' # Psychiatric diagnoses vs PA (Mangerud et al., 2004)
#' n <- matrix(
#'   c(62, 21, 97, 48, 10, 12, 30, 7, 132, 78, 34, 17),
#'   ncol = 2, byrow = TRUE
#' )
#' Pearson_residuals_rxc(n)
#'
#' # Psychiatric diag. vs BMI (Mangerud et al., 2004)
#' n <- rbind(
#'   c(3, 55, 23), c(8, 102, 36), c(6, 14, 1),
#'   c(5, 21, 12), c(19, 130, 64), c(7, 26, 18)
#' )
#' Pearson_residuals_rxc(n)
#' @export
#' @return A list containing matrices of the Pearson residuals and the standardized Pearson residuals
Pearson_residuals_rxc <- function(n, printresults = TRUE) {
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
