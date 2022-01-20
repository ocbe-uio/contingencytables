#' @title The Pearson chi-squared test for association in 2x2 tables
#' @description The Pearson chi-squared test for association in 2x2 tables
#' @description Described in Chapter 4 "The 2x2 Table"
#' @param n the observed counts (a 2x2 matrix)
#' @param printresults display results (FALSE = no, TRUE = yes)
#' @examples
#' # Example: A lady tasting a cup of tea
#' n <- rbind(c(3,1), c(1,3))
#' Pearson_chi_squared_test_2x2(n)
#'
#' # Example: Perondi et al. (2004)
#' n <- rbind(c(7,27), c(1,33))
#' Pearson_chi_squared_test_2x2(n)
#'
#' # Example: Lampasona et al. (2013)
#' n <- rbind(c(9,4), c(4,10))
#' Pearson_chi_squared_test_2x2(n)
#'
#' # Example: Ritland et al. (2007)
#' n <- rbind(c(0,16), c(15,57))
#' Pearson_chi_squared_test_2x2(n)
#'
#' @export
#' @return A data frame containing the two-sided p-value, the statistic and the degrees of freedom
Pearson_chi_squared_test_2x2 <- function(n, printresults=TRUE) {
	n1p <- n[1, 1] + n[1, 2]
	n2p <- n[2, 1] + n[2, 2]
	np1 <- n[1, 1] + n[2, 1]
	np2 <- n[1, 2] + n[2, 2]
	N <- sum(n)

	# The Pearson chi-squared statistic
	T0 <- (N * (n[1, 1] * n[2, 2] - n[1, 2] * n[2, 1]) ^ 2) / (n1p * n2p * np1 * np2)

	# The two-sided P-value (reference distribution: chi-squared with 1 degree of freedom)
	df <- 1
	P <- 1 - pchisq(T0, df)

	# Handle cases where the P-value is not computable
	if (is.na(P)) {
		P <- 1.0
	}

	if (printresults) {
		print(sprintf('The Pearson chi-squared test: P = %7.5f, T = %5.3f (df = %i)', P, T0, df), quote=FALSE)
	}

	res <- data.frame(p.value=P, statistic=T0, df=df)
	invisible(res)

}
