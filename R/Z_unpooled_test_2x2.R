#' @title The Z-unpooled test for association in 2x2 tables
#' @description The Z-unpooled test for association in 2x2 tables
#' @description Described in Chapter 4 "The 2x2 Table"
#' @param n the observed counts (a 2x2 matrix)
#' @param printresults display results (FALSE = no, TRUE = yes)
#' @examples
#' # Example: A lady tasting a cup of tea
#' n <- rbind(c(3,1), c(1,3))
#' Z_unpooled_test_2x2(n)
#'
#' # Example: Perondi et al. (2004)
#' n <- rbind(c(7,27), c(1,33))
#' Z_unpooled_test_2x2(n)
#'
#' # Example: Lampasona et al. (2013)
#' n <- rbind(c(9,4), c(4,10))
#' Z_unpooled_test_2x2(n)
#'
#' # Example: Ritland et al. (2007)
#' n <- rbind(c(0,16), c(15,57))
#' Z_unpooled_test_2x2(n)
#'
#' @export
#' @return A vector containing the two-sided p-value and the unpooled Z statistic
Z_unpooled_test_2x2 <- function(n, printresults=TRUE) {
	n1p <- n[1, 1] + n[1, 2]
	n2p <- n[2, 1] + n[2, 2]

	# The unpooled Z statistic
	Z <- (n[1, 1] / n1p - n[2, 1] / n2p) / sqrt(n[1, 1] * n[1, 2] / n1p ^ 3 + n[2, 1] * n[2, 2] / n2p ^ 3)

	# The two-sided P-value (reference distribution: standard normal)
	P <- 2 * (1 - pnorm(abs(Z), 0, 1))

	# Handle cases where the P-value is not computable
	if (is.na(P)) {
		P <- 1.0
	}

	if (printresults) {
		print(sprintf('The Z-unpooled test: P = %7.5f, Z = %6.3f', P, Z), quote=FALSE)
	}

	res <- data.frame(p.value=P, statistic=Z)
	invisible(res)
}
