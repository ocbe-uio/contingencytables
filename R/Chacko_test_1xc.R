#' @title The Chacko test for order-restriction
#' @description The Chacko test for order-restriction
#' @description Described in Chapter 3 "The 1xc Table and the Multinomial Distribution"
#' @param n the observed counts (a 1xc vector, where c is the number of categories)
#' @param printresults display results (F = no, T = yes)
#' @return A data frame containing the two-sided p-value, the statistic and the degrees of freedom
#' @examples
#' # Hypothetical experiment
#' Chacko_test_1xc(n=c(1, 4, 3, 11, 9))
#' @export
Chacko_test_1xc <- function(n, printresults=TRUE) {
	c0 <- length(n)
	N <- sum(n)

	# The ordering process
	nt <- n
	t0 <- rep(1, c0)
	m <- c0
	notordered <- 1
	while (notordered == 1) {
		for (i in 1:(m - 1)) {
			if (nt[i] > nt[i + 1]) {
				nt[i] <- (nt[i] + nt[i + 1]) / 2
				t0[i] <- t0[i] + 1
				m <- m - 1
				nt[(i + 1):m] <- nt[(i + 2):(m + 1)]
				break
			}
		}
		if (i ==  m - 1) {
			notordered = 0
		}
	}

	# The Chacko test statistic
	T0 <- 0
	for (i in 1:m) {
		T0 <- T0 + t0[i] * ((nt[i] - N / c0) ^ 2)
	}
	T0 <- T0 * c0 / N

	# The two-sided P-value (reference distribution: chi-squared with m-1
	# degrees of freedom)
	df <- m - 1
	P <- 1 - pchisq(T0, df)

	if (printresults) {
		print(
			sprintf(
				'The Chacko test: P = %7.5f, T = %5.3f (df = %i)', P, T0, df
			)
			, quote=FALSE
		)
	}

	res <- data.frame(P=P, T=T0, df=df)
	invisible(res)

}
