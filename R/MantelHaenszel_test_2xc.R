#' @title The Mantel-Haenszel test of association with column scores
#' @description The Mantel-Haenszel test of association with column scores
#' @description Described in Chapter 6 "The Ordered 2xc Table"
#' @param n the observed counts (a 2xc matrix)
#' @param b scores assigned to the columns (if b=0, midranks will be used as scores)
#' @param printresults display results (0 = no, 1 = yes)
#' @examples load_chapter(6)
#' # Postoperative nausea (Lydersen et al., 2012a)
#' n <- rbind(c(14, 10, 3, 2), c(11, 7, 8, 4))
#' MantelHaenszel_test_2xc(n)
#' b <- 0
#' unload_chapter(6)
MantelHaenszel_test_2xc <- function(n, b = 0, printresults = TRUE) {
	c <- ncol(n)
	nip <- apply(n, 1, sum)
	npj <- apply(n, 2, sum)
	N <- sum(n)

	if (b == 0) {
		b <- rep(0, c)
		for (j in 1:c) {
			a0 <- ifelse(j > 1, sum(npj[1:j - 1]), 0)
			b0 <- 1 + sum(npj[1:j])
			b[j] <- 0.5 * (a0 + b0)
		}
	}

	# The Mantel-Haenszel test statistic
	b1mean <- sum(b * n[1, ] / nip[1])
	b1exp <- sum(b * npj / N)
	b1var <- ((N - nip[1]) / (nip[1] * (N - 1))) * sum(((b - b1exp)^2) * npj) / N
	T0 <- ((b1mean - b1exp)^2) / b1var

	# The two-sided P-value (reference distribution: chi-squared with one degree
	# of freedom)
	df <- 1
	P <- 1 - pchisq(T0, df)

	if (printresults) {
		print(
			sprintf(
				"Mantel-Haenszel test of association: P = %6.4f, T = %5.3f (df=%g)",
				P, T0, df
			),
			quote = FALSE
		)
	}

	invisible(data.frame(P = P, T = T0, df = df))
}