#' @title The Fisher mid-P test for association in 2x2 tables
#' @description The Fisher mid-P test for association in 2x2 tables
#' @description Described in Chapter 4 "The 2x2 Table"
#' @param n the observed counts (a 2x2 matrix)
#' @param printresults display results (F = no, T = yes)
#' @param statistic 'hypergeometric' (i.e. Fisher-Irwin default), 'Pearson', or 'LR' (likelihood ratio)
#' @examples load_chapter(4)
#' n <- rbind(c(3,1), c(1,3))       # Example: A lady tasting a cup of tea
#' Fisher_midP_test_2x2(n)
#' n <- rbind(c(7,27), c(1,33))   # Example: Perondi et al. (2004)
#' Fisher_midP_test_2x2(n)
#' n <- rbind(c(9,4), c(4,10))    # Example: Lampasona et al. (2013)
#' Fisher_midP_test_2x2(n)
#' n <- rbind(c(0,16), c(15,57))  # Example: Ritland et al. (2007)
#' Fisher_midP_test_2x2(n)
#' unload_chapter(4)
Fisher_midP_test_2x2 <- function(n, statistic='hypergeometric', printresults=TRUE) {

	n1p <- n[1, 1] + n[1, 2]
	n2p <- n[2, 1] + n[2, 2]
	np1 <- n[1, 1] + n[2, 1]
	N <- sum(sum(n))

	# The possible tables, expressed by their x_11 counts
	x11values <- max(c(0, np1 - n2p)):min(c(n1p, np1))

	# The probabilities of the possible tables (from the hypergeometric distribution)
	fvalues <- dhyper(x11values, np1, N - np1, n1p)

	# Values of the test statistic for all possible tables
	Tvalues <- test_statistic_fisher_midp_test_2x2(
		x11values, n1p - x11values, np1 - x11values, n2p - (np1 - x11values),
		statistic
	)

	# Observed value of the test statistic
	if (n[1, 1] < length(Tvalues)) {
		Tobs <- Tvalues[n[1, 1] + 1]
	} else {
		Tobs <- -Inf # If, for instance, n[1, 2] = 0 and n[2, 2] = 0
	}

	# Two-sided P-value
	P <- sum(fvalues[Tvalues > Tobs]) + 0.5 * sum(fvalues[Tvalues == Tobs])

	if (printresults) {
		if (statistic == 'hypergeometric') {
			print(
				sprintf('The Fisher mid-P test (Fisher-Irwin): P = %7.5f', P),
				quote=FALSE
			)
		} else if (statistic == 'Pearson') {
			print(
				sprintf('The Fisher mid-P test (Pearson): P = %7.5f', P),
				quote=FALSE
			)
		} else if (statistic == 'LR') {
			print(
				sprintf('The Fisher mid-P test (LR): P = %7.5f', P),
				quote=FALSE
			)
		}
	}

	invisible(P)

}

# ========================================================
test_statistic_fisher_midp_test_2x2 <- function(x11, x12, x21, x22, statistic) {
	N <- x11 + x12 + x21 + x22

	if (statistic == 'hypergeometric') {
		# The hypergeomtric distribution distribution
		# Small values agree less with H0 than large values => "-" sign
		T0 <- -dhyper(x11, x11 + x21, N - x11 - x21, x11 + x12)
	} else if (statistic == 'Pearson') {
		# Pearson's chi-squared statistic
		T0 <- (N * (x11 * x22 - x12 * x21) ^ 2) / ((x11 + x12) * (x21 + x22) * (x11 + x21) * (x12 + x22))
	} else if (statistic == 'LR') {
		# The likelihood ratio statistic
		n1p <- x11 + x12
		n2p <- x21 + x22
		np1 <- x11 + x21
		np2 <- x12 + x22
		# Make sure 0 is added to T for terms where x_ij = 0 (log(1) = 0)
		x11[x11 == 0] = 1
		x12[x12 == 0] = 1
		x21[x21 == 0] = 1
		x22[x22 == 0] = 1
		T0 <- 0
		T0 <- T0 + x11 * log(x11 / (n1p * np1 / N))
		T0 <- T0 + x12 * log(x12 / (n1p * np2 / N))
		T0 <- T0 + x21 * log(x21 / (n2p * np1 / N))
		T0 <- T0 + x22 * log(x22 / (n2p * np2 / N))
		T0 <- 2 * T0
	}

	return(T0)
}

