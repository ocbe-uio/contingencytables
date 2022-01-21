#' @title The mid-P multinomial test for multinomial probabilities
#' @description The mid-P multinomial test for multinomial probabilities
#' @description Described in Chapter 3 "The 1xc Table and the Multinomial Distribution"
#' @param n the observed counts (a 1xc vector, where c is the number of categories)
#' @param pi0 given probabilities (a 1xc vector)
#' @param printresults display results (F = no, T = yes)
#' @examples
#' # Genotype counts for SNP rs 6498169 in RA patients
#' \dontrun{
#' MidP_multinomial_test_1xc(n=c(276, 380, 118), pi0=c(0.402, 0.479, 0.119))
#' }
#' # subset of 10 patients
#' MidP_multinomial_test_1xc(n=c(6, 1, 3), pi0=c(0.402, 0.479, 0.119))
#' @export
#' @return probability value
MidP_multinomial_test_1xc <- function(n, pi0, printresults=TRUE) {

	c0 <- length(n)
	N <- sum(n)

	# Identify all possible tables with N observations (with 3,4,...,7 categories)
	if (c0 == 3) {
		x <- all.tables3(N)
	} else if (c0 == 4) {
		x <- all.tables4(N)
	} else if (c0 == 5) {
		x <- all.tables5(N)
	} else if (c0 == 6) {
		x <- all.tables6(N)
	} else if (c0 == 7) {
		x <- all.tables7(N)
	}

	P <- 0
	Tobs <- sum(((n - N * pi0) ^ 2) / (N * pi0))
	for (i in 1:nrow(x)) {
		T0 <- sum(((x[i,] - N * pi0) ^ 2) / (N * pi0)) # Pearson chi-squared
		if (T0 > Tobs) {
			P <- P + dmultinom(x[i,], prob=pi0)
		} else if (T0 == Tobs) {
			P <- P + 0.5 * dmultinom(x[i,], prob=pi0)
		}
	}

	if (printresults) {
		print(sprintf('The mid-P multinomial test: P = %7.5f', P), quote=FALSE)
	}

	invisible(P)

}


# =========================
all.tables3 <- function(N) {
	x <- c()
	for (x1 in 0:N) {
		for (x2 in 0:(N-x1)) {
			x <- rbind(x, c(x1, x2, N-x1-x2))
		}
	}
	return(x)
}

# =========================
all.tables4 <- function(N) {
	x <- c()
	for (x1 in (0:N)) {
		for (x2 in 0:(N-x1)) {
			for (x3 in 0:(N-x1-x2)) {
				x <- rbind(x, c(x1, x2, x3, N-x1-x2-x3))
			}
		}
	}
	return(x)
}

# =========================
all.tables5 <- function(N) {
	x <- c()
	for (x1 in 0:N) {
		for (x2 in 0:(N-x1)) {
			for (x3 in 0:(N-x1-x2)) {
				for (x4 in 0:(N-x1-x2-x3)) {
					x <- rbind(x, c(x1, x2, x3, x4, N-x1-x2-x3-x4))
				}
			}
		}
	}
}

# =========================
all.tables6 <- function(N) {
	x <- c()
	for (x1 in 0:N) {
		for (x2 in 0:(N-x1)) {
			for (x3 in 0:(N-x1-x2)) {
				for (x4 in 0:(N-x1-x2-x3)) {
					for (x5 in 0:(N-x1-x2-x3-x4)) {
					x <- rbind(x, c(x1, x2, x3, x4, x5, N-x1-x2-x3-x4-x5))
					}
				}
			}
		}
	}
}

# =========================
all.tables7 <- function(N) {
	x <- c()
	for (x1 in 0:N) {
		for (x2 in 0:(N-x1)) {
			for (x3 in 0:(N-x1-x2)) {
				for (x4 in 0:(N-x1-x2-x3)) {
					for (x5 in 0:(N-x1-x2-x3-x4)) {
						for (x6 in 0:(N-x1-x2-x3-x4-x5)) {
							x <- rbind(
								x,
								c(x1, x2, x3, x4, x5, x6, N-x1-x2-x3-x4-x5-x6)
							)
						}
					}
				}
			}
		}
	}
}
