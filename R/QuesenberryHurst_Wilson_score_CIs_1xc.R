#' @title The Quesenberry-Hurst Wilson score simultaneous intervals for the multinomial probabilities
#' @description The Quesenberry-Hurst Wilson score simultaneous intervals for the multinomial probabilities
#' @description (with Scheffe adjustment)
#' @description Described in Chapter 3 "The 1xc Table and the Multinomial Distribution"
#' @param n the observed counts (a 1xc vector, where c is the number of categories)
#' @param alpha the nominal level, e.g. 0.05 for 95% CIs
#' @param printresults display results (F = no, T = yes)
#' @examples load_chapter(3)
#' # Genotype counts for SNP rs 6498169 in RA patients
#' QuesenberryHurst_Wilson_score_CIs_1xc(n=c(276, 380, 118))
#' unload_chapter(3)
QuesenberryHurst_Wilson_score_CIs_1xc <- function(n, alpha=0.05, printresults=TRUE) {

	c0 <- length(n)
	N <- sum(n)

	# Estimates of the multinomial probabilities
	pihat <- n / N

	# Simultaneous confidence intervals with Scheffe adjustment
	L <- rep(0, c0)
	U <- rep(0, c0)
	Scheffe <- qchisq(1 - alpha, c0 - 1)
	for (i in 1:c0) {
		L[i] <- (
			Scheffe + 2 * N * pihat[i] - sqrt(
				Scheffe ^ 2 + 4 * N * Scheffe * pihat[i] * (1 - pihat[i])
			)
		) / (2 * Scheffe + 2 * N)
		U[i] <- (
			Scheffe + 2 * N * pihat[i] + sqrt(
				Scheffe ^ 2 + 4 * N * Scheffe * pihat[i] * (1 - pihat[i])
			)
		) / (2 * Scheffe + 2 * N)
	}

	if (printresults) {
		print(
			sprintf(
				'The Quesenberry-Hurst Wilson score simultaneous intervals'
			),
			quote=FALSE
		)
		for (i in 1:c0) {
			print(
				sprintf(
					'  pi_%i: estimate = %6.4f (%6.4f to %6.4f)',
					i, pihat[i], L[i], U[i]
				),
				quote=FALSE
			)
		}
	}

	res <- data.frame(lower=L, upper=U, estimate=pihat)
	invisible(res)

}
