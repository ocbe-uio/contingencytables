#' @title The Cochran-Armitage confidence interval for trend in the linear model
#' @description The Cochran-Armitage confidence interval for trend in the linear model
#' @description Described in Chapter 5 "The Ordered rx2 Table"
#' @param n the observed counts (an rx2 matrix)
#' @param a scores assigned to the rows
#' @param alpha the nominal level, e.g. 0.05 for 95% CIs
#' @param printresults display results
#' @return A vector containing lower, upper and point estimates of the statistic
#' @examples
#' # Alcohol consumption and malformations (Mills and Graubard, 1987)
#' n <- rbind(c(48,17066),c(38,14464),c(5,788),c(1,126),c(1,37))
#' a <- c(1, 2, 3, 4, 5)
#' CochranArmitage_CI_rx2(n, a)
#'
#' # Elevated troponin T levels in stroke patients (Indredavik et al., 2008)
#' n <- rbind(c(8,53),c(10,48),c(11,100),c(22,102),c(6,129))
#' a <- c(1, 2, 3, 4, 5)
#' CochranArmitage_CI_rx2(n, a)
#'
#' @export
CochranArmitage_CI_rx2 <- function(n, a, alpha=0.05, printresults=TRUE) {
	r <- nrow(n)
	nip <- apply(n, 1, sum)
	N <- sum(n)
	abar <- sum(nip * a) / N
	pihat <- n[,1] / nip

	U <- 0
	Saa <- 0
	s2 <- 0
	for (i in 1:r) {
		U <- U + nip[i] * (a[i] - abar) * pihat[i]
		Saa <- Saa + nip[i] * (a[i] - abar) ^ 2
		s2 <- s2 + nip[i] * ((a[i] - abar) ^ 2) * pihat[i] * (1 - pihat[i])
	}

	# Estimate of the linear slope
	betahat <- U / Saa

	# General estimate of the standard error
	SE <- sqrt(s2) / Saa

	# The Cochran-Armitage confidence interval
	z <- qnorm(1-alpha / 2, 0, 1)
	L <- betahat - z * SE
	U <- betahat + z * SE

	if (printresults) {
		print(sprintf('Trend estimate and Cochran-Armitage CI:  betahat = %6.4f (%g%% CI %6.4f to %6.4f)',
			betahat, 100 * (1 - alpha), L, U), quote=FALSE)
	}

	res <- data.frame(lower=L, upper=U, estimate=betahat)

}
