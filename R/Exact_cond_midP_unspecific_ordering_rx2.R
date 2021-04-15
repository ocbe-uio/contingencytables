#' @title The exact conditional and mid-P tests for unspecific ordering
#' @description The exact conditional and mid-P tests for unspecific ordering
#' @description Described in Chapter 5 "The Ordered rx2 Table"
#' @note May also be used for 2xc tables, after flipping rows and columns
#' (i.e. if n is a 2xc table, call this function with n' (the transpose of n)
#' as the first argument).
#' @param n the observed counts (an rx2 matrix)
#' @param direction the direction of the success probabilities ("increasing" or
#' "decreasing")
#' @param statistic the Pearson test statistic ("Pearson") or the likelihood
#' ratio test statistic ("LR"). Can also be used for cumulative ORs in
#' 2xc tables with "PearsonCumOR" or "LRCumOR".
#' @param printresults display results
#' @examples load_chapter(5)
#' # Chapter 5: Alcohol consumption and malformations (Mills and Graubard, 1987)
#' n <- matrix(c(48,17066,38,14464,5,788,1,126,1,37), byrow=TRUE, ncol=2)
#' direction <- 'increasing'
#' statistic <- 'Pearson'
#' Exact_cond_midP_unspecific_ordering_rx2(n, direction, statistic)
#'
#' # Chapter 5: Elevated troponin T levels in stroke patients (Indredavik et al., 2008)
#' n <- matrix(c(8, 53, 10, 48, 11, 100, 22, 102, 6, 129), byrow=TRUE, ncol=2)
#' direction <- 'decreasing'
#' statistic <- 'Pearson'
#' Exact_cond_midP_unspecific_ordering_rx2(n, direction, statistic)
#'
#' # Chapter 6: Postoperative nausea (Lydersen et al., 2012a)
#' n <- matrix(c(14, 10, 3, 2, 11, 7, 8, 4), byrow=FALSE, nrow=4)
#' direction <- 'decreasing'
#' statistic <- 'PearsonCumOR'
#' Exact_cond_midP_unspecific_ordering_rx2(n, direction, statistic)
#'
#' unload_chapter(5)

Exact_cond_midP_unspecific_ordering_rx2 <- function(n, direction, statistic="Pearson", printresults=TRUE) {
	r <- nrow(n)
	nip <- apply(n, 1, sum)
	npj <- apply(n, 2, sum)
	N <- sum(n)
	np1 <- sum(n[,1])

	# Calculate all nchoosek beforehand
	nip_choose_xi1 <- matrix(0, r, max(nip) + 1)  #### Added +1 at the end
	for (i in 1:r) {
		for (xi1 in 0:nip[i]) {
			nip_choose_xi1[i, xi1 + 1] = choose(nip[i], xi1)
		}
	}
	N_choose_np1 <- choose(N, np1)

	# The observed value of the test statistic
	Tobs <- test.statistic(n, r, nip, npj, N, direction, statistic)

	# Calculate the two-sided exact P-value and the mid-P value
	# Need separate functions for different values of r (the number of rows)
	if (r == 4) {
		res <- calc_Pvalue_4x2(Tobs, nip, np1, npj, N, N_choose_np1, nip_choose_xi1, direction, statistic)
		P <- res$P
		midP <- res$midP
	} else if (r == 5) {
		res <- calc_Pvalue_5x2(Tobs, nip, np1, npj, N, N_choose_np1, nip_choose_xi1, direction, statistic)
		P <- res$P
		midP <- res$midP
	}

	if (printresults) {
		print(sprintf('Exact conditional test: %8.5f', P))
		print(sprintf('Mid-P test:             %8.5f', midP))
	}

	res <- data.frame(P=P, midP=midP)
	invisible(res)
}

# Calculate the test statistics

test.statistic <- function(n, r, nip, npj, N, direction, statistic) {
	# These are used for cumulative odds ratios in 2xc tables
	if (statistic=='PearsonCumOR' || statistic=='LRCumOR') {
		n <- t(n)
		n[c(1,2),] = n[c(2,1),]
		T0 <- test.statistic.cum.OR(n, r, nip, statistic)
		return(T0)
	}

	# Common calculations for the Pearson and LR statistics
	nhat <- n[,1] / apply(n,1,sum)
	nhatstar <- nhat
	for (i in 1:(r-1)) {
		if ((direction=='increasing' && nhatstar[i] > nhatstar[i + 1]) ||
	(direction=='decreasing' && nhatstar[i] < nhatstar[i + 1])) {
			pooled_proportion <- (n[i,1] + n[i + 1,1]) / (n[i,1] + n[i,2] + n[i + 1,1] + n[i + 1,2])
			nhatstar[i] = pooled_proportion
			nhatstar[i + 1] = pooled_proportion
		}
	}
	nstar <- matrix(0, r, 2)
	nstar[,1] = apply(n,1,sum) * nhatstar
	nstar[,2] = apply(n,1,sum) * (1 - nhatstar)

	m <- matrix(0, r, 2)
	T0 <- 0
	if (statistic=='Pearson') {
		for (i in 1:r) {
			for (j in 1:2) {
	m[i,j] = nip[i] * npj[j] / N
	if (m[i,j] > 0) {
		T0 <- T0 + ((nstar[i,j] - m[i,j]) ^ 2) / m[i,j]
	}
			}
		}
	} else if (statistic=='LR') {
		for (i in 1:r) {
			for (j in 1:2) {
	m[i,j] = nip[i] * npj[j] / N
	if (m[i,j] > 0 && nstar[i,j] != 0) {
		T0 <- T0 + nstar[i,j] * log(nstar[i,j] / m[i,j])
	}
			}
		}
		T0 <- 2 * T0
	}
	return(T0)
}

# Slightly different calculations are needed for cumulative odds ratios in
# 2xc tables
test.statistic.cum.OR <- function(n, c0, npj, statistic) {
	r <- matrix(0, 1, c0)
	for (j in 1:c0) {
		r[j] = sum(n[1,1:j]) / sum(n[2,1:j])
	}

	J <- list(); nJ = 0
	index1 <- 1
	while (index1 < c0 + 1) {
		v <- which(r[index1:length(r)]==min(r[index1:length(r)]))[1]
		nJ <- nJ + 1
		J[[nJ]] = n[,index1:(index1 + v-1),drop=F]
		index1 <- index1 + v
	}

	m <- array(0, c(length(J), 2, c0))
	T0 <- 0
	if (statistic=='PearsonCumOR') {
		for (h in 1:length(J)) {
			nJ <- J[[h]]
			cols <- ncol(nJ)
			for (i in 1:2) {
	for (j in 1:cols) {
		m[h,i,j] = npj[j] * sum(nJ[i,]) / sum(nJ)
		if (m[h,i,j] > 0) {
			T0 <- T0 + ((n[i,j] - m[h,i,j]) ^ 2) / m[h,i,j]
		}
	}
			}
		}
	} else if (statistic=='LRCumOR') {
		for (h in 1:length(J)) {
			nJ <- J[[h]]
			cols <- ncol(nJ)
			for (i in 1:2) {
	for (j in 1:cols) {
		m[h,i,j] = npj[j] * sum(nJ[i,]) / sum(nJ)
		if (m[h,i,j] > 0 && n[i,j] > 0) {
			T0 <- T0 + n[i,j] * log(n[i,j] / m[h,i,j])
		}
	}
			}
		}
		T0 <- 2 * T0
	}
	return(T0)
}
