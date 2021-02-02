#' @title The Koopman asymptotic score confidence interval for the ratio of probabilities
#' @description The Koopman asymptotic score confidence interval for the ratio of probabilities
#' @note This versions uses the score test statistic of the Miettinen-Nurminen
#' interval without the variance correction term.
#' @description Described in Chapter 4 "The 2x2 Table"
#' @param n the observed counts (a 2x2 matrix)
#' @param alpha the nominal level, e.g. 0.05 for 95% CIs
#' @param printresults display results (FALSE = no, TRUE = yes)
#' @examples load_chapter(4)
#' # An RCT of high vs standard dose of epinephrine (Perondi et al., 2004):
#' n <- matrix(c(7,27,1,33), nrow=2, byrow=TRUE)
#' Koopman_asymptotic_score_CI_2x2(n)
#' # The association between CHRNA4 genotype and XFS (Ritland et al., 2007):
#' n <- matrix(c(0,16,15,57), nrow=2, byrow=TRUE)
#' Koopman_asymptotic_score_CI_2x2(n)
#' unload_chapter(4)
Koopman_asymptotic_score_CI_2x2 <- function(n, alpha=0.05, printresults=TRUE) {

	n11 <- n[1, 1]
	n21 <- n[2, 1]
	n1p <- n[1, 1] + n[1, 2]
	n2p <- n[2, 1] + n[2, 2]

	# Estimates of the two probabilities of success
	pi1hat <- n[1, 1] / n1p
	pi2hat <- n[2, 1] / n2p

	# Estimate of the ratio of probabilities (phihat)
	estimate <- pi1hat / pi2hat

	# Options for Matlab's fzero command
	tol <- 0.0000001
	phi0 <- 0.00001
	phi1 <- 100000

	# Lower CI limit
	# limit = 'lower'
	if (n[1, 1] == 0 && n[2, 1] == 0) {
		L <- 0
		# exitflag = 1
	} else if (is.na(estimate) || estimate==Inf) {
		L <- uniroot(
			calculate_limit_lower, c(phi0, phi1), n11=n11, n21=n21,
			n1p=n1p, n2p=n2p, pi1hat=pi1hat, pi2hat=pi2hat, alpha=alpha, tol=tol
		)$root
	} else if (estimate == 0) {
		L <- 0
		# exitflag = 1
	} else {
		L <- uniroot(
			calculate_limit_lower, c(phi0, estimate), n11=n11, n21=n21,
			n1p=n1p, n2p=n2p, pi1hat=pi1hat, pi2hat=pi2hat, alpha=alpha, tol=tol
		)$root
	}

	# Upper CI limit
	if (is.na(estimate) || estimate==Inf) {
		U <- Inf
	} else if (estimate == 0) {
		U <- uniroot(
			calculate_limit_upper, c(phi0, phi1), n11=n11, n21=n21,
			n1p=n1p, n2p=n2p, pi1hat=pi1hat, pi2hat=pi2hat, alpha=alpha, tol=tol
		)$root
	} else {
		U <- uniroot(
			calculate_limit_upper, c(estimate, phi1), n11=n11, n21=n21,
			n1p=n1p, n2p=n2p, pi1hat=pi1hat, pi2hat=pi2hat, alpha=alpha, tol=tol
		)$root
	}

	if (printresults) {
		print(sprintf('Mietinen-Nurminen asymptotic score CI: estimate = %6.4f (%g%% CI %6.4f to %6.4f)',
			estimate, 100 * (1 - alpha), L, U), quote=FALSE)
	}

	res <- data.frame(lower=L, upper=U, estimate=estimate)
	invisible(res)
}

# ================================
calculate_limit_lower <- function(phi0, n11, n21, n1p, n2p, pi1hat, pi2hat, alpha) {
	# global n11 n21 n1p n2p pi1hat pi2hat alphaglobal limit
	ml.res = ML_estimates(n11, n21, n1p, n2p, phi0)
	T0 <- score_test_statistic(
		pi1hat, pi2hat, ml.res$p1hat, ml.res$p2hat, n1p, n2p, phi0
	)
	if (is.na(T0)) {
		T0 <- 0
	}
	f <- T0 - qnorm(1-alpha / 2, 0, 1)
	return(f)
}

# ================================
calculate_limit_upper <- function(phi0, n11, n21, n1p, n2p, pi1hat, pi2hat, alpha) {
	# global n11 n21 n1p n2p pi1hat pi2hat alphaglobal limit
	ml.res = ML_estimates(n11, n21, n1p, n2p, phi0)
	T0 <- score_test_statistic(
		pi1hat, pi2hat, ml.res$p1hat, ml.res$p2hat, n1p, n2p, phi0
	)
	if (is.na(T0)) {
		T0 <- 0
	}
	f <- T0 + qnorm(1-alpha / 2, 0, 1)
	return(f)
}


# ==============================================================
ML_estimates <- function(n11, n21, n1p, n2p, phi0) {
	A0 <- (n1p + n2p) * phi0
	B0 <- -(n1p * phi0 + n11 + n2p + n21 * phi0)
	C0 <- n11 + n21
	p2hat <- (-B0 - sqrt(B0 * B0 - 4 * A0 * C0)) / (2 * A0)
	p1hat <- p2hat * phi0
	res <- data.frame(p1hat=p1hat, p2hat=p2hat)
	return(res)
}

# ============================================================================
score_test_statistic <- function(pi1hat, pi2hat, p1hat, p2hat, n1p, n2p, phi0) {
	T0 <- (pi1hat - phi0 * pi2hat) / sqrt(p1hat * (1 - p1hat) / n1p + (phi0 ^ 2) * p2hat * (1 - p2hat) / n2p)
	return(T0)
}

