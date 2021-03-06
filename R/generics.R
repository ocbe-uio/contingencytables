# ======================================================== #
# Generic functions                                        #
# ======================================================== #

calculate_limit_lower <- function(...) {
	method <- convertFunName2Method()
	UseMethod("calculate_limit_lower", method)
}

calculate_limit_upper <- function(...) {
	method <- convertFunName2Method()
	UseMethod("calculate_limit_upper", method)
}

ML_estimates <- function(...) {
	method <- convertFunName2Method()
	UseMethod("ML_estimates", method)
}

score_test_statistic <- function(...) {
	method <- convertFunName2Method()
	UseMethod("score_test_statistic", method)
}

calc_prob <- function(...) {
	method <- convertFunName2Method()
	UseMethod("calc_prob", method)
}

calc_Pvalue_4x2 <- function(...) {
	method <- convertFunName2Method()
	UseMethod("calc_Pvalue_4x2", method)
}

calc_Pvalue_5x2 <- function(...) {
	method <- convertFunName2Method()
	UseMethod("calc_Pvalue_5x2", method)
}

linear_rank_test_statistic <- function(...) {
	method <- convertFunName2Method()
	UseMethod("linear_rank_test_statistic", method)
}

#' @author Waldir Leoncio
convertFunName2Method <- function() {
	callstack <- as.list(sys.calls())
	findInCallstack <- function(regex) {
		length_function_name <- length(callstack[grepl(regex, callstack)])
		return(length_function_name > 0)
	}
	if (findInCallstack("^Koopman_asymptotic_score_CI")) {
		cls <- "Koopman"
	} else if (findInCallstack("^Mee_asymptotic_score_CI")) {
		cls <- "Mee"
	} else if (findInCallstack("^MiettinenNurminen_asymptotic_score_CI_diff")) {
		cls <- "Miettinen_diff"
	} else if (findInCallstack("^MiettinenNurminen_asymptotic_score_CI_OR")) {
		cls <- "Miettinen_OR"
	} else if (findInCallstack("^MiettinenNurminen_asymptotic_score_CI_rat")) {
		cls <- "Miettinen_ratio"
	} else if (findInCallstack("^Uncorrected_asymptotic_score_CI_2x2")) {
		cls <- "Uncorrected"
	} else if (findInCallstack("^CochranArmitage")) {
		cls <- "CochranArmitage"
	} else if (findInCallstack("^Exact_cond_midP_unspecific_ordering_rx2")) {
		cls <- "ExactCond"
	} else {
		stop("Unrecognized parent function")
	}
	fun_name <- cls
	class(fun_name) <- cls
	return(fun_name)
}

# ======================================================== #
# Methods for Mee                                          #
# ======================================================== #

calculate_limit_lower.Mee <- function(delta0, n11, n21, n1p, n2p, pi1hat,
pi2hat, alpha) {
	ml.res = ML_estimates(n11, n21, n1p, n2p, delta0)
	T0 <- score_test_statistic(
		pi1hat, pi2hat, delta0, ml.res$p1hat, ml.res$p2hat, n1p, n2p
	)
	if (is.na(T0)) {
		T0 <- 0
	}
	z <- qnorm(1-alpha / 2, 0, 1)
	f <- T0 - z
	return(f)
}

calculate_limit_upper.Mee <- function(delta0, n11, n21, n1p, n2p, pi1hat,
pi2hat, alpha) {
	ml.res = ML_estimates(n11, n21, n1p, n2p, delta0)
	T0 <- score_test_statistic(
		pi1hat, pi2hat, delta0, ml.res$p1hat, ml.res$p2hat, n1p, n2p
	)
	if (is.na(T0)) {
		T0 <- 0
	}
	z <- qnorm(1-alpha / 2, 0, 1)
	f <- T0 + z
	return(f)
}

ML_estimates.Mee <- function(n11, n21, n1p, n2p, delta0) {
	L3 <- n1p + n2p
	L2 <- (n1p + 2 * n2p) * delta0 - (n1p + n2p) - (n11 + n21)
	L1 <- (n2p * delta0 - (n1p + n2p) - 2 * n21) * delta0 + (n11 + n21)
	L0 <- n21 * delta0 * (1 - delta0)
	q <- L2 ^ 3 / ((3 * L3) ^ 3) - L1 * L2 / (6 * L3 ^ 2) + L0 / (2 * L3)
	p <- sign(q) * sqrt(L2 ^ 2 / (3 * L3) ^ 2 - L1 / (3 * L3))
	a <- (1 / 3) * (pi + acos(q / p ^ 3))
	p2hat <- 2 * p*cos(a) - L2 / (3 * L3)
	p1hat <- p2hat + delta0
	res <- data.frame(p1hat=p1hat, p2hat=p2hat)
	return(res)
}

score_test_statistic.Mee <- function(pi1hat, pi2hat, delta0, p1hat, p2hat, n1p,
n2p) {
	T0 <- (pi1hat - pi2hat - delta0) / sqrt(p1hat * (1 - p1hat) / n1p + p2hat *
		(1 - p2hat) / n2p)
	return(T0)
}

# ======================================================== #
# Methods for Koopman                                      #
# ======================================================== #

calculate_limit_lower.Koopman <- function(phi0, n11, n21, n1p, n2p, pi1hat,
pi2hat, alpha) {
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

calculate_limit_upper.Koopman <- function(phi0, n11, n21, n1p, n2p, pi1hat,
pi2hat, alpha) {
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

ML_estimates.Koopman <- function(n11, n21, n1p, n2p, phi0) {
	A0 <- (n1p + n2p) * phi0
	B0 <- -(n1p * phi0 + n11 + n2p + n21 * phi0)
	C0 <- n11 + n21
	p2hat <- (-B0 - sqrt(B0 * B0 - 4 * A0 * C0)) / (2 * A0)
	p1hat <- p2hat * phi0
	res <- data.frame(p1hat=p1hat, p2hat=p2hat)
	return(res)
}

score_test_statistic.Koopman <- function(pi1hat, pi2hat, p1hat, p2hat, n1p,
n2p, phi0) {
	T0 <- (pi1hat - phi0 * pi2hat) / sqrt(p1hat * (1 - p1hat) / n1p +
		(phi0 ^ 2) * p2hat * (1 - p2hat) / n2p)
	return(T0)
}

# ======================================================== #
# Methods for Miettinen-Nurminen difference                 #
# ======================================================== #

calculate_limit_lower.Miettinen_diff <- function(delta0, n11, n21, n1p, n2p, pi1hat, pi2hat, alpha) {
	ml.res = ML_estimates(n11, n21, n1p, n2p, delta0)
	T0 <- score_test_statistic(pi1hat, pi2hat, delta0, ml.res$p1hat, ml.res$p2hat, n1p, n2p)
	if (is.na(T0)) {
		T0 <- 0
	}
	z <- qnorm(1-alpha / 2, 0, 1)
	f <- T0 - z
	return(f)
}

calculate_limit_upper.Miettinen_diff <- function(delta0, n11, n21, n1p, n2p, pi1hat, pi2hat, alpha) {
	ml.res = ML_estimates(n11, n21, n1p, n2p, delta0)
	T0 <- score_test_statistic(pi1hat, pi2hat, delta0, ml.res$p1hat, ml.res$p2hat, n1p, n2p)
	if (is.na(T0)) {
		T0 <- 0
	}
	z <- qnorm(1-alpha / 2, 0, 1)
	f <- T0 + z
	return(f)
}

ML_estimates.Miettinen_diff <- function(n11, n21, n1p, n2p, delta0) {
	L3 <- n1p + n2p
	L2 <- (n1p + 2 * n2p) * delta0 - (n1p + n2p) - (n11 + n21)
	L1 <- (n2p * delta0 - (n1p + n2p) - 2 * n21) * delta0 + (n11 + n21)
	L0 <- n21 * delta0 * (1 - delta0)
	q <- L2 ^ 3 / ((3 * L3) ^ 3) - L1 * L2 / (6 * L3 ^ 2) + L0 / (2 * L3)
	p <- sign(q) * sqrt(L2 ^ 2 / (3 * L3) ^ 2 - L1 / (3 * L3))
	a <- (1 / 3) * (pi + acos(q / p ^ 3))
	p2hat <- 2 * p*cos(a) - L2 / (3 * L3)
	p1hat <- p2hat + delta0
	res <- data.frame(p1hat=p1hat, p2hat=p2hat)
	return(res)
}

score_test_statistic.Miettinen_diff <- function(pi1hat, pi2hat, delta0, p1hat, p2hat, n1p, n2p) {
	T0 <- (pi1hat - pi2hat - delta0) / sqrt(p1hat * (1 - p1hat) / n1p + p2hat * (1 - p2hat) / n2p)
	T0 <- T0 * sqrt(1 - 1 / (n1p + n2p))
	return(T0)
}

# ======================================================== #
# Methods for Miettinen-Nurminen Odds Ratio                 #
# ======================================================== #

calculate_limit_lower.Miettinen_OR <- function(theta0, n11, n21, n1p, n2p, alpha) {
	T0 <- score_test_statistic(theta0, n11, n21, n1p, n2p)
	if (is.na(T0)) {
		T0 <- 0
	}
	f <- T0 - qnorm(1-alpha / 2, 0, 1)
	return(f)
}

calculate_limit_upper.Miettinen_OR <- function(theta0, n11, n21, n1p, n2p, alpha) {
	T0 <- score_test_statistic(theta0, n11, n21, n1p, n2p)
	if (is.na(T0)) {
		T0 <- 0
	}
	f <- T0 + qnorm(1-alpha / 2, 0, 1)
	return(f)
}

score_test_statistic.Miettinen_OR <- function(theta0, n11, n21, n1p, n2p) {
	res <- ML_estimates(theta0, n11, n21, n1p, n2p)
	T0 <- (n1p * (n11 / n1p - res$p1hat)) * sqrt(1 / (n1p * res$p1hat * (1 - res$p1hat)) + 1 / (n2p * res$p2hat * (1 - res$p2hat)))
	T0 <- T0 * sqrt(1 - 1 / (n1p + n2p))
	return(T0)
}

ML_estimates.Miettinen_OR <- function(theta0, n11, n21, n1p, n2p) {
	A <- n2p * (theta0 - 1)
	B <- n1p * theta0 + n2p - (n11 + n21) * (theta0 - 1)
	C <- -(n11 + n21)
	p2hat <- (-B + sqrt(B ^ 2 - 4 * A*C)) / (2 * A)
	p1hat <- p2hat * theta0 / (1 + p2hat * (theta0 - 1))
	res <- data.frame(p1hat=p1hat, p2hat=p2hat)
	return(res)
}

# ======================================================== #
# Methods for Miettinen-Nurminen CI ratio                  #
# ======================================================== #

calculate_limit_lower.Miettinen_ratio <- function(phi0,n11,n21,n1p,n2p,pi1hat,pi2hat,alpha) {
	res <- ML_estimates(n11, n21, n1p, n2p, phi0)
	T0 <- score_test_statistic(pi1hat, pi2hat, res$p1hat, res$p2hat, n1p, n2p, phi0)
	if (is.na(T0)) {
		T0 <- 0
	}
	f <- T0 - qnorm(1-alpha / 2, 0, 1)
	return(f)
}

calculate_limit_upper.Miettinen_ratio <- function(phi0,n11,n21,n1p,n2p,pi1hat,pi2hat,alpha) {
	res <- ML_estimates(n11, n21, n1p, n2p, phi0)
	T0 <- score_test_statistic(pi1hat, pi2hat, res$p1hat, res$p2hat, n1p, n2p, phi0)
	if (is.na(T0)) {
		T0 <- 0
	}
	f <- T0 + qnorm(1-alpha / 2, 0, 1)
	return(f)
}

ML_estimates.Miettinen_ratio <- function(n11, n21, n1p, n2p, phi0) {
	A0 <- (n1p + n2p) * phi0
	B0 <- -(n1p * phi0 + n11 + n2p + n21 * phi0)
	C0 <- n11 + n21
	p2hat <- (-B0 - sqrt(B0 * B0 - 4 * A0 * C0)) / (2 * A0)
	p1hat <- p2hat * phi0
	res <- data.frame(p1hat=p1hat, p2hat=p2hat)
	return(res)
}

score_test_statistic.Miettinen_ratio <- function(pi1hat, pi2hat, p1hat, p2hat, n1p, n2p, phi0) {
	T0 <- (pi1hat - phi0 * pi2hat) / sqrt(p1hat * (1 - p1hat) / n1p + (phi0 ^ 2) * p2hat * (1 - p2hat) / n2p)
	T0 <- T0 * sqrt(1 - 1 / (n1p + n2p))
	return(T0)
}

# ======================================================== #
# Methods for the uncorrected asymptotic score             #
# ======================================================== #

calculate_limit_lower.Uncorrected <- function(theta0, n11, n21, n1p, n2p, alpha) {
	T0 <- score_test_statistic(theta0, n11, n21, n1p, n2p)
	if (is.na(T0)) {
		T0 <- 0
	}
	f <- T0 - qnorm(1-alpha / 2, 0, 1)
	return(f)
}

calculate_limit_upper.Uncorrected <- function(theta0, n11, n21, n1p, n2p, alpha) {
	T0 <- score_test_statistic(theta0, n11, n21, n1p, n2p)
	if (is.na(T0)) {
		T0 <- 0
	}
	f <- T0 + qnorm(1-alpha / 2, 0, 1)
	return(f)
}

score_test_statistic.Uncorrected <- function(theta0, n11, n21, n1p, n2p) {
	res <- ML_estimates(theta0, n11, n21, n1p, n2p)
	T0 <- (n1p * (n11 / n1p - res$p1hat)) * sqrt(1 / (n1p * res$p1hat * (1 - res$p1hat)) + 1 / (n2p * res$p2hat * (1 - res$p2hat)))
	return(T0)
}

ML_estimates.Uncorrected <- function(theta0, n11, n21, n1p, n2p) {
	A0 <- n2p * (theta0 - 1)
	B0 <- n1p * theta0 + n2p - (n11 + n21) * (theta0 - 1)
	C0 <- -(n11 + n21)
	p2hat <- (-B0 + sqrt(B0 ^ 2 - 4 * A0 * C0)) / (2 * A0)
	p1hat <- p2hat * theta0 / (1 + p2hat * (theta0 - 1))
	res <- data.frame(p1hat=p1hat, p2hat=p2hat)
	return(res)
}

# ======================================================== #
# Methods for Exact_cond_midP_unspecific_ordering_rx2      #
# ======================================================== #

# Calculate the probability of table x
# (multiple hypergeometric distribution)

calc_prob.ExactCond <- function(x, r, N_choose_np1, nip_choose_xi1) {
	f <- 1
	for (i in 1:r) {
		f <- f * nip_choose_xi1[i, x[i] + 1]
	}
	f <- f / N_choose_np1
	return(f)
}

# Brute force calculations of the two-sided exact P-value and the mid-P value
# This function assumes r=4 rows

calc_Pvalue_4x2.ExactCond <- function(Tobs, nip, np1, npj, N, N_choose_np1, nip_choose_xi1, direction, statistic) {
	P <- 0
	point_prob <- 0
	for (x1 in 0:min(nip[1], np1)) {
		for (x2 in 0:min(nip[2], np1-x1)) {
			for (x3 in 0:min(nip[3], np1-x1-x2)) {
				x4 <- np1 - x1 - x2 - x3
				if (x4 > nip[4]) {
					next
				}
				x <- rbind(c(x1,nip[1]-x1),c(x2,nip[2]-x2),c(x3,nip[3]-x3),c(x4,nip[4]-x4))
				T0 <- test.statistic(x, 4, nip, npj, N, direction, statistic)
				f <- calc_prob.ExactCond(x[,1], 4, N_choose_np1, nip_choose_xi1)
				if (T0 == Tobs) {
					point_prob <- point_prob + f
				} else if (T0 > Tobs) {
					P <- P + f
				}
			}
		}
	}
	midP <- P + 0.5 * point_prob
	P <- P + point_prob
	res <- list(P=P, midP=midP)
	return(res)
}

# Brute force calculations of the two-sided exact P-value and the mid-P value
# This function assumes r=5 rows

calc_Pvalue_5x2.ExactCond <- function(Tobs, nip, np1, npj, N, N_choose_np1, nip_choose_xi1, direction, statistic) {
	P <- 0
	point_prob <- 0
	for (x1 in 0:min(nip[1], np1)) {
		for (x2 in 0:min(nip[2], np1-x1)) {
			for (x3 in 0:min(nip[3], np1-x1-x2)) {
				for (x4 in 0:min(nip[4], np1-x1-x2-x3)) {
					x5 <- np1 - x1 - x2 - x3 - x4
					if (x5 > nip[5]) {
						next
					}
					x <- rbind(c(x1, nip[1]-x1),c(x2,nip[2]-x2),c(x3,nip[3]-x3),
					c(x4,nip[4]-x4),c(x5,nip[5]-x5))
					T0 <- test.statistic(x, 5, nip, npj, N, direction, statistic)
					f <- calc_prob.ExactCond(x[,1], 5, N_choose_np1, nip_choose_xi1)
					if (T0 == Tobs) {
						point_prob <- point_prob + f
					} else if (T0 > Tobs) {
						P <- P + f
					}
				}
			}
		}
	}
	midP <- P + 0.5 * point_prob
	P <- P + point_prob
	res <- list(P=P, midP=midP)
	return(res)
}

# ======================================================== #
# Methods for Cochran-Armitage                             #
# ======================================================== #

# Calculate the probability of table x
# (multiple hypergeometric distribution)

calc_prob.CochranArmitage <- function(x, r, N_choose_np1, nip_choose_xi1) {
	f <- 1
	for (i in 1:r) {
		f <- f * nip_choose_xi1[i, x[i] + 1]
	}
	f <- f / N_choose_np1
	return(f)
}


# Brute force calculations of the one-sided P-values. Return the smallest one.
# This function assumes r=4 rows

calc_Pvalue_4x2.CochranArmitage <- function(Tobs, nip, np1, N_choose_np1, nip_choose_xi1, a) {
	left_sided_P <- 0
	right_sided_P <- 0
	point_prob <- 0
	for (x1 in 0:min(nip[1], np1)) {
		for (x2 in 0:min(nip[2], np1-x1)) {
			for (x3 in 0:min(nip[3], np1-x1-x2)) {
				x4 <- np1 - x1 - x2 - x3
				if (x4 > nip[4]) {
					next
				}
				x <- c(x1, x2, x3, x4)
				T0 <- linear_rank_test_statistic.CochranArmitage(x, a)
				f <- calc_prob.CochranArmitage(x, 4, N_choose_np1, nip_choose_xi1)
				if (T0 == Tobs) {
					point_prob <- point_prob + f
				} else if (T0 < Tobs) {
					left_sided_P <- left_sided_P + f
				} else if (T0 > Tobs) {
					right_sided_P <- right_sided_P + f
				}
			}
		}
	}
	one_sided_P <- min(left_sided_P, right_sided_P) + point_prob
	res <- data.frame(one_sided_P=one_sided_P, point_prob=point_prob)
	return(res)
}

# Brute force calculations of the one-sided P-values. Return the smallest one.
# This function assumes r=5 rows

calc_Pvalue_5x2.CochranArmitage <- function(Tobs, nip, np1, N_choose_np1, nip_choose_xi1, a) {
	left_sided_P <- 0
	right_sided_P <- 0
	point_prob <- 0
	for (x1 in 0:min(nip[1], np1)) {
		for (x2 in 0:min(nip[2], np1-x1)) {
			for (x3 in 0:min(nip[3], np1-x1-x2)) {
				for (x4 in 0:min(nip[4], np1-x1-x2-x3)) {
					x5 <- np1 - x1 - x2 - x3 - x4
					if (x5 > nip[5]) {next}
					x <- c(x1, x2, x3, x4, x5)
					T0 <- linear_rank_test_statistic.CochranArmitage(x, a)
					f <- calc_prob.CochranArmitage(x, 5, N_choose_np1, nip_choose_xi1)
					if (T0 == Tobs) {
						point_prob <- point_prob + f
					} else if (T0 < Tobs) {
						left_sided_P <- left_sided_P + f
					} else if (T0 > Tobs) {
						right_sided_P <- right_sided_P + f
					}
				}
			}
		}
	}
	one_sided_P <- min(left_sided_P, right_sided_P) + point_prob
	res <- data.frame(one_sided_P=one_sided_P, point_prob=point_prob)
	return(res)
}

# The linear rank test statistic, which gives an equivalent ordering of
# tables as the Cochran-Armitage test statistic (under conditioning on
# both row and column sums)
linear_rank_test_statistic.CochranArmitage <- function(x, a) {
	T0 <- sum(x * a)
	return(T0)
}