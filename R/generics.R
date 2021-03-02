# ======================================================== #
# Generic functions                                        #
# ======================================================== #

calculate_limit_lower <- function(...) {
	method <- convertFunName2Method(sys.calls()[[1]])
	class(method) <- method
	browser()#TEMP
	UseMethod("calculate_limit_lower", method)
}

calculate_limit_upper <- function(...) {
	method <- convertFunName2Method(sys.calls()[[1]])
	class(method) <- method
	UseMethod("calculate_limit_upper", method)
}

ML_estimates <- function(...) {
	UseMethod("ML_estimates")
}

score_test_statistic <- function(...) {
	UseMethod("score_test_statistic")
}

convertFunName2Method <- function(fun_name) {
	if (fun_name == "MiettinenNurminen_asymptotic_score_CI_OR_2x2(n)") {
		return("Mettinen_diff")
	} else if (fun_name == "Koopman_asymptotic_score_CI_2x2(n)") {
		return("Koopman")
	}
}

# ======================================================== #
# Methods for Mee                                          #
# ======================================================== #

calculate_limit_lower.Mee <- function(delta0, n11, n21, n1p, n2p, pi1hat,
										pi2hat, alpha) {
	ml.res = ML_estimates.Mee(n11, n21, n1p, n2p, delta0)
	T0 <- score_test_statistic.Mee(
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
	ml.res = ML_estimates.Mee(n11, n21, n1p, n2p, delta0)
	T0 <- score_test_statistic.Mee(
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
	ml.res = ML_estimates.Koopman(n11, n21, n1p, n2p, phi0)
	T0 <- score_test_statistic.Koopman(
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
	ml.res = ML_estimates.Koopman(n11, n21, n1p, n2p, phi0)
	T0 <- score_test_statistic.Koopman(
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
# Methods for Mettinen-Nurminen difference                 #
# ======================================================== #

calculate_limit_lower.Mettinen_diff <- function(delta0, n11, n21, n1p, n2p, pi1hat, pi2hat, alpha) {
	# global n11 n21 n1p n2p alphaglobal pi1hat pi2hat limit
	ml.res = ML_estimates.Mettinen_diff(n11, n21, n1p, n2p, delta0)
	T0 <- score_test_statistic.Mettinen_diff(pi1hat, pi2hat, delta0, ml.res$p1hat, ml.res$p2hat, n1p, n2p)
	if (is.na(T0)) {
		T0 <- 0
	}
	z <- qnorm(1-alpha / 2, 0, 1)
	f <- T0 - z
	return(f)
}

calculate_limit_upper.Mettinen_diff <- function(delta0, n11, n21, n1p, n2p, pi1hat, pi2hat, alpha) {
	# global n11 n21 n1p n2p alphaglobal pi1hat pi2hat limit
	ml.res = ML_estimates.Mettinen_diff(n11, n21, n1p, n2p, delta0)
	T0 <- score_test_statistic.Mettinen_diff(pi1hat, pi2hat, delta0, ml.res$p1hat, ml.res$p2hat, n1p, n2p)
	if (is.na(T0)) {
		T0 <- 0
	}
	z <- qnorm(1-alpha / 2, 0, 1)
	f <- T0 + z
	return(f)
}

ML_estimates.Mettinen_diff <- function(n11, n21, n1p, n2p, delta0) {
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

score_test_statistic.Mettinen_diff <- function(pi1hat, pi2hat, delta0, p1hat, p2hat, n1p, n2p) {
	T0 <- (pi1hat - pi2hat - delta0) / sqrt(p1hat * (1 - p1hat) / n1p + p2hat * (1 - p2hat) / n2p)
	T0 <- T0 * sqrt(1 - 1 / (n1p + n2p))
	return(T0)
}

# ======================================================== #
# Methods for Mettinen-Nurminen Odds Ratio                 #
# ======================================================== #

calculate_limit_lower.Mettinen_OR <- function(theta0, n11, n21, n1p, n2p, alpha) {
	# global n11 n21 n1p n2p alphaglobal limit
	T0 <- score_test_statistic.Mettinen_OR(theta0, n11, n21, n1p, n2p)
	if (is.na(T0)) {
		T0 <- 0
	}
	f <- T0 - qnorm(1-alpha / 2, 0, 1)
	return(f)
}

calculate_limit_upper.Mettinen_OR <- function(theta0, n11, n21, n1p, n2p, alpha) {
	# global n11 n21 n1p n2p alphaglobal limit
	T0 <- score_test_statistic.Mettinen_OR(theta0, n11, n21, n1p, n2p)
	if (is.na(T0)) {
		T0 <- 0
	}
	f <- T0 + qnorm(1-alpha / 2, 0, 1)
	return(f)
}

score_test_statistic.Mettinen_OR <- function(theta0, n11, n21, n1p, n2p) {
	res <- ML_estimates.Mettinen_OR(theta0, n11, n21, n1p, n2p)
	T0 <- (n1p * (n11 / n1p - res$p1hat)) * sqrt(1 / (n1p * res$p1hat * (1 - res$p1hat)) + 1 / (n2p * res$p2hat * (1 - res$p2hat)))
	T0 <- T0 * sqrt(1 - 1 / (n1p + n2p))
	return(T0)
}

ML_estimates.Mettinen_OR <- function(theta0, n11, n21, n1p, n2p) {
	A <- n2p * (theta0 - 1)
	B <- n1p * theta0 + n2p - (n11 + n21) * (theta0 - 1)
	C <- -(n11 + n21)
	p2hat <- (-B + sqrt(B ^ 2 - 4 * A*C)) / (2 * A)
	p1hat <- p2hat * theta0 / (1 + p2hat * (theta0 - 1))
	res <- data.frame(p1hat=p1hat, p2hat=p2hat)
	return(res)
}
