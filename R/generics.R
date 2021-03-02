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

convertFunName2Method <- function() {
	if (any(as.list(sys.calls()) == "Koopman_asymptotic_score_CI_2x2(n)")) {
		cls <- "Koopman"
	} else if (any(as.list(sys.calls()) == "Mee_asymptotic_score_CI_2x2(n)")) {
		cls <- "Mee"
	} else if (any(as.list(sys.calls()) == "MiettinenNurminen_asymptotic_score_CI_difference_2x2(n)")) {
		cls <- "Miettinen_diff"
	} else if (any(as.list(sys.calls()) == "MiettinenNurminen_asymptotic_score_CI_OR_2x2(n)")) {
		cls <- "Miettinen_OR"
	} else if (any(as.list(sys.calls()) == "MiettinenNurminen_asymptotic_score_CI_ratio_2x2(n)")) {
		cls <- "Miettinen_ratio"
	} else if (any(as.list(sys.calls()) == "Uncorrected_asymptotic_score_CI_2x2(n)")) {
		cls <- "Uncorrected"
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
