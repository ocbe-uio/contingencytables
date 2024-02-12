# ======================================================== #
# Generic functions                                        #
# ======================================================== #

#' @export
#' @title Calculate the lower limit of a confidence interval
#' @param ... arguments passed to methods
#' @note This function has little use to the user, it is exported so that
#' it can be used by [stats::uniroot()].
calculate_limit_lower <- function(...) {
  method <- convertFunName2Method()
  UseMethod("calculate_limit_lower", method)
}

#' @export
#' @title Calculate the upper limit of a confidence interval
#' @param ... arguments passed to methods
#' @note This function has little use to the user, it is exported so that
#' it can be used by [stats::uniroot()].
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

dispatch_lookup <- data.frame(
  "fn" = c(
    "Koopman_asymptotic_score_CI_2x2",
    "Mee_asymptotic_score_CI_2x2",
    "MiettinenNurminen_asymptotic_score_CI_difference_2x2",
    "MiettinenNurminen_asymptotic_score_CI_OR_2x2",
    "MiettinenNurminen_asymptotic_score_CI_ratio_2x2",
    "Uncorrected_asymptotic_score_CI_2x2",
    "CochranArmitage_exact_cond_midP_tests_rx2",
    "Exact_cond_midP_linear_rank_tests_2xc",
    "Exact_cond_midP_unspecific_ordering_rx2"
  ),
  "cls" = c(
    "Koopman", "Mee", "Miettinen_diff", "Miettinen_OR", "Miettinen_ratio",
    "Uncorrected", "CochranArmitage", "ExactCond_linear",
    "ExactCond_unspecific"
  )
)

#' @author Waldir Leoncio
convertFunName2Method <- function() {
  callstack <- gsub(x = as.character(sys.calls()), "\\(.+$", "") # func names
  cls_match <- dispatch_lookup[match(callstack, dispatch_lookup[["fn"]]), "cls"]
  cls <- cls_match[!is.na(cls_match)]
  class(cls) <- cls
  return(cls)
}

# ======================================================== #
# Methods for Mee                                          #
# ======================================================== #

#' @export
calculate_limit_lower.Mee <- function(delta0, n11, n21, n1p, n2p, pi1hat,
                                      pi2hat, alpha, ...) {
  ml.res <- ML_estimates(n11, n21, n1p, n2p, delta0)
  T0 <- score_test_statistic(
    pi1hat, pi2hat, delta0, ml.res$p1hat, ml.res$p2hat, n1p, n2p
  )
  z <- qnorm(1 - alpha / 2, 0, 1)
  f <- T0 - z
  return(f)
}

#' @export
calculate_limit_upper.Mee <- function(delta0, n11, n21, n1p, n2p, pi1hat,
                                      pi2hat, alpha, ...) {
  ml.res <- ML_estimates(n11, n21, n1p, n2p, delta0)
  T0 <- score_test_statistic(
    pi1hat, pi2hat, delta0, ml.res$p1hat, ml.res$p2hat, n1p, n2p
  )
  z <- qnorm(1 - alpha / 2, 0, 1)
  f <- T0 + z
  return(f)
}

ML_estimates.Mee <- function(n11, n21, n1p, n2p, delta0, ...) {
  L3 <- n1p + n2p
  L2 <- (n1p + 2 * n2p) * delta0 - (n1p + n2p) - (n11 + n21)
  L1 <- (n2p * delta0 - (n1p + n2p) - 2 * n21) * delta0 + (n11 + n21)
  L0 <- n21 * delta0 * (1 - delta0)
  q <- L2^3 / ((3 * L3)^3) - L1 * L2 / (6 * L3^2) + L0 / (2 * L3)
  p <- sign(q) * sqrt(L2^2 / (3 * L3)^2 - L1 / (3 * L3))
  a <- (1 / 3) * (pi + acos(q / p^3))
  p2hat <- 2 * p * cos(a) - L2 / (3 * L3)
  p1hat <- p2hat + delta0
  res <- data.frame(p1hat = p1hat, p2hat = p2hat)
  return(res)
}

score_test_statistic.Mee <- function(pi1hat, pi2hat, delta0, p1hat, p2hat, n1p,
                                     n2p, ...) {
  T0 <- (pi1hat - pi2hat - delta0) / sqrt(p1hat * (1 - p1hat) / n1p + p2hat *
    (1 - p2hat) / n2p)
  return(T0)
}

# ======================================================== #
# Methods for Koopman                                      #
# ======================================================== #

#' @export
calculate_limit_lower.Koopman <- function(phi0, n11, n21, n1p, n2p, pi1hat,
                                          pi2hat, alpha, ...) {
  ml.res <- ML_estimates(n11, n21, n1p, n2p, phi0)
  T0 <- score_test_statistic(
    pi1hat, pi2hat, ml.res$p1hat, ml.res$p2hat, n1p, n2p, phi0
  )
  f <- T0 - qnorm(1 - alpha / 2, 0, 1)
  return(f)
}

#' @export
calculate_limit_upper.Koopman <- function(phi0, n11, n21, n1p, n2p, pi1hat,
                                          pi2hat, alpha, ...) {
  ml.res <- ML_estimates(n11, n21, n1p, n2p, phi0)
  T0 <- score_test_statistic(
    pi1hat, pi2hat, ml.res$p1hat, ml.res$p2hat, n1p, n2p, phi0
  )
  f <- T0 + qnorm(1 - alpha / 2, 0, 1)
  return(f)
}

ML_estimates.Koopman <- function(n11, n21, n1p, n2p, phi0, ...) {
  A0 <- (n1p + n2p) * phi0
  B0 <- -(n1p * phi0 + n11 + n2p + n21 * phi0)
  C0 <- n11 + n21
  p2hat <- (-B0 - sqrt(B0 * B0 - 4 * A0 * C0)) / (2 * A0)
  p1hat <- p2hat * phi0
  res <- data.frame(p1hat = p1hat, p2hat = p2hat)
  return(res)
}

score_test_statistic.Koopman <- function(pi1hat, pi2hat, p1hat, p2hat, n1p,
                                         n2p, phi0, ...) {
  T0 <- (pi1hat - phi0 * pi2hat) / sqrt(p1hat * (1 - p1hat) / n1p +
    (phi0^2) * p2hat * (1 - p2hat) / n2p)
  return(T0)
}

# ======================================================== #
# Methods for Miettinen-Nurminen difference                 #
# ======================================================== #

#' @export
calculate_limit_lower.Miettinen_diff <- function(delta0, n11, n21, n1p, n2p, pi1hat, pi2hat, alpha, ...) {
  ml.res <- ML_estimates(n11, n21, n1p, n2p, delta0)
  T0 <- score_test_statistic(pi1hat, pi2hat, delta0, ml.res$p1hat, ml.res$p2hat, n1p, n2p)
  z <- qnorm(1 - alpha / 2, 0, 1)
  f <- T0 - z
  return(f)
}

#' @export
calculate_limit_upper.Miettinen_diff <- function(delta0, n11, n21, n1p, n2p, pi1hat, pi2hat, alpha, ...) {
  ml.res <- ML_estimates(n11, n21, n1p, n2p, delta0)
  T0 <- score_test_statistic(pi1hat, pi2hat, delta0, ml.res$p1hat, ml.res$p2hat, n1p, n2p)
  z <- qnorm(1 - alpha / 2, 0, 1)
  f <- T0 + z
  return(f)
}

ML_estimates.Miettinen_diff <- function(n11, n21, n1p, n2p, delta0, ...) {
  L3 <- n1p + n2p
  L2 <- (n1p + 2 * n2p) * delta0 - (n1p + n2p) - (n11 + n21)
  L1 <- (n2p * delta0 - (n1p + n2p) - 2 * n21) * delta0 + (n11 + n21)
  L0 <- n21 * delta0 * (1 - delta0)
  q <- L2^3 / ((3 * L3)^3) - L1 * L2 / (6 * L3^2) + L0 / (2 * L3)
  p <- sign(q) * sqrt(L2^2 / (3 * L3)^2 - L1 / (3 * L3))
  a <- (1 / 3) * (pi + acos(q / p^3))
  p2hat <- 2 * p * cos(a) - L2 / (3 * L3)
  p1hat <- p2hat + delta0
  res <- data.frame(p1hat = p1hat, p2hat = p2hat)
  return(res)
}

score_test_statistic.Miettinen_diff <- function(pi1hat, pi2hat, delta0, p1hat, p2hat, n1p, n2p, ...) {
  T0 <- (pi1hat - pi2hat - delta0) / sqrt(p1hat * (1 - p1hat) / n1p + p2hat * (1 - p2hat) / n2p)
  T0 <- T0 * sqrt(1 - 1 / (n1p + n2p))
  return(T0)
}

# ======================================================== #
# Methods for Miettinen-Nurminen Odds Ratio                 #
# ======================================================== #

#' @export
calculate_limit_lower.Miettinen_OR <- function(theta0, n11, n21, n1p, n2p, alpha, ...) {
  T0 <- score_test_statistic(theta0, n11, n21, n1p, n2p)
  f <- T0 - qnorm(1 - alpha / 2, 0, 1)
  return(f)
}

#' @export
calculate_limit_upper.Miettinen_OR <- function(theta0, n11, n21, n1p, n2p, alpha, ...) {
  T0 <- score_test_statistic(theta0, n11, n21, n1p, n2p)
  f <- T0 + qnorm(1 - alpha / 2, 0, 1)
  return(f)
}

score_test_statistic.Miettinen_OR <- function(theta0, n11, n21, n1p, n2p, ...) {
  res <- ML_estimates(theta0, n11, n21, n1p, n2p)
  T0 <- (n1p * (n11 / n1p - res$p1hat)) * sqrt(1 / (n1p * res$p1hat * (1 - res$p1hat)) + 1 / (n2p * res$p2hat * (1 - res$p2hat)))
  T0 <- T0 * sqrt(1 - 1 / (n1p + n2p))
  return(T0)
}

ML_estimates.Miettinen_OR <- function(theta0, n11, n21, n1p, n2p, ...) {
  A <- n2p * (theta0 - 1)
  B <- n1p * theta0 + n2p - (n11 + n21) * (theta0 - 1)
  C <- -(n11 + n21)
  p2hat <- (-B + sqrt(B^2 - 4 * A * C)) / (2 * A)
  p1hat <- p2hat * theta0 / (1 + p2hat * (theta0 - 1))
  res <- data.frame(p1hat = p1hat, p2hat = p2hat)
  return(res)
}

# ======================================================== #
# Methods for Miettinen-Nurminen CI ratio                  #
# ======================================================== #

#' @export
calculate_limit_lower.Miettinen_ratio <- function(phi0, n11, n21, n1p, n2p, pi1hat, pi2hat, alpha, ...) {
  res <- ML_estimates(n11, n21, n1p, n2p, phi0)
  T0 <- score_test_statistic(pi1hat, pi2hat, res$p1hat, res$p2hat, n1p, n2p, phi0)
  f <- T0 - qnorm(1 - alpha / 2, 0, 1)
  return(f)
}

#' @export
calculate_limit_upper.Miettinen_ratio <- function(phi0, n11, n21, n1p, n2p, pi1hat, pi2hat, alpha, ...) {
  res <- ML_estimates(n11, n21, n1p, n2p, phi0)
  T0 <- score_test_statistic(pi1hat, pi2hat, res$p1hat, res$p2hat, n1p, n2p, phi0)
  f <- T0 + qnorm(1 - alpha / 2, 0, 1)
  return(f)
}

ML_estimates.Miettinen_ratio <- function(n11, n21, n1p, n2p, phi0, ...) {
  A0 <- (n1p + n2p) * phi0
  B0 <- -(n1p * phi0 + n11 + n2p + n21 * phi0)
  C0 <- n11 + n21
  p2hat <- (-B0 - sqrt(B0 * B0 - 4 * A0 * C0)) / (2 * A0)
  p1hat <- p2hat * phi0
  res <- data.frame(p1hat = p1hat, p2hat = p2hat)
  return(res)
}

score_test_statistic.Miettinen_ratio <- function(pi1hat, pi2hat, p1hat, p2hat, n1p, n2p, phi0, ...) {
  T0 <- (pi1hat - phi0 * pi2hat) / sqrt(p1hat * (1 - p1hat) / n1p + (phi0^2) * p2hat * (1 - p2hat) / n2p)
  T0 <- T0 * sqrt(1 - 1 / (n1p + n2p))
  return(T0)
}

# ======================================================== #
# Methods for the uncorrected asymptotic score             #
# ======================================================== #

#' @export
calculate_limit_lower.Uncorrected <- function(theta0, n11, n21, n1p, n2p, alpha, ...) {
  T0 <- score_test_statistic(theta0, n11, n21, n1p, n2p)
  f <- T0 - qnorm(1 - alpha / 2, 0, 1)
  return(f)
}

#' @export
calculate_limit_upper.Uncorrected <- function(theta0, n11, n21, n1p, n2p, alpha, ...) {
  T0 <- score_test_statistic(theta0, n11, n21, n1p, n2p)
  f <- T0 + qnorm(1 - alpha / 2, 0, 1)
  return(f)
}

score_test_statistic.Uncorrected <- function(theta0, n11, n21, n1p, n2p, ...) {
  res <- ML_estimates(theta0, n11, n21, n1p, n2p)
  T0 <- (n1p * (n11 / n1p - res$p1hat)) * sqrt(1 / (n1p * res$p1hat * (1 - res$p1hat)) + 1 / (n2p * res$p2hat * (1 - res$p2hat)))
  return(T0)
}

ML_estimates.Uncorrected <- function(theta0, n11, n21, n1p, n2p, ...) {
  A0 <- n2p * (theta0 - 1)
  B0 <- n1p * theta0 + n2p - (n11 + n21) * (theta0 - 1)
  C0 <- -(n11 + n21)
  p2hat <- (-B0 + sqrt(B0^2 - 4 * A0 * C0)) / (2 * A0)
  p1hat <- p2hat * theta0 / (1 + p2hat * (theta0 - 1))
  res <- data.frame(p1hat = p1hat, p2hat = p2hat)
  return(res)
}

# ======================================================== #
# Methods for Cochran-Armitage                             #
# ======================================================== #

# Calculate the probability of table x
# (multiple hypergeometric distribution)

calc_prob.CochranArmitage <- function(x, r, N_choose_np1, nip_choose_xi1, ...) {
  f <- 1
  for (i in 1:r) {
    f <- f * nip_choose_xi1[i, x[i] + 1]
  }
  f <- f / N_choose_np1
  return(f)
}


# Brute force calculations of the one-sided P-values. Return the smallest one.
# This function assumes r=4 rows

calc_Pvalue_4x2.CochranArmitage <- function(Tobs, nip, np1, N_choose_np1, nip_choose_xi1, a, ...) {
  left_sided_P <- 0
  right_sided_P <- 0
  point_prob <- 0
  for (x1 in 0:min(nip[1], np1)) {
    for (x2 in 0:min(nip[2], np1 - x1)) {
      for (x3 in 0:min(nip[3], np1 - x1 - x2)) {
        x4 <- np1 - x1 - x2 - x3
        if (x4 > nip[4]) {
          next
        }
        x <- c(x1, x2, x3, x4)
        T0 <- linear_rank_test_statistic(x, a)
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
  res <- data.frame(one_sided_P = one_sided_P, point_prob = point_prob)
  return(res)
}

# Brute force calculations of the one-sided P-values. Return the smallest one.
# This function assumes r=5 rows

calc_Pvalue_5x2.CochranArmitage <- function(Tobs, nip, np1, N_choose_np1, nip_choose_xi1, a, ...) {
  left_sided_P <- 0
  right_sided_P <- 0
  point_prob <- 0
  for (x1 in 0:min(nip[1], np1)) {
    for (x2 in 0:min(nip[2], np1 - x1)) {
      for (x3 in 0:min(nip[3], np1 - x1 - x2)) {
        for (x4 in 0:min(nip[4], np1 - x1 - x2 - x3)) {
          x5 <- np1 - x1 - x2 - x3 - x4
          if (x5 > nip[5]) {
            next
          }
          x <- c(x1, x2, x3, x4, x5)
          T0 <- linear_rank_test_statistic(x, a)
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
  res <- data.frame(one_sided_P = one_sided_P, point_prob = point_prob)
  return(res)
}

# ======================================================== #
# Methods for Exact_cond_midP_linear_rank_tests_2xc        #
# ======================================================== #

# Brute force calculations of the one-sided P-values. Return the smallest one.
# This function assumes c=3 columns

calc_Pvalue_2x3.ExactCond_linear <- function(Tobs, nip, npj, N_choose_n1p, npj_choose_x1j, b) {
  left_sided_P <- 0
  right_sided_P <- 0
  point_prob <- 0
  for (x1 in 0:min(c(npj[1], nip[1]))) {
    for (x2 in 0:min(c(npj[2], nip[1] - x1))) {
      x3 <- nip[1] - x1 - x2
      if (x3 > npj[3]) {
        next
      }
      x <- c(x1, x2, x3)
      T0 <- linear_rank_test_statistic(x, b)
      f <- calc_prob(x, 3, N_choose_n1p, npj_choose_x1j)
      if (T0 == Tobs) {
        point_prob <- point_prob + f
      } else if (T0 < Tobs) {
        left_sided_P <- left_sided_P + f
      } else if (T0 > Tobs) {
        right_sided_P <- right_sided_P + f
      }
    }
  }
  one_sided_P <- min(c(left_sided_P, right_sided_P)) + point_prob
  data.frame(one_sided_P = one_sided_P, point_prob = point_prob)
}

# Brute force calculations of the one-sided P-values. Return the smallest one.
# This function assumes c=4 columns

calc_Pvalue_2x4.ExactCond_linear <- function(Tobs, nip, npj, N_choose_n1p, npj_choose_x1j, b) {
  left_sided_P <- 0
  right_sided_P <- 0
  point_prob <- 0
  for (x1 in 0:min(c(npj[1], nip[1]))) {
    for (x2 in 0:min(c(npj[2], nip[1] - x1))) {
      for (x3 in 0:min(c(npj[3], nip[1] - x1 - x2))) {
        x4 <- nip[1] - x1 - x2 - x3
        if (x4 > npj[4]) {
          next
        }
        x <- c(x1, x2, x3, x4)
        T0 <- linear_rank_test_statistic(x, b)
        f <- calc_prob.ExactCond_linear(x, 4, N_choose_n1p, npj_choose_x1j)
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
  one_sided_P <- min(c(left_sided_P, right_sided_P)) + point_prob
  data.frame(one_sided_P = one_sided_P, point_prob = point_prob)
}

# Calculate the probability of table x
# (multiple hypergeometric distribution)

calc_prob.ExactCond_linear <- function(x, c, N_choose_n1p, npj_choose_x1j, ...) {
  f <- 1
  for (j in 1:c) {
    f <- f * npj_choose_x1j[j, x[j] + 1]
  }
  f <- f / N_choose_n1p
  return(f)
}

# ======================================================== #
# Methods for Exact_cond_midP_unspecific_ordering_rx2      #
# ======================================================== #

# Brute force calculations of the two-sided exact P-value and the mid-P value
# This function assumes r=4 rows

calc_Pvalue_4x2.ExactCond_unspecific <- function(Tobs, nip, np1, npj, N, N_choose_np1, nip_choose_xi1, direction, statistic, ...) {
  P <- 0
  point_prob <- 0
  for (x1 in 0:min(c(nip[1], np1))) {
    for (x2 in 0:min(c(nip[2], np1 - x1))) {
      for (x3 in 0:min(c(nip[3], np1 - x1 - x2))) {
        x4 <- np1 - x1 - x2 - x3
        if (x4 > nip[4]) {
          next
        }
        x <- rbind(c(x1, nip[1] - x1), c(x2, nip[2] - x2), c(x3, nip[3] - x3), c(x4, nip[4] - x4))
        T0 <- test_statistic(x, 4, nip, npj, N, direction, statistic)
        f <- calc_prob(x[, 1], 4, N_choose_np1, nip_choose_xi1)
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

  return(data.frame(P = P, midP = midP))
}

# Brute force calculations of the two-sided exact P-value and the mid-P value
# This function assumes r=5 rows

calc_Pvalue_5x2.ExactCond_unspecific <- function(Tobs, nip, np1, npj, N, N_choose_np1, nip_choose_xi1, direction, statistic, ...) {
  P <- 0
  point_prob <- 0
  for (x1 in 0:min(c(nip[1], np1))) {
    for (x2 in 0:min(c(nip[2], np1 - x1))) {
      for (x3 in 0:min(c(nip[3], np1 - x1 - x2))) {
        for (x4 in 0:min(c(nip[4], np1 - x1 - x2 - x3))) {
          x5 <- np1 - x1 - x2 - x3 - x4
          if (x5 > nip[5]) {
            next
          }
          x <- rbind(c(x1, nip[1] - x1), c(x2, nip[2] - x2), c(x3, nip[3] - x3), c(x4, nip[4] - x4), c(x5, nip[5] - x5))
          T0 <- test_statistic(x, 5, nip, npj, N, direction, statistic)
          f <- calc_prob(x[, 1], 5, N_choose_np1, nip_choose_xi1)
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

  return(data.frame(P = P, midP = midP))
}

# Calculate the probability of table x
# (multiple hypergeometric distribution)

calc_prob.ExactCond_unspecific <- function(x, r, N_choose_np1, nip_choose_xi1, ...) {
  f <- 1
  for (i in 1:r) {
    f <- f * nip_choose_xi1[i, x[i] + 1]
  }
  f <- f / N_choose_np1
  return(f)
}
