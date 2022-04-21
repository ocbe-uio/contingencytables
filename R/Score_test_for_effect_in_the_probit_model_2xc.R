#' @title Score test for effect in the cumulative probit model
#' @description The score test for effect in the cumulative probit model
#' described in Chapter 6 "The Ordered 2xc Table"
#' @param n the observed counts (a 2xc matrix)
#' @param alphahat0 a column vector with c-1 estimated coefficients
#' (\code{alpha_j}) under the null hypothesis (\code{beta = 0})
#' @param printresults display results (F = no, T = yes)
#' @note Must give the alphahats under the null hypothesis as input,
#'  because Matlab does not calculate an intercept-only probit model (and this
#' may apply to R code as well). alphahat0 can be calculated in, for instance,
#' Stata.
#' @importFrom stats dnorm
#' @examples
#' # The Adolescent Placement Study (Fontanella et al., 2008)
#' n <- rbind(c(8, 28, 72, 126), c(46, 73, 69, 86))
#' alphahat0 <- c(-1.246452, -0.5097363, 0.2087471)
#' Score_test_for_effect_in_the_probit_model_2xc(n, alphahat0)
#'
#' # Postoperative nausea (Lydersen et al., 2012a)
#' n <- rbind(c(14, 10, 3, 2), c(11, 7, 8, 4))
#' alphahat0 <- c(-0.1923633, 0.5588396, 1.271953)
#' Score_test_for_effect_in_the_probit_model_2xc(n, alphahat0)
#'
#' @export
#' @return A list containing the probability, the statistic and the degrees of freedom
Score_test_for_effect_in_the_probit_model_2xc <- function(n, alphahat0, printresults = TRUE) {
  c <- ncol(n)

  # Evaluate the derivatives under the null hypothesis: H0: beta = 0
  betahat <- 0
  dldb <- first_derivative_wrt_beta(n, c, alphahat0, betahat)
  dldalpha <- first_derivative_wrt_alpha(n, c, alphahat0, betahat)
  d2ldb2 <- second_derivative_wrt_beta(n, c, alphahat0, betahat)
  d2ldbdalpha <- second_derivative_wrt_beta_and_alpha(n, c, alphahat0, betahat)
  d2ldalpha2 <- second_derivative_wrt_alpha(n, c, alphahat0, betahat)

  # Vector of first-order derivatives
  theta <- c(dldalpha, dldb)

  # Information matrix
  I <- matrix(NA, c, c)
  I[1:(c - 1), 1:(c - 1)] <- d2ldalpha2
  I[c, 1:(c - 1)] <- d2ldbdalpha
  I[1:(c - 1), c] <- d2ldbdalpha
  I[c, c] <- d2ldb2

  # Calculate test statistic and P-value
  T0 <- theta %*% solve(-I) %*% theta
  df <- 1
  P <- 1 - pchisq(T0, 1)

  if (printresults) {
    print(
      sprintf("Score test for effect: P = %6.4f, T = %5.3f (df=%g)", P, T0, df),
      quote = TRUE
    )
  }
  invisible(data.frame(P = P, T = T0, df = df))
}

# =========================================================
first_derivative_wrt_beta <- function(n, c, alpha, beta) {
  alpha <- c(-Inf, alpha, Inf) # Now alphahat must be indexed by j+1
  dldb <- 0
  for (j in 1:c) {
    phi_j <- dnorm(alpha[j + 1] - beta)
    phi_jminus1 <- dnorm(alpha[j] - beta)
    PHI_j <- pnorm(alpha[j + 1] - beta)
    PHI_jminus1 <- pnorm(alpha[j] - beta)
    dldb <- dldb + n[1, j] * (phi_jminus1 - phi_j) / (PHI_j - PHI_jminus1)
  }
  return(dldb)
}

# ==============================================================
first_derivative_wrt_alpha <- function(n, c, alpha, beta) {
  alpha <- c(-Inf, alpha, Inf) # Now alphahat must be indexed by j+1
  dldalpha <- rep(0, c - 1)
  for (k in 1:(c - 1)) {
    for (j in 1:c) {
      jk <- ifelse(j == k, 1, 0)
      jk1 <- ifelse(j - 1 == k, 1, 0)
      phi_j <- dnorm(alpha[j + 1] - beta)
      phi_jminus1 <- dnorm(alpha[j] - beta)
      PHI_j <- pnorm(alpha[j + 1] - beta)
      PHI_jminus1 <- pnorm(alpha[j] - beta)
      dldalpha[k] <- dldalpha[k] + n[1, j] * (jk * phi_j - jk1 * phi_jminus1) /
        (PHI_j - PHI_jminus1)

      phi_j <- dnorm(alpha[j + 1])
      phi_jminus1 <- dnorm(alpha[j])
      PHI_j <- pnorm(alpha[j + 1])
      PHI_jminus1 <- pnorm(alpha[j])
      dldalpha[k] <- dldalpha[k] + n[2, j] * (jk * phi_j - jk1 * phi_jminus1) /
        (PHI_j - PHI_jminus1)
    }
  }
  return(dldalpha)
}

# ==========================================================
second_derivative_wrt_beta <- function(n, c, alpha, beta) {
  alpha <- c(-Inf, alpha, Inf) # Now alphahat must be indexed by j+1
  d2ldb2 <- 0
  for (j in 1:c) {
    phi_j <- dnorm(alpha[j + 1] - beta)
    phi_jminus1 <- dnorm(alpha[j] - beta)
    PHI_j <- pnorm(alpha[j + 1] - beta)
    PHI_jminus1 <- pnorm(alpha[j] - beta)
    term1 <- (alpha[j] - beta) * phi_jminus1
    term2 <- (alpha[j + 1] - beta) * phi_j
    if (is.na(term1)) {
      term1 <- 0
    }
    if (is.na(term2)) {
      term2 <- 0
    }
    d2ldb2 <- d2ldb2 + n[1, j] * (PHI_j - PHI_jminus1) * (term1 - term2) /
      ((PHI_j - PHI_jminus1)^2)
    d2ldb2 <- d2ldb2 - n[1, j] * ((phi_jminus1 - phi_j)^2) /
      ((PHI_j - PHI_jminus1)^2)
  }
  return(d2ldb2)
}

# ===========================================================================
second_derivative_wrt_beta_and_alpha <- function(n, c, alpha, beta) {
  alpha <- c(-Inf, alpha, Inf) # Now alphahat must be indexed by j+1
  d2ldbdalpha <- rep(0, c - 1)
  for (l in 1:(c - 1)) {
    for (j in 1:c) {
      jl <- ifelse(j == l, 1, 0)
      jl1 <- ifelse(j - 1 == l, 1, 0)
      phi_j <- dnorm(alpha[j + 1] - beta)
      phi_jminus1 <- dnorm(alpha[j] - beta)
      PHI_j <- pnorm(alpha[j + 1] - beta)
      PHI_jminus1 <- pnorm(alpha[j] - beta)
      term1 <- (alpha[j + 1] - beta) * phi_j
      term2 <- (alpha[j] - beta) * phi_jminus1
      if (is.na(term1)) {
        term1 <- 0
      }
      if (is.na(term2)) {
        term2 <- 0
      }
      d2ldbdalpha[l] <- d2ldbdalpha[l] + n[1, j] * (PHI_j - PHI_jminus1) *
        (jl * term1 - jl1 * term2) / ((PHI_j - PHI_jminus1)^2)
      d2ldbdalpha[l] <- d2ldbdalpha[l] - n[1, j] * (phi_jminus1 - phi_j) *
        (jl * phi_j - jl1 * phi_jminus1) / ((PHI_j - PHI_jminus1)^2)
    }
  }
  return(d2ldbdalpha)
}

#  =================================================================
second_derivative_wrt_alpha <- function(n, c, alpha, beta) {
  alpha <- c(-Inf, alpha, Inf) # Now alphahat must be indexed by j+1
  d2ldalpha2 <- matrix(0, c - 1, c - 1)
  for (k in 1:(c - 1)) {
    for (l in 1:(c - 1)) {
      for (j in 1:c) {
        jl <- ifelse(j == l, 1, 0)
        jl1 <- ifelse(j - 1 == l, 1, 0)
        jk <- ifelse(j == k, 1, 0)
        jk1 <- ifelse(j - 1 == k, 1, 0)
        phi_j <- dnorm(alpha[j + 1] - beta)
        phi_jminus1 <- dnorm(alpha[j] - beta)
        PHI_j <- pnorm(alpha[j + 1] - beta)
        PHI_jminus1 <- pnorm(alpha[j] - beta)
        term1 <- (alpha[j] - beta) * phi_jminus1
        term2 <- (alpha[j + 1] - beta) * phi_j
        if (is.na(term1)) {
          term1 <- 0
        }
        if (is.na(term2)) {
          term2 <- 0
        }
        d2ldalpha2[k, l] <- d2ldalpha2[k, l] + n[1, j] *
          ((PHI_j - PHI_jminus1) * (jk1 * jl1 * term1 - jk * jl * term2)) /
          ((PHI_j - PHI_jminus1)^2)
        d2ldalpha2[k, l] <- d2ldalpha2[k, l] - n[1, j] *
          ((jk * phi_j - jk1 * phi_jminus1) * (jl * phi_j - jl1 * phi_jminus1)) /
          ((PHI_j - PHI_jminus1)^2)
        phi_j <- dnorm(alpha[j + 1])
        phi_jminus1 <- dnorm(alpha[j])
        PHI_j <- pnorm(alpha[j + 1])
        PHI_jminus1 <- pnorm(alpha[j])
        term1 <- alpha[j] * phi_jminus1
        term2 <- alpha[j + 1] * phi_j
        if (is.na(term1)) {
          term1 <- 0
        }
        if (is.na(term2)) {
          term2 <- 0
        }
        d2ldalpha2[k, l] <- d2ldalpha2[k, l] + n[2, j] *
          ((PHI_j - PHI_jminus1) * (jk1 * jl1 * term1 - jk * jl * term2)) /
          ((PHI_j - PHI_jminus1)^2)
        d2ldalpha2[k, l] <- d2ldalpha2[k, l] - n[2, j] *
          ((jk * phi_j - jk1 * phi_jminus1) * (jl * phi_j - jl1 * phi_jminus1)) /
          ((PHI_j - PHI_jminus1)^2)
      }
    }
  }
  return(d2ldalpha2)
}
