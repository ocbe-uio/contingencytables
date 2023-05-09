#' @title The Brant test for the proportional odds assumption
#' @description The Brant test for the proportional odds assumption
#' @description Described in Chapter 6 "The Ordered 2xc Table"
#' @param n the observed table (a 2xc matrix)
#' @importFrom stats binomial glm predict
#' @return An object of the [contingencytables_result] class,
#' basically a subclass of [base::list()]. Use the [utils::str()] function
#' to see the specific elements returned.
#' @examples
#' Brant_test_2xc(fontanella_2008)
#' Brant_test_2xc(lydersen_2012a)
#' @export
Brant_test_2xc <- function(n) {
  validateArguments(mget(ls()))
  # Note that this function only works for 2xc tables (not for rxc tables)
  r0 <- nrow(n)
  c0 <- ncol(n)
  N <- sum(n)

  # Build dataset suitable for estimation
  x <- rep(0, N)
  y <- rep(0, N)
  z <- matrix(0, N, c0 - 1)
  id <- 1
  for (i in 1:r0) {
    for (j in 1:c0) {
      x[id:(id + n[i, j] - 1)] <- i - 1
      y[id:(id + n[i, j] - 1)] <- j
      for (k in 1:(c0 - 1)) {
        z[id:(id + n[i, j] - 1), k] <- ifelse(j > k, 1, 0)
      }
      id <- id + n[i, j]
    }
  }

  X <- cbind(1, x)

  # Run c0-1 binary logistic regressions
  pihat <- matrix(0, N, c0 - 1)
  betas <- rep(0, c0 - 1)
  for (i in 1:(c0 - 1)) {
    tmp <- glm(factor(z[, i]) ~ x, family = binomial("logit"))
    pihat[, i] <- predict(tmp, type = "response")
    betas[i] <- tmp$coef[2]
  }


  # Find the covariance between the betas
  w <- array(0, dim = c(N, c0 - 1, c0 - 1))
  for (i in 1:N) {
    for (m in 1:(c0 - 1)) {
      for (l in 1:(c0 - 1)) {
        if (m <= l) {
          w[i, m, l] <- pihat[i, l] - pihat[i, m] * pihat[i, l]
        }
      }
    }
  }

  covar <- matrix(0, c0 - 1, c0 - 1) ## ORIGINAL MATLAB CODE NOT SAFE : zeros(m,l)
  for (m in 1:(c0 - 1)) {
    for (l in 1:(c0 - 1)) {
      if (m <= l) {
        Wmm <- diag(w[, m, m])
        Wml <- diag(w[, m, l])
        Wll <- diag(w[, l, l])
        matrix2x2 <- solve(t(X) %*% Wmm %*% X) %*% (t(X) %*% Wml %*% X) %*% solve(t(X) %*% Wll %*% X)
        covar[m, l] <- matrix2x2[2, 2]
        if (m < l) {
          covar[l, m] <- covar[m, l]
        }
      }
    }
  }


  D <- matrix(0, c0 - 2, c0 - 1)
  D[, 1] <- 1
  for (i in 1:(c0 - 2)) {
    D[i, i + 1] <- -1
  }


  # The Brant test statistic is a Wald-type statistic
  T0 <- t(D %*% betas) %*% solve(D %*% covar %*% t(D)) %*% (D %*% betas)
  df <- c0 - 2
  P0 <- 1 - pchisq(T0, df)

  # Output
  printresults <- function() {
    my_sprintf_cat(
      "Brant test: P = %7.6f, T = %5.3f (df = %g)", P0, T0, df
    )
  }
  return(
    contingencytables_result(
      list("pvalue" = P0, "T" = T0, "df" = df),
      printresults
    )
  )
}
