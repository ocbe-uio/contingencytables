#' @title The Brant test for the proportional odds assumption
#' @description The Brant test for the proportional odds assumption
#' @description Described in Chapter 6 "The Ordered 2xc Table"
#' @param n the observed table (a 2xc matrix)
#' @param printresults display results (FALSE = no, TRUE = yes)
#' @importFrom stats binomial glm predict
#' @return A data frame containing the probability, the statistic and the degrees of freedom
#' @examples
#' # The Adolescent Placement Study (Fontanella et al., 2008)
#' n <- rbind(c(8, 28, 72, 126), c(46, 73, 69, 86))
#' Brant_test_2xc(n)
#'
#' # Postoperative nausea (Lydersen et al., 2012a)
#' n <- rbind(c(14, 10, 3, 2), c(11, 7, 8, 4))
#' Brant_test_2xc(n)
#'
#' @export
Brant_test_2xc <- function(n, printresults = TRUE) {
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

  if (printresults) {
    print(sprintf("Brant test: T = %6.3f, df = %g, P = %7.5f", T0, df, P0), quote = FALSE)
  }

  invisible(data.frame(P = P0, T = T0, df = df))
}
