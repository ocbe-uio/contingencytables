#' @title Cumulative logit and probit models
#' @description Cumulative logit and probit models
#' @description Described in Chapter 7 "The rxc Table"
#' @param n the observed table (an rxc matrix)
#' @param linkfunction either "logit" or "probit"
#' @param alpha the nominal level, e.g. 0.05 for 95% CIs
#' @param printresults display results (FALSE = no, TRUE = yes)
#' @examples
#' # Psychiatric diag. vs BMI with hyperkinetic disorders
#' # as reference category (Table 7.5)
#' n <- rbind(
#'   c(19, 130, 64), c(3, 55, 23), c(8, 102, 36), c(6, 14, 1), c(5, 21, 12), c(7, 26, 18)
#' )
#' Cumulative_models_for_rxc(n)
#'
#' # LBW vs psych. morbidity with control as reference category (Table 7.6)
#' n <- rbind(c(51, 7, 6), c(22, 4, 12), c(24, 9, 10))
#' Cumulative_models_for_rxc(n)
#' @export
#' @return A list containing the results of statistical tests for the
#' goodness-of-fit of a proportional odds model, the effect in a proportional
#' odds model and row comparisons
Cumulative_models_for_rxc <- function(n, linkfunction = "logit", alpha = 0.05, printresults = TRUE) {
  r <- nrow(n)
  c <- ncol(n)
  nip <- apply(n, 1, sum)
  N <- sum(n)

  # Build dataset suitable for Matlab's mnrfit command
  x <- matrix(0, N, r - 1)
  y <- rep(0, N)
  id <- 1
  for (i in 1:r) {
    for (j in 1:c) {
      for (row in 2:r) {
        if (n[i, j] > 0) {
          x[id:(id + n[i, j] - 1), row - 1] <- ifelse(i == row, 1, 0)
        }
      }
      if (n[i, j] > 0) {
        y[id:(id + n[i, j] - 1)] <- j
      }
      id <- id + n[i, j]
    }
  }
  if (max(y) == 1) {
    results <- list()
    return(invisible(results))
  }

  # Fit model
  dat <- data.frame(x = x, y = factor(y))
  .dat001 <- dat
  if (identical(linkfunction, "logit")) {
    tmp <- polr(y ~ ., method = "logistic", Hess = TRUE, data = .dat001)
  } else if (identical(linkfunction, "probit")) {
    tmp <- polr(y ~ ., method = "probit", Hess = TRUE, data = .dat001)
  }
  beta <- c(tmp$zeta, -tmp$coef)
  L1 <- tmp$deviance
  stats.se <- summary(tmp)$coef[, 2]
  stats.se <- stats.se[names(beta)] # Important - order differs in beta and stats.se

  # Calculate estimated probabilities
  xx <- matrix(0, r, r - 1)
  for (i in 1:(r - 1)) {
    xx[i + 1, i] <- 1
  }
  dat <- data.frame(x = xx)
  .dat002 <- dat
  pihat <- predict(tmp, newdata = .dat002, type = "probs")

  # Calculate expected values
  m <- matrix(0, r, c)
  for (i in 1:r) {
    m[i, ] <- nip[i] * pihat[i, ]
  }

  # Calculate the deviance (D) and Pearson (X2) statistics
  D <- 0
  residuals <- matrix(0, r, c)
  for (i in 1:r) {
    for (j in 1:c) {
      if (m[i, j] > 0) {
        if (n[i, j] > 0) {
          D <- D + n[i, j] * log(n[i, j] / m[i, j])
        }
        residuals[i, j] <- (n[i, j] - m[i, j]) / sqrt(m[i, j])
      }
    }
  }
  D <- 2 * D
  X2 <- sum(residuals^2)
  df_D <- (r - 1) * (c - 2)
  df_X2 <- (r - 1) * (c - 2)
  P_D <- 1 - pchisq(D, df_D)
  P_X2 <- 1 - pchisq(X2, df_X2)

  # Wald tests and Wald CIs for the betas
  betahat <- rep(0, r - 1)
  se <- rep(0, r - 1)
  Z_Wald <- rep(0, r - 1)
  T_Wald <- rep(0, r - 1)
  P_Wald <- rep(0, r - 1)
  Wald_CI <- matrix(0, r - 1, 2)
  Wald_CI_OR <- matrix(0, r - 1, 2)
  Wald_CI_width <- rep(0, r - 1)
  z <- qnorm(1 - alpha / 2, 0, 1)
  for (i in 1:(r - 1)) {
    betahat[i] <- -beta[c - 1 + i]
    se[i] <- stats.se[c - 1 + i]
    Z_Wald[i] <- betahat[i] / se[i]
    T_Wald[i] <- Z_Wald[i]^2
    P_Wald[i] <- 2 * (1 - pnorm(abs(Z_Wald[i]), 0, 1))
    Wald_CI[i, ] <- c(betahat[i] - z * se[i], betahat[i] + z * se[i])
    Wald_CI_OR[i, ] <- exp(Wald_CI[i, ])
    Wald_CI_width[i] <- abs(Wald_CI[i, 2] - Wald_CI[i, 1])
  }

  # Likelihood ratio test for beta
  if (identical(linkfunction, "logit")) {
    tmp <- polr(y ~ 1, method = "logistic", data = .dat001)
  } else if (identical(linkfunction, "probit")) {
    tmp <- polr(y ~ 1, method = "probit", data = .dat001)
  }
  L0 <- tmp$deviance
  T_LR <- L0 - L1
  df_LR <- r - 1
  P_LR <- 1 - pchisq(T_LR, df_LR)


  # Output arguments
  results <- list()
  results$betahat <- betahat
  results$OR <- exp(betahat)
  results$se <- se
  results$D <- D
  results$df_D <- df_D
  results$P_D <- P_D
  results$X2 <- X2
  results$df_X2 <- df_X2
  results$P_X2 <- P_X2
  results$Z_Wald <- Z_Wald
  results$T_Wald <- T_Wald
  results$P_Wald <- P_Wald
  results$T_LR <- T_LR
  results$df_LR <- df_LR
  results$P_LR <- P_LR
  results$Wald_CI <- Wald_CI
  results$Wald_CI_OR <- Wald_CI_OR
  results$Wald_CI_width <- Wald_CI_width


  if (printresults) {
    if (identical(linkfunction, "logit")) {
      model <- "proportional odds"
    } else if (identical(linkfunction, "probit")) {
      model <- "probit"
    }
    .print("\nTesting the fit of a %s model\n", model)
    .print("  Pearson goodness of fit:     P = %8.5f, X2 = %6.3f (df=%g)\n", P_X2, X2, df_X2)
    .print("  Likelihodd ratio (deviance): P = %8.5f, D  = %6.3f (df=%g)\n", P_D, D, df_D)

    .print("\nTesting the effect in a %s model\n", model)
    .print("  Likelihood ratio             P = %8.5f, T = %6.3f (df=%g)\n", P_LR, T_LR, df_LR)

    .print("\nComparing the rows                  Statistic   P-value\n")
    .print("--------------------------------------------------------\n")
    for (i in 1:(r - 1)) {
      .print("Wald (Z-statistic) row %g vs row 1    %6.3f    %9.6f\n", i + 1, Z_Wald[i], P_Wald[i])
    }
    .print("--------------------------------------------------------\n\n")

    .print("Comparing the rows     Estimate (%g%% Wald CI)     Odds ratio (%g%% Wald CI)\n", 100 * (1 - alpha), 100 * (1 - alpha))
    .print("--------------------------------------------------------------------------\n")
    for (i in 1:(r - 1)) {
      .print("row %g vs row 1:      %6.3f (%6.3f to %6.3f)     %5.3f (%5.3f to %5.3f)\n", i + 1, betahat[i], Wald_CI[i, 1], Wald_CI[i, 2], exp(betahat[i]), Wald_CI_OR[i, 1], Wald_CI_OR[i, 2])
    }
    .print("--------------------------------------------------------------------------\n")
  }

  invisible(results)
}

.print <- function(s, ...) {
  print(sprintf(gsub("\n", "", s), ...), quote = FALSE)
}
