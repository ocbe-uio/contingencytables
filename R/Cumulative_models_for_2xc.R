#' @title Cumulative logit and probit models
#' @description Cumulative logit and probit models
#' @description Described in Chapter 6 "The Ordered 2xc Table"
#' @param  n the observed table (a 2xc matrix)
#' @param  linkfunction either "logit" or "probit"
#' @param  alpha the nominal level, e.g. 0.05 for 95% CIs
#' @importFrom MASS polr
#' @importFrom stats binomial glm predict
#' @examples
#' Cumulative_models_for_2xc(fontanella_2008)
#' Cumulative_models_for_2xc(lydersen_2012a)
#' @export
#' @return An object of the [contingencytables_result] class,
#' basically a subclass of [base::list()]. Use the [utils::str()] function
#' to see the specific elements returned.
Cumulative_models_for_2xc <- function(n, linkfunction = "logit", alpha = 0.05) {
  validateArguments(mget(ls()))
  c0 <- ncol(n)
  nip <- apply(n, 1, sum)
  npj <- apply(n, 2, sum)
  N <- sum(n)

  # Build dataset
  x <- rep(0, N)
  y <- rep(0, N)
  id <- 1
  for (i in 1:2) {
    for (j in 1:c0) {
      x[id:(id + n[i, j] - 1)] <- ifelse(i == 1, 1, 0)
      y[id:(id + n[i, j] - 1)] <- j
      id <- id + n[i, j]
    }
  }

  #  If there should be only one outcome category
  if (max(y) == 1) {
    results <- list()
    return(invisible(results))
  }

  #  Fit the model
  dat <- data.frame(x = x, y = factor(y))
  .dat001 <- dat
  if (identical(linkfunction, "logit")) {
    tmp <- polr(y ~ x, method = "logistic", Hess = TRUE, data = .dat001)
  } else if (identical(linkfunction, "probit")) {
    tmp <- polr(factor(y) ~ x, method = "probit", Hess = TRUE, data = .dat001)
  }

  # Extract from model
  beta <- c(tmp$zeta, -tmp$coef)
  L1 <- tmp$deviance
  pihat <- predict(tmp, newdata = data.frame(x = c(1, 0)), type = "probs")

  #  Handle cases with missing outcome categories
  if (ncol(pihat) == c0 - 2) {
    pihat <- cbind(pihat, matrix(0, 2, 2))
  } else if (ncol(pihat) == c0 - 1) {
    pihat <- cbind(pihat, 0)
  }

  #  Calculate expected values
  m <- matrix(0, 2, c0)
  m[1, ] <- nip[1] * pihat[1, ]
  m[2, ] <- nip[2] * pihat[2, ]

  #  Calculate the deviance (D) and Pearson (X2) goodness-of-fit tests
  D <- 0
  r <- matrix(0, 2, c0)
  for (i in 1:2) {
    for (j in 1:c0) {
      if (m[i, j] > 0) {
        if (n[i, j] > 0) {
          D <- D + n[i, j] * log(n[i, j] / m[i, j])
        }
        r[i, j] <- (n[i, j] - m[i, j]) / sqrt(m[i, j])
      }
    }
  }
  D <- 2 * D
  X2 <- sum(r^2)
  df_D <- c0 - 2
  df_X2 <- c0 - 2
  P_D <- 1 - pchisq(D, df_D)
  P_X2 <- 1 - pchisq(X2, df_X2)

  #  Wald test for beta
  betahat <- -beta[length(beta)]
  se <- summary(tmp)$coef[1, 2]
  Z_Wald <- betahat / se
  T_Wald <- Z_Wald^2
  P_Wald <- 2 * (1 - pnorm(abs(Z_Wald), 0, 1))

  #  Wald confidence interval for beta
  z <- qnorm(1 - alpha / 2, 0, 1)
  Wald_CI <- c(betahat - z * se, betahat + z * se)
  Wald_CI_width <- abs(Wald_CI[2] - Wald_CI[1])

  #  LR test for beta
  # [~, L0, ~] = mnrfit([], y, 'model', 'ordinal', 'link', linkfunction);
  if (identical(linkfunction, "logit")) {
    tmp <- polr(y ~ 1, method = "logistic", Hess = TRUE, data = .dat001)
  } else if (identical(linkfunction, "probit")) {
    tmp <- polr(y ~ 1, method = "probit", Hess = TRUE, data = .dat001)
  }
  L0 <- tmp$deviance
  T_LR <- L0 - L1
  df_LR <- 1
  P_LR <- 1 - pchisq(T_LR, df_LR)

  #  Calculate the midranks
  midranks <- rep(0, c0)
  for (j in 1:c0) {
    a0 <- ifelse(j > 1, sum(npj[1:(j - 1)]), 0)
    b0 <- 1 + sum(npj[1:j])
    midranks[j] <- 0.5 * (a0 + b0)
  }

  #  The score test (linear rank) a.k.a. Wilcoxon-Mann-Whitney
  if (linkfunction == "logit") {
    W <- sum(n[1, ] * midranks) # The Wilcoxon form of the WMW statistic
    Exp_W <- 0.5 * nip[1] * (N + 1)
    Var_W <- (nip[1] * nip[2] * (N + 1)) / 12
    tieadj <- (nip[1] * nip[2] * sum(npj^3 - npj)) / (12 * N * (N - 1))
    Var_W <- Var_W - tieadj
    Z_MW <- (W - Exp_W) / sqrt(Var_W)
    P_MW <- 2 * (1 - pnorm(abs(Z_MW), 0, 1))
  }

  #  Output arguments
  results <- list()
  results$betahat <- betahat
  results$OR <- exp(-betahat)
  results$se <- se
  results$D <- D
  results$P_D <- P_D
  results$df_D <- df_D
  results$X2 <- X2
  results$P_X2 <- P_X2
  results$df_X2 <- df_X2
  results$Z_Wald <- Z_Wald
  results$T_Wald <- T_Wald
  results$P_Wald <- P_Wald
  results$T_LR <- T_LR
  results$P_LR <- P_LR
  results$df_LR <- df_LR
  if (linkfunction == "logit") {
    results$Z_MW <- Z_MW
    results$P_MW <- P_MW
  }
  results$Wald_CI <- Wald_CI
  results$Wald_CI_OR <- c(exp(-Wald_CI[2]), exp(-Wald_CI[1]))
  results$Wald_CI_width <- Wald_CI_width

  # Output
  statistics <- list(
    "linkfunction" = linkfunction,
    "P_X2" = P_X2, "X2" = X2, "df_X2" = df_X2,
    "P_D" = P_D, "D" = D, "df_D" = df_D,
    "P_Wald" = P_Wald, "Z_Wald" = Z_Wald,
    "P_LR" = P_LR, "T_LR" = T_LR, "df_LR" = df_LR,
    "P_MW" = ifelse(linkfunction == "logit", P_MW, NA),
    "Z_MW" = ifelse(linkfunction == "logit", Z_MW, NA),
    "alpha" = alpha,
    "betahat" = betahat, "Wald_CI" = Wald_CI, "Wald_CI_width" = Wald_CI_width,
    "OR" = exp(-betahat),
    "Wald_CI_OR" = c(exp(-Wald_CI[2]), exp(-Wald_CI[1]))
  )
  print_fun <- function() {
    if (identical(linkfunction, "logit")) {
      model <- "proportional odds"
    } else if (identical(linkfunction, "probit")) {
      model <- "probit"
    }
    my_sprintf_cat("\nTesting the fit of a %s model\n", model)
    my_sprintf_cat("  Pearson goodness of fit:     P = %8.5f, X2 = %6.3f (df=%g)\n", P_X2, X2, df_X2)
    my_sprintf_cat("  Likelihodd ratio (deviance): P = %8.5f, D  = %6.3f (df=%g)\n", P_D, D, df_D)
    my_sprintf_cat("\nTesting the effect in a %s model\n", model)
    my_sprintf_cat("  Wald (Z-statistic):          P = %8.5f, Z = %6.3f\n", P_Wald, Z_Wald)
    my_sprintf_cat("  Likelihood ratio:            P = %8.5f, T = %6.3f (df=%g)\n", P_LR, T_LR, df_LR)
    if (linkfunction %in% c("logistic", "logit")) {
      my_sprintf_cat("  Score (WMW):                 P = %8.5f, Z = %6.3f\n", P_MW, Z_MW)
    }

    my_sprintf_cat("\nEstimation of the effect parameter beta with %g%% CIs\n", 100 * (1 - alpha))
    my_sprintf_cat("in the %s model\n", model)
    my_sprintf_cat("----------------------------------------------------\n")
    my_sprintf_cat("Interval         Estimate     Conf. int       Width\n")
    my_sprintf_cat("----------------------------------------------------\n")
    my_sprintf_cat("  Wald           %6.3f    %6.3f to %6.3f   %6.4f\n", betahat, Wald_CI[1], Wald_CI[2], Wald_CI_width)
    if (linkfunction == "logistic") {
      my_sprintf_cat("  Wald (OR)      %6.3f    %6.3f to %6.3f\n", exp(-betahat), exp(-Wald_CI[2]), exp(-Wald_CI[1]))
    }
    my_sprintf_cat("----------------------------------------------------\n")
  }
  return(contingencytables_result(statistics, print_fun))
}
