#' @title Trend estimate for linear and logit models
#' @description Trend estimate for linear and logit models
#' \itemize{
#'  \item The Wald test and CI
#'  \item Likelihood ratio test
#'  \item The Pearson goodness-of-fit test
#'  \item Likelihood ratio (deviance) goodness-of-fit test
#' }
#' @description Described in Chapter 5 "The Ordered rx2 Table"
#' @param n the observed counts (an rx2 matrix)
#' @param a scores assigned to the rows
#' @param alpha the nominal level, e.g. 0.05 for 95% CIs
#' @param linkfunction Link function for the binomial distribution see
#' \code{?family} for more details
#' @examples
#' # Alcohol consumption and malformations (Mills and Graubard, 1987)
#' Trend_estimate_CI_tests_rx2(mills_graubard_1987, 1:5)
#'
#' # levated troponin T levels in stroke patients (Indredavik et al., 2008)
#' Trend_estimate_CI_tests_rx2(indredavik_2008, 1:5)
#' @export
#' @return An object of the [contingencytables_result] class,
#' basically a subclass of [base::list()]. Use the [utils::str()] function
#' to see the specific elements returned.
Trend_estimate_CI_tests_rx2 <- function(
  n, a, linkfunction = "logit", alpha = 0.05
) {
  validateArguments(mget(ls()))

  nip <- apply(n, 1, sum)

  # Fit model and get estimat of the trend (beta) and its standard error
  mdl1 <- glm(
    cbind(n[, 1], n[, 2]) ~ 1 + a, family = binomial(link = linkfunction)
  )
  L1 <- -mdl1$deviance / 2
  betahat <- mdl1$coefficients[2]
  SEhat <- summary(mdl1)$coefficients[2, 2]

  # Fit constant-only model
  L0 <- -mdl1$null.deviance / 2

  # The Wald test
  Z_Wald <- betahat / SEhat
  P_Wald <- 2 * (1 - pnorm(abs(Z_Wald), 0, 1))

  # The likelihood ratio test
  T_LR <- -2 * (L0 - L1)
  df_LR <- 1
  if (is.na(T_LR)) {
    P_LR <- 1.0
  } else {
    P_LR <- 1 - pchisq(T_LR, df_LR)
  }

  # The Wald confidence interval
  z <- qnorm(1 - alpha / 2, 0, 1)
  CI_Wald <- c(betahat - z * SEhat, betahat + z * SEhat)

  # The Pearson goodness-of-fit test
  m <- mdl1$fitted.values * nip
  m <- c(m, nip - m)
  chi2 <- sum(((n - m)^2) / m)
  df_chi2 <- length(a) - 2
  if (is.na(chi2)) {
    P_chi2 <- 1.0
  } else {
    P_chi2 <- 1 - pchisq(chi2, df_chi2)
  }

  # The likelihood ratio (deviance) test
  D <- mdl1$deviance
  df_D <- length(a) - 2
  if (is.na(D)) {
    P_D <- 1.0
  } else {
    P_D <- 1 - pchisq(D, df_D)
  }

  # Output arguments (estimated trend, observed statistics, degrees of freedom,
  # P-values, and CI)
  results <- list()
  results$betahat <- betahat

  results$Z_Wald <- Z_Wald
  results$P_Wald <- P_Wald

  results$T_LR <- T_LR
  results$P_LR <- P_LR
  results$df_LR <- df_LR

  results$chi2 <- chi2
  results$P_chi2 <- P_chi2
  results$df_chi2 <- df_chi2

  results$D <- D
  results$P_D <- P_D
  results$df_D <- df_D

  results$CI_Wald <- CI_Wald
  results$CI_Wald_width <- CI_Wald[2] - CI_Wald[1]


  printresults <- function() {
    my_sprintf_cat(
        "Wald test:                    P = %7.5f, T = %5.3f\n", P_Wald, Z_Wald
    )
    my_sprintf_cat(
        "Likelihood ratio test:        P = %7.5f, T = %5.3f (df = %i)\n",
        P_LR, T_LR, df_LR
    )
    my_sprintf_cat(
        "Pearson goodness-of-fit test: P = %7.5f, T = %5.3f (df = %i)\n",
        P_chi2, chi2, df_chi2
    )
    my_sprintf_cat(
        "LR (deviance) test:           P = %7.5f, T = %5.3f (df = %i)\n",
        P_D, D, df_D
    )
    my_sprintf_cat(
      paste(
        "Trend estimate and Wald CI:   betahat =",
        "%6.4f (%g%% CI %6.4f to %6.4f)"
      ),
      betahat, 100 * (1 - alpha), CI_Wald[1], CI_Wald[2]
    )
  }

  return(contingencytables_result(results, printresults))
}
