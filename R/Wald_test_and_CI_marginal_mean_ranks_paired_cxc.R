#' @title The Wald test and confidence interval for the difference between
#' marginal mean ranks / ridits
#' @description The Wald test and confidence interval for the difference between
#' marginal mean ranks / ridits
#' @description Described in Chapter 9 "The Paired cxc Table"
#' @param n the observed table (a cxc matrix)
#' @param alpha the nominal level, e.g. 0.05 for 95% CIs
#' @examples
#' # A comparison between serial and retrospective measurements
#' # (Fischer et al., 1999)
#' Wald_test_and_CI_marginal_mean_ranks_paired_cxc(fischer_1999)
#' @export
#' @return An object of the [contingencytables_result] class,
#' basically a subclass of [base::list()]. Use the [utils::str()] function
#' to see the specific elements returned.
Wald_test_and_CI_marginal_mean_ranks_paired_cxc <- function(n, alpha = 0.05) {
  validateArguments(mget(ls()))

  c <- nrow(n)
  N <- sum(n)
  nip <- apply(n, 1, sum)
  npi <- apply(n, 2, sum)

  # The midranks for each category
  r <- rep(0, c)
  for (i in 1:c) {
    if (i > 1) {
      for (j in 1:(i - 1)) {
        r[i] <- r[i] + nip[j] + npi[j]
      }
    }
    r[i] <- r[i] + 0.5 * (1 + nip[i] + npi[i])
  }

  # The ridits
  a <- matrix(0, 2, c)
  for (i in 1:c) {
    if (i > 1) {
      a[1, i] <- sum(nip[1:(i - 1)] / N) + 0.5 * nip[i] / N
      a[2, i] <- sum(npi[1:(i - 1)] / N) + 0.5 * npi[i] / N
    } else {
      a[1, i] <- 0.5 * nip[i] / N
      a[2, i] <- 0.5 * npi[i] / N
    }
  }

  # Estimates of tau and alpha
  tauhat <- sum(npi * a[1, ] / N) - sum(nip * a[2, ] / N)
  alphahat <- sum(npi * a[1, ] / N)

  phihat <- matrix(0, c, c)
  for (i in 1:c) {
    for (j in 1:c) {
      phihat[i, j] <- 2 * (a[2, j] - a[1, i])
    }
  }

  # The upper alpha / 2 percentile of the standard normal distribution
  z <- qnorm(1 - alpha / 2, 0, 1)

  # Inference for tau
  SE_tau <- sqrt(
    (sum(sum((phihat^2) * n / N)) - (sum(sum(phihat * n / N)))^2) / N
  )
  CI_tau <- c(tauhat - z * SE_tau, tauhat + z * SE_tau)
  Z_Wald <- tauhat / SE_tau
  P <- 2 * (1 - pnorm(abs(Z_Wald), 0, 1))

  # Inference for alpha
  SE_alpha <- SE_tau / 2
  CI_alpha <- c(alphahat - z * SE_alpha, alphahat + z * SE_alpha)


  # Inference for alpha on the logit scale
  l <- log(
    alphahat / (1 - alphahat)) - z * SE_alpha / (alphahat * (1 - alphahat)
  )
  u <- log(
    alphahat / (1 - alphahat)) + z * SE_alpha / (alphahat * (1 - alphahat)
  )
  CI_alpha_logit <- c(exp(l) / (1 + exp(l)), exp(u) / (1 + exp(u)))
  Z_Wald_logit <- (alphahat * (1 - alphahat) * log(alphahat / (1 - alphahat))) /
    SE_alpha
  P_logit <- 2 * (1 - pnorm(abs(Z_Wald_logit), 0, 1))

  # Inference for tau on the logit scale
  CI_tau_logit <- c(2 * CI_alpha_logit[1] - 1, 2 * CI_alpha_logit[2] - 1)

  results <- list()
  results$tauhat <- tauhat
  results$SE_tau <- SE_tau
  results$CI_tau <- CI_tau
  results$Z_Wald <- Z_Wald
  results$P <- P
  results$alphahat <- alphahat
  results$SE_alpha <- SE_alpha
  results$CI_alpha <- CI_alpha
  results$CI_alpha_logit <- CI_alpha_logit
  results$Z_Wald_logit <- Z_Wald_logit
  results$P_logit <- P_logit
  results$CI_tau_logit <- CI_tau_logit

  printresults <- function() {
    common_part <- "%6.4f (%g%% CI %6.4f to %6.4f); P = %7.5f, Z = %6.3f"
    cat("\nInference for tau\n-----------------\n")
    cat(
      sprintf(
        paste("Wald:       estimate =", common_part, "\n"),
        tauhat, 100 * (1 - alpha), CI_tau[1], CI_tau[2], P, Z_Wald
      )
    )
    cat(
      sprintf(
        paste("Wald logit: estimate =", common_part, "\n"),
        tauhat, 100 * (1 - alpha), CI_tau_logit[1], CI_tau_logit[2], P_logit,
        Z_Wald_logit
      )
    )
    cat("\nInference for alpha\n-------------------\n")
    cat(
      sprintf(
        paste("Wald:       estimate =", common_part, "\n"),
        alphahat, 100 * (1 - alpha), CI_alpha[1], CI_alpha[2], P, Z_Wald
      )
    )
    cat(
      sprintf(
        paste("Wald logit: estimate =", common_part),
        alphahat, 100 * (1 - alpha), CI_alpha_logit[1], CI_alpha_logit[2],
        P_logit, Z_Wald_logit
      )
    )
  }

  return(contingencytables_result(results, printresults))
}
