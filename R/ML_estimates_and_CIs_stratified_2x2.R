#' @title Maximum likelihood estimates with CIs of the grouping and strata effects
#' @description Maximum likelihood estimates with CIs of the grouping and strata effects
#' @description Described in Chapter 10 "Stratified 2x2 Tables and Meta-Analysis"
#' @param n the observed table (a 2x2xk matrix, where k is the number of strata)
#' @param link the link function ('linear', 'log', or 'logit')
#' @param alpha the nominal level, e.g. 0.05 for 95% CIs
#' @examples
#' # Smoking and lung cancer (Doll and Hill, 1950)
#' ML_estimates_and_CIs_stratified_2x2(doll_hill_1950)
#'
#' # Prophylactice use of Lidocaine in myocardial infarction (Hine et al., 1989)
#' ML_estimates_and_CIs_stratified_2x2(hine_1989)
#'
#' @export
#' @importFrom stats coef
#' @return An object of the [contingencytables_result] class,
#' basically a subclass of [base::list()]. Use the [utils::str()] function
#' to see the specific elements returned.
ML_estimates_and_CIs_stratified_2x2 <- function(n, link = "log", alpha = 0.05) {
  validateArguments(mget(ls()))

  N <- sum(n)
  K <- dim(n)[3]
  if (identical(link, "linear")) {
    link <- "identity"
  }

  # Build dataset suitable for Matlab's glmfit command
  x <- matrix(0, N, K)
  y <- rep(0, N)
  id <- 1
  for (k in 1:K) {
    for (i in 1:2) {
      for (j in 1:2) {
        x[id:(id + n[i, j, k] - 1), 1] <- (i == 1)
        if (k > 1) {
          x[id:(id + n[i, j, k] - 1), k] <- 1
        }
        y[id:(id + n[i, j, k] - 1)] <- 2 - j
        id <- id + n[i, j, k]
      }
    }
  }

  # Fit the general model with only main effects
  glm.dat <- data.frame(y = y, x = x)
  glm.res <- glm(y ~ 1 + ., family = binomial(link = link), data = glm.dat)
  alphaandbetahat <- coef(glm.res)
  stats.se <- summary(glm.res)$coef[, 2]

  # Calculate the estimated cell probabilities
  x <- diag(rep(1, k))
  pihat <- array(0, dim = c(2, 2, K))
  for (i in 1:2) {
    x[, 1] <- (2 - i) * rep(1, k)
    dat <- data.frame(x = x)
    pihat[i, 1, ] <- predict(glm.res, newdata = dat, type = "response")
    pihat[i, 2, ] <- 1 - pihat[i, 1, ]
  }

  # The upper alpha / 2 percentile of the standard normal distribution
  z_alpha <- qnorm(1 - alpha / 2, 0, 1)

  # The confidence intervals
  L <- alphaandbetahat - z_alpha * stats.se
  U <- alphaandbetahat + z_alpha * stats.se

  results <- list()
  results$alphahat <- as.vector(alphaandbetahat[1])
  results$alphahatSE <- as.vector(stats.se[1])
  results$alphahatCI <- as.vector(c(L[1], U[1]))
  results$betahat <- as.vector(alphaandbetahat[2])
  results$betahatSE <- as.vector(stats.se[2])
  results$betahatCI <- as.vector(c(L[2], U[2]))
  results$gammahat <- as.vector(alphaandbetahat[-c(1, 2)])
  results$gammahatSE <- as.vector(stats.se[-c(1, 2)])
  results$gammahatCI <- cbind(L[-c(1, 2)], U[-c(1, 2)])
  rownames(results$gammahatCI) <- rep("", nrow(results$gammahatCI))
  results$pihat <- pihat

  printresults <- function() {
    my_sprintf_cat("Maximum likelihood estimates:\n")
    my_sprintf_cat("  alphahat   = %7.4f (%g%% CI %6.4f to %6.4f)\n", alphaandbetahat[1], 100 * (1 - alpha), L[1], U[1])
    my_sprintf_cat("  betahat    = %7.4f (%g%% CI %6.4f to %6.4f)\n", alphaandbetahat[2], 100 * (1 - alpha), L[2], U[2])
    for (k in 2:K) {
      my_sprintf_cat("  gammahat_%i = %7.4f (%g%% CI %6.4f to %6.4f)\n", k, alphaandbetahat[k + 1], 100 * (1 - alpha), L[k + 1], U[k + 1])
    }
  }

  return(contingencytables_result(results, printresults))
}
