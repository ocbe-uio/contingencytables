#' @title Maximum likelihood estimates with CIs of the grouping and strata effects
#' @description Maximum likelihood estimates with CIs of the grouping and strata effects
#' @description Described in Chapter 10 "Stratified 2x2 Tables and Meta-Analysis"
#' @param n the observed table (a 2x2xk matrix, where k is the number of strata)
#' @param link the link function ('linear', 'log', or 'logit')
#' @param alpha the nominal level, e.g. 0.05 for 95% CIs
#' @param printresults display results (FALSE = no, TRUE = yes)
#' @examples
#' # Smoking and lung cancer (Doll and Hill, 1950)
#' n <- array(dim = c(2, 2, 2))
#' n[, , 1] <- matrix(c(647, 622, 2, 27), 2, byrow = TRUE)
#' n[, , 2] <- matrix(c(41, 28, 19, 32), 2, byrow = TRUE)
#' ML_estimates_and_CIs_stratified_2x2(n)
#'
#' # Prophylactice use of Lidocaine in myocardial infarction (Hine et al., 1989)
#' n <- array(0, dim = c(2, 2, 6))
#' n[, , 1] <- rbind(c(2, 37), c(1, 42))
#' n[, , 2] <- rbind(c(4, 40), c(4, 40))
#' n[, , 3] <- rbind(c(6, 101), c(4, 106))
#' n[, , 4] <- rbind(c(7, 96), c(5, 95))
#' n[, , 5] <- rbind(c(7, 103), c(3, 103))
#' n[, , 6] <- rbind(c(11, 143), c(4, 142))
#' ML_estimates_and_CIs_stratified_2x2(n)
#'
#' @export
#' @importFrom stats coef
#' @return A list containing the maximum likelihood estimates
ML_estimates_and_CIs_stratified_2x2 <- function(n, link = "log", alpha = 0.05, printresults = TRUE) {
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
  results$gammahat <- as.vector(alphaandbetahat[-c(1:2)])
  results$gammahatSE <- as.vector(stats.se[-c(1:2)])
  results$gammahatCI <- cbind(L[-c(1:2)], U[-c(1:2)])
  rownames(results$gammahatCI) <- rep("", nrow(results$gammahatCI))
  results$pihat <- pihat

  if (printresults) {
    .print("Maximum likelihood estimates:\n")
    .print("  alphahat   = %7.4f (%g%% CI %6.4f to %6.4f)\n", alphaandbetahat[1], 100 * (1 - alpha), L[1], U[1])
    .print("  betahat    = %7.4f (%g%% CI %6.4f to %6.4f)\n", alphaandbetahat[2], 100 * (1 - alpha), L[2], U[2])
    for (k in 2:K) {
      .print("  gammahat_%i = %7.4f (%g%% CI %6.4f to %6.4f)\n", k, alphaandbetahat[k + 1], 100 * (1 - alpha), L[k + 1], U[k + 1])
    }
  }

  invisible(results)
}


.print <- function(s, ...) {
  print(sprintf(gsub("\n", "", s), ...), quote = FALSE)
}
