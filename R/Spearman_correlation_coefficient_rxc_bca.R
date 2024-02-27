#' @title The Spearman correlation coefficient with the bias-corrected and accelerated
#' @description The Spearman correlation coefficient with the bias-corrected and accelerated
#' @description boostrap confidence interval
#' @description Described in Chapter 7 "The rxc Table"
#' @param n the observed table (an rxc matrix)
#' @param nboot number of bootstrap samples
#' @param alpha the nominal significance level, used to compute a 100(1-alpha) confidence interval
#' @examples
#' set.seed(2921)
#' Spearman_correlation_coefficient_rxc_bca(table_7.7, nboot = 800)
#' Spearman_correlation_coefficient_rxc_bca(table_7.8, nboot = 200)
#' \dontrun{
#'   Spearman_correlation_coefficient_rxc_bca(table_7.9)
#' }
#' @export
#' @return An object of the [contingencytables_result] class,
#' basically a subclass of [base::list()]. Use the [utils::str()] function
#' to see the specific elements returned.
Spearman_correlation_coefficient_rxc_bca <- function(n, nboot = 10000, alpha = 0.05) {
  validateArguments(mget(ls()))

  r <- nrow(n)
  c <- ncol(n)
  N <- sum(n)

  # Put the observed data into long format for bootstrapping the two samples
  Y1 <- rep(0, N)
  Y2 <- rep(0, N)
  id <- 0
  for (i in 1:r) {
    for (j in 1:c) {
      if (n[i, j] > 0) {
        for (k in 1:n[i, j]) {
          id <- id + 1
          Y1[id] <- i
          Y2[id] <- j
        }
      }
    }
  }

  # The estimate
  rho <- Spearman_correlation_coefficient_rxc(n, alpha)$rho

  # The CI bootstrap sample
  dat <- data.frame(Y1 = Y1, Y2 = Y2)
  ans.boot <- boot(dat, f.Sccrb, R = nboot, stype = "i", .param = list(alpha, r, c))
  ans.ci <- tryCatch(
    boot.ci(ans.boot, conf = 1 - alpha, type = "bca"),
    error = function(e) {
      stop("Insufficient samples. Increase nboot.", call. = FALSE)
    }
  )
  L <- ans.ci$bca[4]
  U <- ans.ci$bca[5]

  return(
    contingencytables_result(
      list("rho" = rho, "lower" = L, "upper" = U),
      sprintf("The Spearman correlation w / BCa bootstrap CI: rho = %7.4f (%g%% CI %7.4f to %7.4f)", rho, 100 * (1 - alpha), L, U)
    )
  )
}


# ===================================================
f.Sccrb <- function(dat, indx, .param) {
  # global alphaglobal r c
  alpha <- .param[[1]]
  r <- .param[[2]]
  c <- .param[[3]]
  Y1 <- dat$Y1[indx]
  Y2 <- dat$Y2[indx]
  n <- matrix(0, r, c)
  for (id in seq_along(Y1)) {
    n[Y1[id], Y2[id]] <- n[Y1[id], Y2[id]] + 1
  }
  rho <- Spearman_correlation_coefficient_rxc(n, alpha)$rho
  return(rho)
}
