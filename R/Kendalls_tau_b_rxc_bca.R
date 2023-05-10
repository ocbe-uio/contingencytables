#' @title Kendall's tau-b with the bias-corrected and accelerated boostrap confidence interval
#' @description Kendall's tau-b with the bias-corrected and accelerated boostrap confidence interval
#' @description Described in Chapter 7 "The rxc Table"
#' @param n the observed table (an rxc matrix)
#' @param nboot number of bootstrap samples
#' @param alpha the nominal significance level, used to compute a 100(1-alpha) confidence interval
#' @examples
#' set.seed(9974)
#' Kendalls_tau_b_rxc_bca(table_7.7, nboot = 800)
#' Kendalls_tau_b_rxc_bca(table_7.8, nboot = 200)
#' \dontrun{
#'   Kendalls_tau_b_rxc_bca(table_7.9)
#' }
#' @export
#' @return An object of the [contingencytables_result] class,
#' basically a subclass of [base::list()]. Use the [utils::str()] function
#' to see the specific elements returned.
Kendalls_tau_b_rxc_bca <- function(n, nboot = 10000, alpha = 0.05) {
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
      for (k in 1:n[i, j]) {
        id <- id + 1
        Y1[id] <- i
        Y2[id] <- j
      }
    }
  }

  # The estimate
  tau_b <- Kendalls_tau_b_rxc(n, alpha)$tau_b

  # The CI bootstrap sample
  dat <- data.frame(Y1 = Y1, Y2 = Y2)
  ans.boot <- boot(dat, f.Ktbrb, R = nboot, stype = "i", .alpha = alpha, .r = r, .c = c)
  ans.ci <- boot.ci(ans.boot, conf = 1 - alpha, type = "bca")
  L <- ans.ci$bca[4]
  U <- ans.ci$bca[5]

  return(
    contingencytables_result(
      list(tau_b = tau_b, L = L, U = U),
      sprintf(
        "Kendalls tau-b w / BCa bootstrap CI: tau-b = %7.4f (%g%% CI %7.4f to %7.4f)",
        tau_b, 100 * (1 - alpha), L, U
      )
    )
  )
}

f.Ktbrb <- function(dat, d, .alpha, .r, .c) {
  n <- matrix(0, .r, .c)
  Y1 <- dat$Y1[d]
  Y2 <- dat$Y2[d]
  for (id in seq_along(Y1)) {
    n[Y1[id], Y2[id]] <- n[Y1[id], Y2[id]] + 1
  }
  res <- Kendalls_tau_b_rxc(n, .alpha)
  return(res$tau_b)
}
