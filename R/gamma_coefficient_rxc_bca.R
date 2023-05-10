#' @title The gamma coefficient with the bias-corrected and accelerated boostrap confidence interval
#' @description The gamma coefficient with the bias-corrected and accelerated boostrap confidence interval
#' @description Described in Chapter 7 "The rxc Table"
#' @param n the observed table (an rxc matrix)
#' @param nboot number of bootstrap samples
#' @param alpha the nominal significance level, used to compute a 100(1-alpha) confidence interval
#' @importFrom boot boot boot.ci
#' @examples
#' set.seed(9623)
#' gamma_coefficient_rxc_bca(table_7.7, nboot = 800)
#' gamma_coefficient_rxc_bca(table_7.8, nboot = 200)
#' \dontrun{
#'   gamma_coefficient_rxc_bca(table_7.9, nboot = 3000, alpha = 0.2)
#' }
#' @export
#' @return An object of the [contingencytables_result] class,
#' basically a subclass of [base::list()]. Use the [utils::str()] function
#' to see the specific elements returned.
gamma_coefficient_rxc_bca <- function(n, nboot = 10000, alpha = 0.05) {
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
  gamma <- gamma_coefficient_rxc(n)$gamma

  # The CI bootstrap sample
  dat <- data.frame(Y1 = Y1, Y2 = Y2)
  ans.boot <- boot(dat, f.gcrb, R = nboot, stype = "i")
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
      list(gamma = gamma, L = L, U = U),
      sprintf("The gamma coefficient w / BCa bootstrap CI: gamma = %7.4f (%g%% CI %7.4f to %7.4f)",
        gamma, 100 * (1 - alpha), L, U
      )
    )
  )
}

f.gcrb <- function(dat, d) {
  Y1 <- dat[d, 1]
  Y2 <- dat[d, 2]
  n <- matrix(0, max(Y1), max(Y2))
  for (id in seq_along(Y1)) {
    n[Y1[id], Y2[id]] <- n[Y1[id], Y2[id]] + 1
  }
  res <- gamma_coefficient_rxc(n)
  return(res$gamma)
}
