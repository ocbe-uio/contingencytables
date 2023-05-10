#' @title The Pearson correlation coefficient with the bias-corrected and accelerated
#' @description The Pearson correlation coefficient with the bias-corrected and accelerated
#' @description boostrap confidence interval
#' @description Described in Chapter 7 "The rxc Table"
#' @param n the observed table (an rxc matrix)
#' @param nboot number of bootstrap samples
#' @param a scores assigned to the rows
#' @param b scores assigned to the columns
#' @param alpha the nominal significance level, used to compute a 100(1-alpha) confidence interval
#' @examples
#' set.seed(3509)
#' Pearson_correlation_coefficient_rxc_bca(table_7.7, nboot = 800)
#' Pearson_correlation_coefficient_rxc_bca(table_7.8, nboot = 200)
#' \dontrun{
#'   Pearson_correlation_coefficient_rxc_bca(table_7.9)
#' }
#' @export
#' @return An object of the [contingencytables_result] class,
#' basically a subclass of [base::list()]. Use the [utils::str()] function
#' to see the specific elements returned.
Pearson_correlation_coefficient_rxc_bca <- function(
  n, nboot = 1e4, a = seq_len(nrow(n)), b = seq_len(ncol(n)), alpha = 0.05
) {
  validateArguments(mget(ls()))

  # If no scores are given, use equally spaced scores
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
  rP <- Pearson_correlation_coefficient_rxc(n, a, b, alpha)$rP

  # The CI bootstrap sample
  dat <- data.frame(Y1 = Y1, Y2 = Y2)
  ans.boot <- boot(dat, f.Pccrb, R = nboot, stype = "i", .param = list(a, b, alpha, r, c))
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
      list(rP = rP, L = L, U = U),
      sprintf("The Pearson correlation w / BCa bootstrap CI: r = %7.4f (%g%% CI %7.4f to %7.4f)", rP, 100 * (1 - alpha), L, U)
    )
  )
}


# ===================================================
f.Pccrb <- function(dat, indx, .param) {
  # global aglobal bglobal alphaglobal r c
  a <- .param[[1]]
  b <- .param[[2]]
  alpha <- .param[[3]]
  r <- .param[[4]]
  c <- .param[[5]]
  n <- matrix(0, r, c)
  Y1 <- dat$Y1[indx]
  Y2 <- dat$Y2[indx]
  for (id in seq_along(Y1)) {
    n[Y1[id], Y2[id]] <- n[Y1[id], Y2[id]] + 1
  }
  rP <- Pearson_correlation_coefficient_rxc(n, a, b, alpha)$rP
  return(rP)
}
