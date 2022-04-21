#' @title The MOVER Wilson score confidence interval for the ratio of paired probabilities
#' @description The MOVER Wilson score confidence interval for the ratio of paired probabilities
#' @description Described in Chapter 8 "The Paired 2x2 Table"
#' @param n the observed counts (a 2x2 matrix)
#' @param alpha the nominal level, e.g. 0.05 for 95% CIs
#' @param printresults display results (F = no, T= yes)
#' @examples
#' n <- rbind(c(1, 1), c(7, 12))
#' MOVER_Wilson_score_CI_paired_2x2(n)
#'
#' # Complete response before and after consolidation therapy
#' # (Cavo et al., 2012)
#' n <- matrix(c(59, 6, 16, 80), 2, byrow = TRUE)
#' MOVER_Wilson_score_CI_paired_2x2(n)
#'
#' @export
#' @return A list containing lower, upper and point estimates of the statistic
MOVER_Wilson_score_CI_paired_2x2 <- function(n, alpha = 0.05, printresults = TRUE) {
  N <- sum(n)
  pi1phat <- (n[1, 1] + n[1, 2]) / N
  pip1hat <- (n[1, 1] + n[2, 1]) / N

  # Estimate of the ratio of probabilities (phihat)
  estimate <- (n[1, 1] + n[1, 2]) / (n[1, 1] + n[2, 1])

  # Use Wilson score intervals for pi_1+ and pi_ + 1
  tmp <- Wilson_score_CI_1x2(n[1, 1] + n[1, 2], N, alpha, printresults = FALSE)
  l1 <- tmp[[1]]
  u1 <- tmp[[2]]
  tmp <- Wilson_score_CI_1x2(n[1, 1] + n[2, 1], N, alpha, printresults = FALSE)
  l2 <- tmp[[1]]
  u2 <- tmp[[2]]

  # The estimated correlation between pi_1+ and pi_ + 1
  corrhat <- (n[1, 1] * n[2, 2] - n[1, 2] * n[2, 1]) / sqrt((n[1, 1] + n[1, 2]) * (n[2, 1] + n[2, 2]) * (n[1, 1] + n[2, 1]) * (n[1, 2] + n[2, 2]))
  if (is.na(corrhat) | abs(corrhat) == Inf) {
    corrhat <- 0
  }
  A <- corrhat * (pi1phat - l1) * (u2 - pip1hat)
  B <- corrhat * (u1 - pi1phat) * (pip1hat - l2)

  L <- (A - pi1phat * pip1hat + sqrt((A - pi1phat * pip1hat)^2 - l1 * (2 * pi1phat - l1) * u2 * (2 * pip1hat - u2))) / (u2 * (u2 - 2 * pip1hat))
  U <- (B - pi1phat * pip1hat - sqrt((B - pi1phat * pip1hat)^2 - u1 * (2 * pi1phat - u1) * l2 * (2 * pip1hat - l2))) / (l2 * (l2 - 2 * pip1hat))
  L <- max(0, L)
  if (is.na(U) | U < 0) {
    U <- Inf
  }

  if (printresults) {
    .print("The MOVER Wilson score CI: estimate = %6.4f (%g%% CI %6.4f to %6.4f)\n", estimate, 100 * (1 - alpha), L, U)
  }
  invisible(list(L = L, U = U, estimate = estimate))
}

.print <- function(s, ...) {
  print(sprintf(gsub("\n", "", s), ...), quote = FALSE)
}
