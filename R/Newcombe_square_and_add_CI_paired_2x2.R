#' @title The Newcombe square-and-add confidence interval for the difference
#' @description The Newcombe square-and-add confidence interval for the difference between paired probabilities.
#' @description Described in Chapter 8 "The Paired 2x2 Table"
#' @param n the observed table (a 2x2 matrix)
#' @param alpha the nominal level, e.g. 0.05 for 95# CIs
#' @param printresults display results (FALSE = no, TRUE = yes)
#' @examples
#' # Airway hyper-responsiveness before and after stem cell transplantation
#' # (Bentur et al., 2009)
#' n <- rbind(c(1, 1), c(7, 12))
#' Newcombe_square_and_add_CI_paired_2x2(n)
#'
#' # Complete response before and after consolidation therapy
#' # (Cavo et al., 2012)
#' n <- matrix(c(59, 6, 16, 80), 2, byrow = TRUE)
#' Newcombe_square_and_add_CI_paired_2x2(n)
#'
#' @export
#' @return A list containing lower, upper and point estimates of the statistic
Newcombe_square_and_add_CI_paired_2x2 <- function(n, alpha = 0.05, printresults = TRUE) {
  nip <- apply(n, 1, sum)
  npi <- apply(n, 2, sum)
  N <- sum(n)

  # Estimates of the success probabilities and their difference (deltahat)
  pi1phat <- nip[1] / N
  pip1hat <- npi[1] / N
  estimate <- pi1phat - pip1hat

  # Calculate the Wilson score interval for each success probability
  tmp <- Wilson_score_CI_1x2(nip[1], N, alpha, printresults = FALSE)
  l1 <- tmp[[1]]
  u1 <- tmp[[2]]
  tmp <- Wilson_score_CI_1x2(npi[1], N, alpha, printresults = FALSE)
  l2 <- tmp[[1]]
  u2 <- tmp[[2]]

  # psi: estimate of the correlation coefficient between pi1phat and pip1hat
  if (nip[1] == 0 || nip[2] == 0 || npi[1] == 0 || npi[2] == 0) {
    psi <- 0
  } else {
    nprod <- nip[1] * nip[2] * npi[1] * npi[2]
    A <- n[1, 1] * n[2, 2] - n[1, 2] * n[2, 1]
    if (A > N / 2) {
      psi <- (A - N / 2) / sqrt(nprod)
    } else if (A >= 0 && A <= N / 2) {
      psi <- 0
    } else {
      psi <- A / sqrt(nprod)
    }
  }

  # The square and add method
  L <- estimate - sqrt((pi1phat - l1)^2 + (u2 - pip1hat)^2 - 2 * psi * (pi1phat - l1) * (u2 - pip1hat))
  U <- estimate + sqrt((pip1hat - l2)^2 + (u1 - pi1phat)^2 - 2 * psi * (pip1hat - l2) * (u1 - pi1phat))

  if (printresults) {
    .print("The Newcombe square-and-add CI: estimate = %7.4f (%g%% CI %7.4f to %7.4f)\n", estimate, 100 * (1 - alpha), L, U)
  }

  invisible(list(L = L, U = U, estimate = estimate))
}

.print <- function(s, ...) {
  print(sprintf(gsub("\n", "", s), ...), quote = FALSE)
}
