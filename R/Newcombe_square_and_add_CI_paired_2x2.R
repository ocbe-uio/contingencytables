#' @title The Newcombe square-and-add confidence interval for the difference
#' @description The Newcombe square-and-add confidence interval for the difference between paired probabilities.
#' @description Described in Chapter 8 "The Paired 2x2 Table"
#' @param n the observed table (a 2x2 matrix)
#' @param alpha the nominal level, e.g. 0.05 for 95# CIs
#' @examples
#' # Airway hyper-responsiveness before and after stem cell transplantation
#' # (Bentur et al., 2009)
#' Newcombe_square_and_add_CI_paired_2x2(bentur_2009)
#'
#' # Complete response before and after consolidation therapy
#' # (Cavo et al., 2012)
#' Newcombe_square_and_add_CI_paired_2x2(cavo_2012)
#'
#' @export
#' @return An object of the [contingencytables_result] class,
#' basically a subclass of [base::list()]. Use the [utils::str()] function
#' to see the specific elements returned.
Newcombe_square_and_add_CI_paired_2x2 <- function(n, alpha = 0.05) {
  validateArguments(mget(ls()))

  nip <- apply(n, 1, sum)
  npi <- apply(n, 2, sum)
  N <- sum(n)

  # Estimates of the success probabilities and their difference (deltahat)
  pi1phat <- nip[1] / N
  pip1hat <- npi[1] / N
  estimate <- pi1phat - pip1hat

  # Calculate the Wilson score interval for each success probability
  tmp <- Wilson_score_CI_1x2(nip[1], N, alpha)
  l1 <- tmp[[1]]
  u1 <- tmp[[2]]
  tmp <- Wilson_score_CI_1x2(npi[1], N, alpha)
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

  return(
    contingencytables_result(
      list(L = L, U = U, estimate = estimate),
      sprintf("The Newcombe square-and-add CI: estimate = %7.4f (%g%% CI %7.4f to %7.4f)", estimate, 100 * (1 - alpha), L, U)
    )
  )
}
