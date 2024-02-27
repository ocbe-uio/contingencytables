#' @title The Mantel-Haenszel estimate of the overall effect across strata
#' @description The Mantel-Haenszel estimate of the overall effect across strata
#' @description Described in Chapter 10 "Stratified 2x2 Tables and Meta-Analysis"
#' @param n the observed table (a 2x2xk matrix, where k is the number of strata)
#' @param link the link function ('linear', 'log', or 'logit')
#' @examples
#' MantelHaenszel_estimate_stratified_2x2(doll_hill_1950)
#' MantelHaenszel_estimate_stratified_2x2(hine_1989)
#' @export
#' @return An object of the [contingencytables_result] class,
#' basically a subclass of [base::list()]. Use the [utils::str()] function
#' to see the specific elements returned.
MantelHaenszel_estimate_stratified_2x2 <- function(n, link = "logit") {
  validateArguments(mget(ls()))

  n1pk <- apply(n[1, , ], 2, sum)
  n2pk <- apply(n[2, , ], 2, sum)
  nppk <- apply(n, 3, sum)

  # Calculate stratum-specific effect estimates
  if (identical(link, "linear")) {
    psihat <- n[1, 1, ] / n1pk - n[2, 1, ] / n2pk
  } else if (identical(link, "log")) {
    psihat <- (n[1, 1, ] / n1pk) / (n[2, 1, ] / n2pk)
  } else if (identical(link, "logit")) {
    psihat <- (n[1, 1, ] * n[2, 2, ]) / (n[1, 2, ] * n[2, 1, ])
  }

  # Calculate weights
  if (identical(link, "linear")) {
    w <- n1pk * n2pk / nppk
  } else if (identical(link, "log")) {
    w <- n1pk * n[2, 1, ] / nppk
  } else if (identical(link, "logit")) {
    w <- n[1, 2, ] * n[2, 1, ] / nppk
  }

  # The Mantel-Haenszel estimate of the overall effect
  estimate <- sum(w * psihat) / sum(w)

  return(
    contingencytables_result(
      list("estimate" = estimate, "psihat" = psihat, "w" = w),
      sprintf("The Mantel-Haenszel estimate = %7.4f", estimate)
    )
  )
}
