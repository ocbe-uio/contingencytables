#' @title The inverse variance estimate of the overall effect across strata
#' @description The inverse variance estimate of the overall effect across strata
#' @description Described in Chapter 10 "Stratified 2x2 Tables and Meta-Analysis"
#' @param n the observed table (a 2x2xk matrix, where k is the number of strata)
#' @param link the link function ('linear', 'log', or 'logit')
#' @examples
#' InverseVariance_estimate_stratified_2x2(doll_hill_1950)
#' InverseVariance_estimate_stratified_2x2(hine_1989)
#' @export
#' @return An object of the [contingencytables_result] class,
#' basically a subclass of [base::list()]. Use the [utils::str()] function
#' to see the specific elements returned.
InverseVariance_estimate_stratified_2x2 <- function(n, link = "logit") {
  validateArguments(mget(ls()))

  n1pk <- apply(n[1, , ], 2, sum)
  n2pk <- apply(n[2, , ], 2, sum)

  # Calculate stratum-specific effect estimates
  if (identical(link, "linear")) {
    psihat <- n[1, 1, ] / n1pk - n[2, 1, ] / n2pk
  } else if (identical(link, "log")) {
    psihat <- log((n[1, 1, ] / n1pk) / (n[2, 1, ] / n2pk))
  } else if (identical(link, "logit")) {
    psihat <- log((n[1, 1, ] * n[2, 2, ]) / (n[1, 2, ] * n[2, 1, ]))
  }

  # Calculate weights
  if (identical(link, "linear")) {
    v <- 1 / (n[1, 1, ] * n[1, 2, ] / n1pk^3 + n[2, 1, ] * n[2, 2, ] / n2pk^3)
  } else if (identical(link, "log")) {
    v <- 1 / (1 / n[1, 1, ] - 1 / n1pk + 1 / n[2, 1, ] - 1 / n2pk)
  } else if (identical(link, "logit")) {
    v <- 1 / (1 / n[1, 1, ] + 1 / n[1, 2, ] + 1 / n[2, 1, ] + 1 / n[2, 2, ])
  }

  # The inverse variance estimate of the overall effect
  estimate <- sum(v * psihat) / sum(v)
  if (identical(link, "log") || identical(link, "logit")) {
    estimate <- exp(estimate)
  }

  return(
    contingencytables_result(
      list(estimate = estimate, psihat = psihat, v = v),
      sprintf("The inverse variance estimate = %7.4f", estimate)
    )
  )
}
