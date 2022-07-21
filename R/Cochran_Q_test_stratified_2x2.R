#' @title The Cochran Q test of homogeneity of effects over strata
#' @description The Cochran Q test of homogeneity of effects over strata
#' @description Described in Chapter 10 "Stratified 2x2 Tables and Meta-Analysis"
#' @param n the observed table (a 2x2xk matrix, where k is the number of strata)
#' @param link the link function ('linear', 'log', or 'logit')
#' @param estimatetype Mantel-Haenszel or inverse variance estimate ('MH' or 'IV')
#' @param printresults display results (FALSE = no, TRUE = yes)
#' @examples
#' Cochran_Q_test_stratified_2x2(doll_hill_1950)
#' Cochran_Q_test_stratified_2x2(hine_1989)
#' @export
#' @return A list containing the probability, the statistic and the degrees of freedom
Cochran_Q_test_stratified_2x2 <- function(
  n, link = "linear", estimatetype = "MH", printresults = TRUE
) {
  validateArguments(mget(ls()))
  K <- dim(n)[3]
  # Get the inverse variance weights (which are used for both MH and IV) and
  # the inverse variance estimate (this may be overwritten if the MH estimate
  # is chosen). In any case, use the psihat from the inverse variance because
  # these are on the log scale for the log and logit links.
  tmp <- InverseVariance_estimate_stratified_2x2(n, link, FALSE)
  estimate <- tmp[[1]]
  psihat <- tmp[[2]]
  v <- tmp[[3]]
  if (identical(estimatetype, "MH")) {
    estimate <- MantelHaenszel_estimate_stratified_2x2(n, link, FALSE)[[1]]
  }

  # The Cochran Q test statistic
  if (identical(link, "linear")) {
    Q <- sum(v * (psihat - estimate)^2)
  } else {
    if (identical(link, "log") || identical(link, "logit")) {
      Q <- sum(v * (psihat - log(estimate))^2)
    }
  }

  # The two-sided P-value (reference distribution: chi-squared with K - 1
  # degrees of freedom)
  df <- K - 1
  P <- 1 - pchisq(Q, df)

  if (printresults) {
    .print("The Cochran Q test (%s): P = %7.5f, Q = %5.3f (df = %i)\n", estimatetype, P, Q, df)
  }

  invisible(list(P = P, Q = Q, df = df))
}


.print <- function(s, ...) {
  print(sprintf(gsub("\n", "", s), ...), quote = FALSE)
}
