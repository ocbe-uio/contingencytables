#' @title The Cochran Q test of homogeneity of effects over strata
#' @description The Cochran Q test of homogeneity of effects over strata
#' @description Described in Chapter 10 "Stratified 2x2 Tables and Meta-Analysis"
#' @param n the observed table (a 2x2xk matrix, where k is the number of strata)
#' @param link the link function ('linear', 'log', or 'logit')
#' @param estimatetype Mantel-Haenszel or inverse variance estimate ('MH' or 'IV')
#' @examples
#' Cochran_Q_test_stratified_2x2(doll_hill_1950)
#' Cochran_Q_test_stratified_2x2(hine_1989)
#' @export
#' @return An object of the [contingencytables_result] class,
#' basically a subclass of [base::list()]. Use the [utils::str()] function
#' to see the specific elements returned.
Cochran_Q_test_stratified_2x2 <- function(
  n, link = "linear", estimatetype = "MH"
) {
  validateArguments(mget(ls()))
  K <- dim(n)[3]
  # Get the inverse variance weights (which are used for both MH and IV) and
  # the inverse variance estimate (this may be overwritten if the MH estimate
  # is chosen). In any case, use the psihat from the inverse variance because
  # these are on the log scale for the log and logit links.
  tmp <- InverseVariance_estimate_stratified_2x2(n, link)
  estimate <- tmp[[1]]
  psihat <- tmp[[2]]
  v <- tmp[[3]]
  if (identical(estimatetype, "MH")) {
    estimate <- MantelHaenszel_estimate_stratified_2x2(n, link)[[1]]
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

  # Output
  printresults <- function() {
    my_sprintf_cat(
      "The Cochran Q test (%s): P = %7.6f, Q = %5.3f (df = %g)",
      estimatetype, P, Q, df
    )
  }
  return(
    contingencytables_result(
      list("pvalue" = P, "Q" = Q, "df" = df),
      printresults
    )
  )
}
