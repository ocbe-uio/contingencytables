#' @title The Pearson chi-squared and likelihood ratio tests for homogeneity over strata
#' @description The Pearson chi-squared and likelihood ratio tests for homogeneity over strata
#' @description Described in Chapter 10 "Stratified 2x2 Tables and Meta-Analysis"
#' @param n the observed table (a 2x2xk matrix, where k is the number of strata)
#' @param link the link function ('linear', 'log', or 'logit')
#' @examples
#' # Smoking and lung cancer (Doll and Hill, 1950)
#' Pearson_LR_homogeneity_test_stratified_2x2(doll_hill_1950)
#'
#' # Prophylactice use of Lidocaine in myocardial infarction (Hine et al., 1989)
#' Pearson_LR_homogeneity_test_stratified_2x2(hine_1989)
#'
#' @export
#' @return An object of the [contingencytables_result] class,
#' basically a subclass of [base::list()]. Use the [utils::str()] function
#' to see the specific elements returned.
Pearson_LR_homogeneity_test_stratified_2x2 <- function(n, link = "logit") {
  validateArguments(mget(ls()))

  n1pk <- apply(n[1, , ], 2, sum)
  n2pk <- apply(n[2, , ], 2, sum)
  K <- dim(n)[3]

  # Get the estimated probabilities
  results <- ML_estimates_and_CIs_stratified_2x2(n, link, 0.05)
  pihat <- results$pihat

  # Calculate the expected cell counts
  m <- array(0, dim = c(2, 2, K))
  for (i in 1:2) {
    m[1, 1, ] <- n1pk * pihat[1, 1, ]
    m[1, 2, ] <- n1pk * pihat[1, 2, ]
    m[2, 1, ] <- n2pk * pihat[2, 1, ]
    m[2, 2, ] <- n2pk * pihat[2, 2, ]
  }

  # The likelihood ratio test statistic
  T_LR <- 0
  for (k in 1:K) {
    for (j in 1:2) {
      for (i in 1:2) {
        if (n[i, j, k] != 0) {
          T_LR <- T_LR + n[i, j, k] * log(n[i, j, k] / m[i, j, k])
        }
      }
    }
  }
  T_LR <- 2 * T_LR

  # The two-sided P-value (reference distribution: chi-squared with K - 1
  # degrees of freedom)
  df <- K - 1
  P_LR <- 1 - pchisq(T_LR, df)


  # The Pearson chi-squared test statistic
  T_Pearson <- 0
  for (k in 1:K) {
    for (j in 1:2) {
      for (i in 1:2) {
        if (m[i, j, k] != 0) {
          T_Pearson <- T_Pearson + ((n[i, j, k] - m[i, j, k])^2) / m[i, j, k]
        }
      }
    }
  }

  # The two-sided P-value (reference distribution: chi-squared with K - 1
  # degrees of freedom)
  P_Pearson <- 1 - pchisq(T_Pearson, df)

  results <- list()
  results$P_LR <- P_LR
  results$T_LR <- T_LR
  results$df_LR <- df
  results$P_Pearson <- P_Pearson
  results$T_Pearson <- T_Pearson
  results$df_Pearson <- df

  printresults <- function() {
    cat_sprintf("The likelihood ratio test: P = %7.5f, T0 = %5.3f (df = %i)\n", P_LR, T_LR, df)
    cat_sprintf("The Pearson chi-squared test: P = %7.5f, T0 = %5.3f (df = %i)", P_Pearson, T_Pearson, df)
  }

  return(contingencytables_result(results, printresults))
}
