#' @title The Pearson chi-squared and likelihood ratio tests for association in rxc tables
#' @description The Pearson chi-squared and likelihood ratio tests for association in rxc tables
#' @description Described in Chapter 7 "The rxc Table"
#' @param n the observed counts (an rxc matrix)
#' @examples
#' # Examples from Chapter 5 (ordered rx2 tables)
#'
#' ## Alcohol consumption and malformations (Mills and Graubard, 1987):
#' Pearson_LR_tests_rxc(mills_graubard_1987)
#'
#' ## Elevated troponin T levels in stroke patients (Indredavik et al., 2008):
#' Pearson_LR_tests_rxc(indredavik_2008)
#'
#' # Examples from Chapter 6 (ordered 2xc tables)
#' ## The Adolescent Placement Study (Fontanella et al., 2008):
#' Pearson_LR_tests_rxc(fontanella_2008)
#'
#' ## Postoperative nausea (Lydersen et al., 2012a):
#' Pearson_LR_tests_rxc(lydersen_2012a)
#'
#' # Examples from Chapter 7 (unordered rxc tables)
#'
#' ## Treatment for ear infection (van Balen et al., 2003):
#' Pearson_LR_tests_rxc(table_7.3)
#'
#' ## Psychiatric diagnoses vs PA (Mangerud et al., 2004):
#' Pearson_LR_tests_rxc(table_7.4)
#'
#' ## Psychiatric diag. vs BMI (Mangerud et al., 2004):
#' Pearson_LR_tests_rxc(table_7.5)
#' @export
#' @return An object of the [contingencytables_result] class,
#' basically a subclass of [base::list()]. Use the [utils::str()] function
#' to see the specific elements returned.
Pearson_LR_tests_rxc <- function(n) {
  validateArguments(mget(ls()))

  r <- nrow(n)
  c0 <- ncol(n)
  nip <- apply(n, 1, sum)
  npj <- apply(n, 2, sum)
  N <- sum(n)

  # The Pearson chi-squared and likelihood ratio statistics
  m <- matrix(0, r, c0)
  T_Pearson <- 0
  T_LR <- 0
  for (i in 1:r) {
    for (j in 1:c0) {
      m[i, j] <- nip[i] * npj[j] / N
      T_Pearson <- T_Pearson + ((n[i, j] - m[i, j])^2) / m[i, j]
      if (n[i, j] > 0) {
        T_LR <- T_LR + n[i, j] * log(n[i, j] / m[i, j])
      }
    }
  }
  T_LR <- 2 * T_LR

  # The two-sided P-values (reference distribution: chi-squared with
  # (r-1)(c-1) degrees of freedom)
  df <- (r - 1) * (c0 - 1)
  P_Pearson <- 1 - pchisq(T_Pearson, df)
  P_LR <- 1 - pchisq(T_LR, df)

  # Output arguments (observed statistics, degrees of freedom, and P-values)
  results <- list()
  results$T_Pearson <- T_Pearson
  results$df_Pearson <- df
  results$P_Pearson <- P_Pearson
  results$T_LR <- T_LR
  results$df_LR <- df
  results$P_LR <- P_LR

  printresults <- function() {
    my_sprintf_cat("Pearson chi-squared test: T = %6.3f, df = %g, P = %7.5f\n", T_Pearson, df, P_Pearson)
    my_sprintf_cat("Likelihood ratio test:    T = %6.3f, df = %g, P = %7.5f", T_LR, df, P_LR)
  }

  return(contingencytables_result(results, printresults))
}
