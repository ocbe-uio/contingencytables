#' @title The 2x2 table tests
#' @description Wrapper for `_test_2x2` functions on Chapter 4.
#' @param n frequency matrix
#' @param gamma  parameter for the Berger and Boos procedure
#' @examples
#' # Example: A lady tasting a cup of tea
#' the_2x2_table_tests(tea)
#'
#' # Example: Lampasona et al. (2013)
#' the_2x2_table_tests(lampasona_2013)
#'
#' \dontrun{
#'   the_2x2_table_tests(perondi_2004) # Example: Perondi et al. (2004)
#'   the_2x2_table_tests(ritland_2007) # Example: Ritland et al. (2007)
#' }
#' @export
#' @return NULL. This function should be called for its printed output
the_2x2_table_tests <- function(n, gamma = 0.0001) {
  validateArguments(mget(ls()))

  pi1hat <- n[1, 1] / (n[1, 1] + n[1, 2])
  pi2hat <- n[2, 1] / (n[2, 1] + n[2, 2])

  cat("H_0: pi_1 = pi_2  vs  H_A: pi_1 = / = pi_2\n")
  cat_sprintf("Estimate of pi_1: %i / %i = %5.3f\n", n[1, 1], n[1, 1] + n[1, 2], pi1hat)
  cat_sprintf("Estimate of pi_2: %i / %i = %5.3f\n\n", n[2, 1], n[2, 1] + n[2, 2], pi2hat)

  cat("Test                                  P-value  (test statistic)\n")
  cat("------------------------------------------------------------------\n")
  res <- Pearson_chi_squared_test_2x2(n)
  P0 <- res$Pvalue
  T0 <- res$T
  df <- res$df
  cat_sprintf("Pearson chi-squared                     %6.4f (T = %5.3f, df = %i)\n", P0, T0, df)

  res <- Pearson_chi_squared_test_CC_2x2(n)
  P0 <- res$Pvalue
  T0 <- res$T
  df <- res$df
  cat_sprintf("Pearson chi-squared w / CC              %6.4f (T = %5.3f, df = %i)\n", P0, T0, df)

  res <- LR_test_2x2(n)
  P0 <- res$Pvalue
  T0 <- res$T
  df <- res$df
  cat_sprintf("Likelihood ratio                        %6.4f (T = %5.3f, df = %i)\n", P0, T0, df)

  res <- Z_unpooled_test_2x2(n)
  P0 <- res$Pvalue
  Z0 <- res$T
  cat_sprintf("Z-unpooled                              %6.4f (Z = %5.3f)\n", P0, Z0)

  P0 <- Fisher_exact_test_2x2(n, "hypergeometric")$Pvalue
  cat_sprintf("Fisher exact test (Fisher-Irwin)        %6.4f\n", P0)

  P0 <- Fisher_exact_test_2x2(n, "Pearson")$Pvalue
  cat_sprintf("Fisher exact test (Pearson)             %6.4f\n", P0)

  P0 <- Fisher_exact_test_2x2(n, "LR")$Pvalue
  cat_sprintf("Fisher exact test (LR)                  %6.4f\n", P0)

  P0 <- Fisher_midP_test_2x2(n, "hypergeometric")$Pvalue
  cat_sprintf("Fisher mid-P test (Fisher-Irwin)        %6.4f\n", P0)

  P0 <- Exact_unconditional_test_2x2(n, "Pearson", gamma)$Pvalue
  cat_sprintf("Suissa-Shuster exact uncond.*           %6.4f\n", P0)

  P0 <- Exact_unconditional_test_2x2(n, "LR", gamma)$Pvalue
  cat_sprintf("Exact uncond. w / LR statistic*         %6.4f\n", P0)

  P0 <- Exact_unconditional_test_2x2(n, "unpooled", gamma)$Pvalue
  cat_sprintf("Exact uncond. w / unpooled Z statistic* %6.4f\n", P0)

  P0 <- Exact_unconditional_test_2x2(n, "Fisher", gamma)$Pvalue
  cat_sprintf("Fisher-Boschloo exact uncond.*          %6.4f\n", P0)

  cat("------------------------------------------------------------------\n")
  cat_sprintf(" * gamma = %-10.8g\n", gamma)
  invisible(NULL)
}
