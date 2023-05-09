#' @title The Paired CxC table - nominal
#' @param n the total number of observations
#' @param alpha the nominal level, e.g. 0.05 for 95% CIs
#' @examples
#' # Pretherapy susceptability of pathogens (Peterson et al., 2007)
#' the_paired_cxc_table_nominal(peterson_2007)
#' @export
#' @return NULL. This function should be called for its printed output.
the_paired_cxc_table_nominal <- function(n, alpha = 0.05) {
  validateArguments(mget(ls()))

  c <- nrow(n)
  N <- sum(n)

  my_sprintf_cat("\n                       rows    cols\n")
  my_sprintf_cat("-----------------------------------\n")
  for (i in 1:c) {
    my_sprintf_cat("Marginal proportion %g: %5.3f  %5.3f\n", i, sum(n[i, ]) / N, sum(n[, i]) / N)
  }
  my_sprintf_cat("-----------------------------------\n")

  my_sprintf_cat("\nTests for nominal categories                Statistic      P-value\n")
  my_sprintf_cat("-------------------------------------------------------------------\n")

  tmp <- Bhapkar_test_paired_cxc(n)
  P <- tmp[[1]]
  T0 <- tmp[[2]]
  df <- tmp[[3]]
  my_sprintf_cat("Bhapkar test for marginal homogeneity      %6.3f (df=%g)  %9.6f\n", T0, df, P)

  tmp <- Stuart_test_paired_cxc(n)
  P <- tmp[[1]]
  T0 <- tmp[[2]]
  df <- tmp[[3]]
  my_sprintf_cat("Stuart test for marginal homogeneity       %6.3f (df=%g)  %9.6f\n", T0, df, P)

  if (c == 3) {
    tmp <- FleissEveritt_test_paired_cxc(n)
    P <- tmp[[1]]
    T0 <- tmp[[2]]
    df <- tmp[[3]]
    my_sprintf_cat("Fleiss-Everitt version of the Stuart test  %6.3f (df=%g)  %9.6f\n", T0, df, P)
  }

  tmp <- McNemarBowker_test_paired_cxc(n)
  P <- tmp[[1]]
  T0 <- tmp[[2]]
  df <- tmp[[3]]
  my_sprintf_cat("McNemar-Bowker test for symmetry           %6.3f (df=%g)  %9.6f\n", T0, df, P)
  my_sprintf_cat("-------------------------------------------------------------------\n")


  my_sprintf_cat("\nTests and confidence intervals for individual categories:\n")
  my_sprintf_cat("\nCategory        Estimate    ScheffE 95%%CI         Bonferroni 95%%CI     P-value*\n")
  my_sprintf_cat("-------------------------------------------------------------------------------\n")
  for (i in 1:c) {
    tmp <- Scheffe_type_CIs_paired_cxc(n, alpha)
    Scheffe_L <- tmp[[1]]
    Scheffe_U <- tmp[[2]]
    deltahat <- tmp[[3]]
    tmp <- Bonferroni_type_CIs_paired_cxc(n, alpha)
    Bonferroni_L <- tmp[[1]]
    Bonferroni_U <- tmp[[2]]

    # Calculate the McNemar asymptotic test with adjusted degrees of freedom
    n12 <- sum(n[i, ]) - n[i, i]
    n21 <- sum(n[, i]) - n[i, i]
    Z <- (n12 - n21) / sqrt(n12 + n21)
    P <- 1 - pchisq(Z^2, c - 1)

    my_sprintf_cat("pi_%g+ - pi_+%g: %8.4f  (%7.4f to %7.4f)   (%7.4f to %7.4f)    %6.4f\n", i, i, deltahat[i], Scheffe_L[i], Scheffe_U[i], Bonferroni_L[i], Bonferroni_U[i], P)
  }
  my_sprintf_cat("-------------------------------------------------------------------------------\n")
  my_sprintf_cat("*Adjusted McNemar asymptotic test with c - 1 = %g degrees of freedom\n\n", c - 1)
  invisible(NULL)
}
