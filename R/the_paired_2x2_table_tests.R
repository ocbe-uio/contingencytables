#' @title The Paired 2x2 table tests
#' @param n frequency matrix
#' @param gamma parameter for the Berger and Boos procedure
#' @param num_pi_values number of values to use in the partition of the nuisance parameter space (default=1000)
#' @examples
#' the_paired_2x2_table_tests(bentur_2009)
#' the_paired_2x2_table_tests(cavo_2012, gamma = 0, num_pi_values = 10)
#' the_paired_2x2_table_tests(ezra_2010, gamma = 0, num_pi_values = 20)
#' @export
#' @return NULL. This function should be called for its printed output.
the_paired_2x2_table_tests <- function(n, gamma = 0.0001, num_pi_values = 1000L) {
  validateArguments(mget(ls()))

  N <- sum(n)

  pi1phat <- (n[1, 1] + n[1, 2]) / N
  pip1hat <- (n[1, 1] + n[2, 1]) / N

  cat_sprintf("\nH_0: pi_1+ = pi_+1  vs  H_A: pi_1+ ~= pi_+1\n\n")
  cat_sprintf("Estimate of pi_1+: %i/%i = %5.3f\n", n[1, 1] + n[1, 2], N, pi1phat)
  cat_sprintf("Estimate of pi_+1: %i/%i = %5.3f\n\n", n[1, 1] + n[2, 1], N, pip1hat)

  cat("Test                             P-value  (test statistic)\n")
  cat("---------------------------------------------------------\n")

  tmp <- McNemar_asymptotic_test_paired_2x2(n)
  P <- tmp[[1]]
  Z <- tmp[[2]]
  cat_sprintf("McNemar asymptotic               %6.4f   (Z = %5.3f)\n", P, Z)

  tmp <- McNemar_asymptotic_test_CC_paired_2x2(n)
  P <- tmp[[1]]
  Z <- tmp[[2]]
  cat_sprintf("McNemar asymptotic w/CC          %6.4f   (Z = %5.3f)\n", P, Z)

  P <- McNemar_exact_cond_test_paired_2x2(n)
  cat_sprintf("McNemar exact conditional        %6.4f\n", P)

  P <- McNemar_midP_test_paired_2x2(n)
  cat_sprintf("McNemar mid-P                    %6.4f\n", P)

  if (gamma != 0) {
    P <- McNemar_exact_unconditional_test_paired_2x2(n, 0, num_pi_values)
    cat_sprintf("McNemar exact unconditional      %6.4f\n", P)
  }

  P <- McNemar_exact_unconditional_test_paired_2x2(n, gamma, num_pi_values)
  cat_sprintf("McNemar exact unconditional*     %6.4f\n", P)

  cat("---------------------------------------------------------\n")
  cat_sprintf("*gamma = %-10.8g\n", gamma)
  invisible(NULL)
}
