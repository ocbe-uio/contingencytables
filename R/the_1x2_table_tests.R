#' @title The 1x2 Table tests
#' @param X the number of successes
#' @param n the total number of observations
#' @param pi0 a given probability
#' @examples
#' # Example: The number of 1st order male births (Singh et al. 2010)
#' the_1x2_table_tests(X = 250, n = 533, pi0 = 0.513)
#' # Example: The number of 2nd order male births (Singh et al. 2010)
#' the_1x2_table_tests(X = 204, n = 412, pi0 = 0.513)
#' # Example: The number of 3rd order male births (Singh et al. 2010)
#' the_1x2_table_tests(X = 103, n = 167, pi0 = 0.513)
#' # Example: The number of 4th order male births (Singh et al. 2010)
#' the_1x2_table_tests(X = 33, n = 45, pi0 = 0.513)
#' # Example: Ligarden et al. (2010)
#' the_1x2_table_tests(X = 13, n = 16, pi0 = 0.5)
#' @export
#' @return NULL. This function should be called for its printed output
the_1x2_table_tests <- function(X, n, pi0) {

  # ======================================================== #
  # Ad-hoc function to print output                          #
  # ======================================================== #
  myprint <- function(txt, ...) cat(sprintf(txt, ...), "\n")

  # ======================================================== #
  # Output                                                   #
  # ======================================================== #

  estimate <- X / n

  myprint("H_0: pi = %5.3f  vs  H_A: pi ~= %5.3f", pi0, pi0)
  myprint("Estimate of pi: %i/%i = %5.3f", X, n, estimate)

  myprint("Test                P-value  (test statistic)")
  myprint("------------------------------------------------")

  res <- Wald_test_1x2(X, n, pi0, FALSE)
  myprint("Wald                %6.4f   (Z = %5.3f)", res[1], res[2])

  res <- Wald_test_CC_1x2(X, n, pi0, FALSE)
  myprint("Wald with CC        %6.4f   (Z = %5.3f)", res[1], res[2])

  res <- LR_test_1x2(X, n, pi0, FALSE)
  myprint("Likelihood ratio    %6.4f   (T = %5.3f, df = %i)", res[1], res[2], res[3])

  res <- Score_test_1x2(X, n, pi0, FALSE)
  myprint("Score               %6.4f   (Z = %5.3f)", res[1], res[2])

  res <- Score_test_CC_1x2(X, n, pi0, FALSE)
  myprint("Score with CC       %6.4f   (Z = %5.3f)", res[1], res[2])

  P <- Exact_binomial_test_1x2(X, n, pi0, FALSE)
  myprint("Exact binomial      %6.4f", P)

  P <- Blaker_exact_test_1x2(X, n, pi0, FALSE)
  myprint("Blaker exact        %6.4f", P)

  midP <- MidP_binomial_test_1x2(X, n, pi0, FALSE)
  myprint("Mid-P binomial      %6.4f", midP)

  midP <- Blaker_midP_test_1x2(X, n, pi0, FALSE)
  myprint("Blaker mid-P        %6.4f", midP)

  myprint("------------------------------------------------")
  myprint("CC = continuity correction")
}
