#' @title The 1xc table tests
#' @param n the observed counts (a 1xc vector, where c is the number of categories)
#' @param pi0 given probabilities (a 1xc vector)
#' @param chacko.test if TRUE, only performs the Chacko test
#' @examples
#' # Genotype counts for SNP rs 6498169 in RA patients
#' the_1xc_table_tests(n = c(276, 380, 118), pi0 = c(0.402, 0.479, 0.119))
#' # subset of 10 patients
#' the_1xc_table_tests(n = c(6, 1, 3), pi0 = c(0.402, 0.479, 0.119))
#' # Example for the Chacko test: Hypothetical experiment
#' the_1xc_table_tests(n = c(1, 4, 3, 11, 9), pi0 = c(0.402, 0.479, 0.119), TRUE)
#' @export
#' @return NULL. This function should be called for its printed output
the_1xc_table_tests <- function(n, pi0, chacko.test = FALSE) {
  # ======================================================== #
  # Ad-hoc function to print output                          #
  # ======================================================== #
  myprint <- function(txt, ...) cat(sprintf(txt, ...), "\n")

  # ======================================================== #
  # Output                                                   #
  # ======================================================== #

  if (chacko.test) {
    Chacko_test_1xc(n, printresults = TRUE)
  } else {
    c0 <- length(n)
    N <- sum(n)
    for (i in 1:c0) {
      myprint(
        "Estimate of pi_%i: %2g / %2g = %5.3f (pi_%i,0 = %5.3f)",
        i, n[i], N, n[i] / N, i, pi0[i]
      )
    }
    myprint("Method                 P-value  (test statistic)")
    myprint("---------------------------------------------------")

    res <- Pearson_chi_squared_test_1xc(n, pi0, printresults = FALSE)
    myprint(
      "Pearson chi-squared    %6.4f   (T = %5.3f, df = %i)", res$P,
      res$T, res$df
    )

    res <- LR_test_1xc(n, pi0, printresults = FALSE)
    myprint(
      "Likelihood ratio       %6.4f   (T = %5.3f, df = %i)",
      res$P, res$T, res$df
    )

    if (N < 774) {
      res <- Exact_multinomial_test_1xc(n, pi0, printresults = FALSE)
      myprint("Exact multinomial      %6.4f\n", res)

      res <- MidP_multinomial_test_1xc(n, pi0, printresults = FALSE)
      myprint("Mid-P multinomial      %6.4f\n", res)
    }

    myprint("---------------------------------------------------")
  }
}
