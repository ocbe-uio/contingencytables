#' @title The Paired 2x2 table tests
#' @param n frequency matrix
#' @param gamma parameter for the Berger and Boos procedure
#' @examples
#' # Airway hyper-responsiveness before and after stem cell transplantation
#' # (Bentur et al., 2009)
#' n <- rbind(c(1, 1), c(7, 12))
#' the_paired_2x2_table_tests(n)
#'
#' \dontrun{
#' # Complete response before and after consolidation therapy
#' # (Cavo et al., 2012)
#' n <- rbind(c(59, 6), c(16, 80))
#' the_paired_2x2_table_tests(n)
#'
#' #' # Floppy eyelid syndrome vs obstructive sleep apnea (Ezra et al., 2010)
#' n <- rbind(c(7, 25), c(2, 68))
#' the_paired_2x2_table_tests(n)
#' }
#' @export
#' @return The value of gamma. This function should be called for its printed output.
the_paired_2x2_table_tests <- function(n, gamma = 0.0001) {
  N <- sum(n)

  pi1phat <- (n[1, 1] + n[1, 2]) / N
  pip1hat <- (n[1, 1] + n[2, 1]) / N

  .print("\nH_0: pi_1+ = pi_+1  vs  H_A: pi_1+ ~= pi_+1\n\n")
  .print("Estimate of pi_1+: %i/%i = %5.3f\n", n[1, 1] + n[1, 2], N, pi1phat)
  .print("Estimate of pi_+1: %i/%i = %5.3f\n\n", n[1, 1] + n[2, 1], N, pip1hat)

  print("Test                             P-value  (test statistic)", quote = F)
  print("---------------------------------------------------------", quote = F)

  tmp <- McNemar_asymptotic_test_paired_2x2(n, F)
  P <- tmp[[1]]
  Z <- tmp[[2]]
  .print("McNemar asymptotic               %6.4f   (Z = %5.3f)\n", P, Z)

  tmp <- McNemar_asymptotic_test_CC_paired_2x2(n, F)
  P <- tmp[[1]]
  Z <- tmp[[2]]
  .print("McNemar asymptotic w/CC          %6.4f   (Z = %5.3f)\n", P, Z)

  P <- McNemar_exact_cond_test_paired_2x2(n, F)
  .print("McNemar exact conditional        %6.4f\n", P)

  P <- McNemar_midP_test_paired_2x2(n, F)
  .print("McNemar mid-P                    %6.4f\n", P)

  P <- McNemar_exact_unconditional_test_paired_2x2(n, 0, F)
  .print("McNemar exact unconditional      %6.4f\n", P)

  P <- McNemar_exact_unconditional_test_paired_2x2(n, gamma, F)
  .print("McNemar exact unconditional*     %6.4f\n", P)

  print("---------------------------------------------------------", quote = F)
  .print("*gamma = %-10.8g\n", gamma)
}

.print <- function(s, ...) {
  print(sprintf(gsub("\n", "", s), ...), quote = F)
}
