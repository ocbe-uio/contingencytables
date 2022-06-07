#' @title The 1x2 Table CIs
#' @param X the number of successes
#' @param n the total number of observations
#' @param alpha the nominal level, e.g. 0.05 for 95% CIs
#' @examples
#' # The number of 1st order male births (Singh et al. 2010)
#' the_1x2_table_CIs(X = 250, n = 533)
#' # The number of 2nd order male births (Singh et al. 2010)
#' the_1x2_table_CIs(X = 204, n = 412)
#' # The number of 3rd order male births (Singh et al. 2010)
#' the_1x2_table_CIs(X = 103, n = 167)
#' # The number of 4th order male births (Singh et al. 2010)
#' the_1x2_table_CIs(X = 33, n = 45)
#' # Ligarden et al. (2010)
#' the_1x2_table_CIs(X = 13, n = 16)
#'
#' @export
#' @return NULL. This function should be called for its printed output
the_1x2_table_CIs <- function(X, n, alpha = 0.05) {
  estimate <- X / n

  # ======================================================== #
  # Ad-hoc function to print output                          #
  # ======================================================== #
  myprint <- function(txt, ...) cat(sprintf(txt, ...), "\n")

  # ======================================================== #
  # Output                                                   #
  # ======================================================== #

  myprint("Estimate of pi: %i / %i = %5.3f", X, n, estimate)

  myprint(
    "Interval method                  %i%% CI        width", 100 * (1 - alpha)
  )
  myprint("----------------------------------------------------")

  res <- Wald_CI_1x2(X, n, alpha, FALSE)
  myprint(
    "Wald                         %5.3f to %5.3f %8.3f",
    res[1], res[2], res[2] - res[1]
  )

  res <- Wald_CI_CC_1x2(X, n, alpha, FALSE)
  myprint(
    "Wald with CC                 %5.3f to %5.3f %8.3f",
    res[1], res[2], res[2] - res[1]
  )

  res <- LR_CI_1x2(X, n, alpha, FALSE)
  myprint(
    "Likelihood ratio             %5.3f to %5.3f %8.3f",
    res[1], res[2], res[2] - res[1]
  )

  res <- Wilson_score_CI_1x2(X, n, alpha, FALSE)
  myprint(
    "Wilson score                 %5.3f to %5.3f %8.3f",
    res[1], res[2], res[2] - res[1]
  )

  res <- Wilson_score_CI_CC_1x2(X, n, alpha, FALSE)
  myprint(
    "Wilson score with CC         %5.3f to %5.3f %8.3f",
    res[1], res[2], res[2] - res[1]
  )

  res <- AgrestiCoull_CI_1x2(X, n, alpha, FALSE)
  myprint(
    "Agresti-Coull                %5.3f to %5.3f %8.3f",
    res[1], res[2], res[2] - res[1]
  )

  res <- Jeffreys_CI_1x2(X, n, alpha, FALSE)
  myprint(
    "Jeffreys                     %5.3f to %5.3f %8.3f",
    res[1], res[2], res[2] - res[1]
  )

  res <- Arcsine_CI_1x2(X, n, alpha, FALSE)
  myprint(
    "Arcsine (Anscombe)           %5.3f to %5.3f %8.3f",
    res[1], res[2], res[2] - res[1]
  )

  res <- ClopperPearson_exact_CI_1x2(X, n, alpha, FALSE)
  myprint(
    "Clopper-Pearson exact        %5.3f to %5.3f %8.3f",
    res[1], res[2], res[2] - res[1]
  )

  res <- Blaker_exact_CI_1x2(X, n, alpha, FALSE)
  myprint(
    "Blaker exact                 %5.3f to %5.3f %8.3f",
    res[1], res[2], res[2] - res[1]
  )

  res <- ClopperPearson_midP_CI_1x2(X, n, alpha, FALSE)
  myprint(
    "Clopper-Pearson mid-p        %5.3f to %5.3f %8.3f",
    res[1], res[2], res[2] - res[1]
  )

  res <- Blaker_midP_CI_1x2(X, n, alpha, FALSE)
  myprint(
    "Blaker mid-P                 %5.3f to %5.3f %8.3f",
    res[1], res[2], res[2] - res[1]
  )

  myprint("----------------------------------------------------")
  myprint("CC = continuity correction")
}
