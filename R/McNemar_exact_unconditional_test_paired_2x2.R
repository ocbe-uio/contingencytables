#' @title The McNemar exact unconditional test
#' @description The McNemar exact unconditional test
#' @description Described in Chapter 8 "The Paired 2x2 Table"
#' @param n the observed table (a 2x2 matrix)
#' @param gamma parameter for the Berger and Boos procedure (default=0.0001; gamma=0: no adj)
#' @param num_pi_values number of values to use in the partition of the nuisance parameter space (default=1000)
#' @examples
#' McNemar_exact_unconditional_test_paired_2x2(bentur_2009)
#' \dontrun{
#'   McNemar_exact_unconditional_test_paired_2x2(cavo_2012, gamma = 0)
#'   McNemar_exact_unconditional_test_paired_2x2(ezra_2010)
#' }
#' @export
#' @note Somewhat crude code with maximization over a simple partition of the
#' nuisance parameter space into 'num_pi_values' equally spaced values
#' The number may be changed. This method could be
#' improved with a better algorithm for the maximization; however, it works
#' well for most purposes. Try \code{showplot=1} to get an indication of
#' the precision. A refinement of the maximization can be done with a manual
#' restriction of the parameter space.
#' @importFrom graphics segments
#' @return An object of the [contingencytables_result] class,
#' basically a subclass of [base::list()]. Use the [utils::str()] function
#' to see the specific elements returned.
McNemar_exact_unconditional_test_paired_2x2 <- function(n, gamma = 0.0001, num_pi_values = 1000L) {
  validateArguments(mget(ls()))
  N <- sum(n)

  # Calculate and store the trinomial coefficients needed
  trinomcoeffs <- matrix(0, N + 1, N + 1)
  for (x12 in 0:N) {
    for (x21 in 0:(N - x12)) {
      trinomcoeffs[x12 + 1, x21 + 1] <- factorial(N) / (factorial(x12) * factorial(x21) * factorial(N - x12 - x21))
    }
  }

  # Find the tables that agree equally or less with H0 as the observed
  tables <- matrix(0, N + 1, N + 1)
  Tobs <- test_statistic.2(n[1, 2], n[2, 1])
  for (x12 in 0:N) {
    for (x21 in 0:(N - x12)) {
      T0 <- test_statistic.2(x12, x21)
      if (!is.na(T0) && T0 >= Tobs) {
        tables[x12 + 1, x21 + 1] <- 1
      }
    }
  }

  # A simple partition the nuisance parameter space
  if (gamma == 0) {
    pivalues <- seq(0, 1, length = num_pi_values)
  } else {
    # Berger and Boos procedure
    # Use the Clopper-Pearson exact interval
    tmp <- ClopperPearson_exact_CI_1x2_beta_version(n[1, 2] + n[2, 1], N, gamma)
    L <- tmp$lower
    U <- tmp$upper
    pivalues <- seq(L, U, length = num_pi_values)
  }

  # Calculate the P-value corresponding to each value of the nuisance parameter
  Pvalues <- rep(0, length(pivalues))
  for (i in seq_along(pivalues)) {
    Pvalues[i] <- calculate_Pvalue.2(pivalues[i], tables, trinomcoeffs, N)
  }

  # Let the exact unconditional P-value equal the maximum of the P-values
  P <- max(Pvalues)
  index <- which(P == Pvalues)[1]

  # Add gamma (the parameter for the Berger and Boos procedure) to make sure
  # the actual significance level is bounded by the nominal level
  P <- min(P + gamma, 1)

  # Handle cases where the P-value is not computable
  if (sum(tables) == 0) {
    P <- 1.0
  }

  return(
    contingencytables_result(
      list("Pvalue" = P, "Pvalues" = Pvalues, "pi_values" = pivalues),
      sprintf("The McNemar exact unconditional test: P = %8.6f", P)
    )
  )
}



# ==================================================================
calculate_Pvalue.2 <- function(pivalue, tables, trinomcoeffs, N) {
  Pvalue <- 0
  for (x12 in 0:N) {
    for (x21 in 0:(N - x12)) {
      if (tables[x12 + 1, x21 + 1] == 1) {
        Pvalue <- Pvalue + trinomcoeffs[x12 + 1, x21 + 1] * ((pivalue / 2)^(x12 + x21)) * ((1 - pivalue)^(N - x12 - x21))
      }
    }
  }
  return(Pvalue)
}


# ===================================
test_statistic.2 <- function(x12, x21) {
  # This is the T version of the statistic, not the Z version
  T0 <- ((x12 - x21)^2) / (x12 + x21)
  return(T0)
}
