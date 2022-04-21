#' @title The McNemar exact unconditional test
#' @description The McNemar exact unconditional test
#' @description Described in Chapter 8 "The Paired 2x2 Table"
#' @param n the observed table (a 2x2 matrix)
#' @param gamma parameter for the Berger and Boos procedure (default=0.0001; gamma=0: no adj)
#' @param printresults display results (FALSE = no, TRUE = yes)
#' @examples
#' # Airway hyper-responsiveness before and after stem cell transplantation
#' # (Bentur et al., 2009)
#' n <- rbind(c(1, 1), c(7, 12))
#' McNemar_exact_unconditional_test_paired_2x2(n)
#'
#' \dontrun{
#' # Complete response before and after consolidation therapy
#' # (Cavo et al., 2012)
#' n <- rbind(c(59, 6), c(16, 80))
#' McNemar_exact_unconditional_test_paired_2x2(n)
#' }
#'
#' # Floppy eyelid syndrome vs obstructive sleep apnea (Ezra et al., 2010)
#' n <- rbind(c(7, 25), c(2, 68))
#' McNemar_exact_unconditional_test_paired_2x2(n)
#' @export
#' @note Somewhat crude code with maximization over a simple partition of the
#' nuisance parameter space into 'num_pi_values' equally spaced values
#' (default: 1000). The number may be changed below. This method could be
#' improved with a better algorithm for the maximization; however, it works
#' well for most purposes. Try \code{showplot=1} to get an indication of
#' the precision. A refinement of the maximization can be done with a manual
#' restriction of the parameter space.
#' @importFrom graphics segments
#' @return The T version of the test statistic (not the Z one)
McNemar_exact_unconditional_test_paired_2x2 <- function(n, gamma = 0.0001, printresults = TRUE) {
  # Partition the parameter space into 'num_pi_values' equally spaced values
  num_pi_values <- 1000

  # Display a plot of the P-value as a function of the common success probability
  showplot <- 0

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
    tmp <- ClopperPearson_exact_CI_1x2_beta_version(n[1, 2] + n[2, 1], N, gamma, FALSE)
    L <- tmp[[1]]
    U <- tmp[[2]]
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

  # Display a plot of the P-value as a function of the common success probability
  if (showplot == 1) {
    common_pi_at_max_value <- pivalues[index]
    dev.new()
    plot(pivalues, Pvalues, type = "l", lwd = 2)
    segments(common_pi_at_max_value, 0, common_pi_at_max_value, P, col = "red", lty = 2)
  }


  if (printresults) {
    .print("The McNemar exact unconditional test: P = %8.6f\n", P)
  }

  invisible(P)
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

.print <- function(s, ...) {
  print(sprintf(gsub("\n", "", s), ...), quote = FALSE)
}
