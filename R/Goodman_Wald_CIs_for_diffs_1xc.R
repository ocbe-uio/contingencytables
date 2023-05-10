#' @title The Goodman Wald simultaneous intervals for the differences between the
#' @description The Goodman Wald simultaneous intervals for the differences between the
#' @description multinomial probabilities (with Scheffe or Bonferroni adjustment)
#' @description Described in Chapter 3 "The 1xc Table and the Multinomial Distribution"
#' @param n the observed counts (a 1xc vector, where c is the number of categories)
#' @param alpha the nominal level, e.g. 0.05 for 95# CIs
#' @param adjustment Scheffe or Bonferroni adjustment ("Scheffe" or "Bonferroni")
#' @examples
#' Goodman_Wald_CIs_for_diffs_1xc(n = snp6498169$complete$n)
#' @export
#' @return An object of the [contingencytables_result] class,
#' basically a subclass of [base::list()]. Use the [utils::str()] function
#' to see the specific elements returned.
Goodman_Wald_CIs_for_diffs_1xc <- function(n, alpha = 0.05, adjustment = "Bonferroni") {
  validateArguments(
    x = mget(ls()),
    types = list(
      n = "counts", alpha = "probability",
      adjustment = c("Bonferroni", "Scheffe")
    )
  )

  c0 <- length(n)
  N <- sum(n)

  # Estimates of the multinomial probabilities
  pihat <- n / N

  # Simultaneous confidence intervals with Scheffe or Bonferroni adjustment
  L <- rep(0, c0 * (c0 - 1) / 2)
  U <- rep(0, c0 * (c0 - 1) / 2)
  diffs <- rep(0, c0 * (c0 - 1) / 2)
  Scheffe <- qchisq(1 - alpha, c0 - 1)
  Bonferroni <- qchisq(1 - alpha / c0, 1)

  k <- 0
  for (i in 1:(c0 - 1)) {
    for (j in (i + 1):c0) {
      k <- k + 1
      diffs[k] <- pihat[i] - pihat[j]
      if (adjustment == "Scheffe") {
        term2 <- sqrt(
          Scheffe * (pihat[i] + pihat[j] - (diffs[k])^2) / N
        )
        L[k] <- diffs[k] - term2
        U[k] <- diffs[k] + term2
      } else if (adjustment == "Bonferroni") {
        term2 <- sqrt(
          Bonferroni * (pihat[i] + pihat[j] - (diffs[k])^2) / N
        )
        L[k] <- diffs[k] - term2
        U[k] <- diffs[k] + term2
      }
    }
  }

  # Output
  printresults <- function() {
    cat(
      "The Goodman Wald simultaneous intervals for differences (",
      adjustment,
      ")\n ",
      sep = ""
    )
    k <- 0
    for (i in 1:(c0 - 1)) {
      for (j in (i + 1):c0) {
        k <- k + 1
        txt <- sprintf(
          " pi_%i - pi_%i: estimate = %6.4f (%6.4f to %6.4f)\n ",
          i, j, diffs[k], L[k], U[k]
        )
        cat(txt)
      }
    }
  }
  res <- list("lower" = L, "upper" = U, "estimate" = diffs)
  return(contingencytables_result(res, printresults))
}
