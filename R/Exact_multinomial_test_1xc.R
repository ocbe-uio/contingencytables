#' @title The exact multinomial test for multinomial probabilities
#' @description The exact multinomial test for multinomial probabilities
#' @description Described in Chapter 3 "The 1xc Table and the Multinomial Distribution"
#' @param n the observed counts (a 1xc vector, where c is the number of categories)
#' @param pi0 given probabilities (a 1xc vector)
#' @importFrom stats dmultinom
#' @examples
#' # Genotype counts for SNP rs 6498169 in RA patients
#' Exact_multinomial_test_1xc(n = snp6498169$complete$n, pi0 = snp6498169$complete$pi0)
#'
#' # subset of 10 patients
#' Exact_multinomial_test_1xc(n = snp6498169$subset$n, pi0 = snp6498169$subset$pi0)
#' @export
#' @return An object of the [contingencytables_result] class,
#' basically a subclass of [base::list()]. Use the [utils::str()] function
#' to see the specific elements returned.
Exact_multinomial_test_1xc <- function(n, pi0) {
  validateArguments(mget(ls()))
  c0 <- length(n)
  N <- sum(n)

  # Identify all possible tables with N observations (with 3,4,...,7 categories)
  x <- switch(
    EXPR = c0,
    stop("Please provide a sample of size 3 or larger"),
    stop("Please provide a sample of size 3 or larger"),
    all_tables_3(N), all_tables_4(N), all_tables_5(N), all_tables_6(N),
    all_tables_7(N)
  )

  P <- 0
  Tobs <- sum(((n - N * pi0)^2) / (N * pi0))
  for (i in seq_len(nrow(x))) {
    T0 <- sum(((x[i, ] - N * pi0)^2) / (N * pi0)) # Pearson chi-squared
    if (T0 >= Tobs) {
      P <- P + dmultinom(x[i, ], prob = pi0)
    }
  }

  res <- contingencytables_result(
    list("P" = P), sprintf("The exact multinomial test: P = %7.5f", P)
  )
  return(res)
}

# =========================
all_tables_4 <- function(N) {
  x <- vector()
  for (x1 in (0:N)) {
    for (x2 in 0:(N - x1)) {
      for (x3 in 0:(N - x1 - x2)) {
        x <- rbind(x, c(x1, x2, x3, N - x1 - x2 - x3))
      }
    }
  }
  return(x)
}

# =========================
all_tables_5 <- function(N) {
  x <- vector()
  for (x1 in 0:N) {
    for (x2 in 0:(N - x1)) {
      for (x3 in 0:(N - x1 - x2)) {
        for (x4 in 0:(N - x1 - x2 - x3)) {
          x <- rbind(x, c(x1, x2, x3, x4, N - x1 - x2 - x3 - x4))
        }
      }
    }
  }
}

# =========================
all_tables_6 <- function(N) {
  x <- vector()
  for (x1 in 0:N) {
    for (x2 in 0:(N - x1)) {
      for (x3 in 0:(N - x1 - x2)) {
        for (x4 in 0:(N - x1 - x2 - x3)) {
          for (x5 in 0:(N - x1 - x2 - x3 - x4)) {
            x <- rbind(x, c(x1, x2, x3, x4, x5, N - x1 - x2 - x3 - x4 - x5))
          }
        }
      }
    }
  }
}

# =========================
all_tables_7 <- function(N) {
  x <- vector()
  for (x1 in 0:N) {
    for (x2 in 0:(N - x1)) {
      for (x3 in 0:(N - x1 - x2)) {
        for (x4 in 0:(N - x1 - x2 - x3)) {
          for (x5 in 0:(N - x1 - x2 - x3 - x4)) {
            for (x6 in 0:(N - x1 - x2 - x3 - x4 - x5)) {
              x <- rbind(
                x,
                c(x1, x2, x3, x4, x5, x6, N - x1 - x2 - x3 - x4 - x5 - x6)
              )
            }
          }
        }
      }
    }
  }
}
