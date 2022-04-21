#' @title The linear-by-linear test for association
#' @description The linear-by-linear test for association
#' @description Described in Chapter 7 "The rxc Table"
#' @param n the observed table (an rxc matrix)
#' @param a scores assigned to the rows
#' @param b scores assigned to the columns
#' @param printresults display results (0 = no, 1 = yes)
#' @examples
#' #' # Colorectal cancer (Table 7.7)
#' n <- rbind(
#'   c(2, 4, 29, 19), c(7, 6, 116, 51), c(19, 27, 201, 76), c(18, 22, 133, 54)
#' )
#' linear_by_linear_test_rxc(n)
#'
#' \dontrun{
#' # Breast Tumor (Table 7.8)
#' n <- matrix(
#'   c(15, 35, 6, 9, 6, 2, 4, 2, 11, 11, 0, 0, 1, 10, 21),
#'   ncol = 5, byrow = TRUE
#' )
#' linear_by_linear_test_rxc(n)
#'
#' # Self-rated health (Table 7.9)
#' n <- matrix(
#'   c(2, 3, 3, 3, 2, 58, 98, 14, 8, 162, 949, 252, 4, 48, 373, 369),
#'   ncol = 4, byrow = TRUE
#' )
#' linear_by_linear_test_rxc(n)
#' }
#' @export
#' @return a list containing the linear-by-linear test statistic
linear_by_linear_test_rxc <- function(n, a = seq_len(ncol(n)), b = seq_len(nrow(n)), printresults = TRUE) {
  # If no scores are given, use equally spaced scores
  r <- nrow(n)
  c <- ncol(n)

  N <- sum(n)

  # Put the observed data into long format
  Y1 <- rep(0, N)
  Y2 <- rep(0, N)
  id <- 0
  for (i in 1:r) {
    for (j in 1:c) {
      for (k in 1:n[i, j]) {
        id <- id + 1
        Y1[id] <- a[i]
        Y2[id] <- b[j]
      }
    }
  }

  # The Pearson correlation coefficient
  r <- cor(Y1, Y2)

  # The linear-by-linear test statistic
  Z <- sqrt(N - 1) * r
  P <- 2 * (1 - pnorm(abs(Z), 0, 1))

  if (printresults) {
    .print("The linear-by-linear test for association: P = %8.6f, Z = %6.3f\n", P, Z)
  }

  invisible(list(P = P, Z = Z))
}

.print <- function(s, ...) {
  print(sprintf(gsub("\n", "", s), ...), quote = FALSE)
}
