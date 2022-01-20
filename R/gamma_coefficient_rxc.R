#' @title The gamma coefficient
#' @description The gamma coefficient
#' @description Described in Chapter 7 "The rxc Table"
#' @param n the observed table (an rxc matrix)
#' @param printresults display results (FALSE = no, TRUE = yes)
#' @examples
#' \dontrun{
#' # Colorectal cancer (Table 7.7)
#' n <- rbind(
#'   c(2, 4, 29, 19), c(7, 6, 116, 51), c(19, 27, 201, 76), c(18, 22, 133, 54)
#' )
#' gamma_coefficient_rxc_bca(n)
#'
#'   # Breast Tumor (Table 7.8)
#'   n <- matrix(
#'     c(15, 35, 6, 9, 6, 2, 4, 2, 11, 11, 0, 0, 1, 10, 21),
#'     ncol = 5, byrow = TRUE
#'   )
#'   gamma_coefficient_rxc_bca(n)
#'
#'   # Self-rated health (Table 7.9)
#'   n <- matrix(
#'     c(2, 3, 3, 3, 2, 58, 98, 14, 8, 162, 949, 252, 4, 48, 373, 369),
#'     ncol = 4, byrow = TRUE
#'   )
#'   gamma_coefficient_rxc_bca(n)
#' }
#' @export
#' @return a list containing the gamma coefficient, the number of concordant
#' pairs and the number of discordant pairs
gamma_coefficient_rxc <- function(n, printresults = TRUE) {
	r <- nrow(n)
	c <- ncol(n)

	C <- 0
	for (i in 1:(r - 1)) {
		for (j in 1:(c - 1)) {
			for (k in (i + 1):r) {
				for (l in (j + 1):c) {
					C <- C + n[i, j] * n[k, l]
				}
			}
		}
	}

	D <- 0
	for (i in 1:(r - 1)) {
		for (l in 1:(c - 1)) {
			for (k in (i + 1):r) {
				for (j in (l + 1):c) {
					D <- D + n[i, j] * n[k, l]
				}
			}
		}
	}

	# The gamma coefficient
	gamma <- (C - D) / (C + D)

	if (printresults) {
		.print("The number of concordant pairs:      %g\n", C)
		.print("The number of discordant pairs:      %g\n", D)
		.print("The proportion of concordant pairs:  %g\n", C / (C + D))
		.print("The proportion of discordant pairs:  %g\n", D / (C + D))
		.print("The gamma coefficient:               %6.4f\n", gamma)
	}

	invisible(list(gamma = gamma, C = C, D = D))
}

.print <- function(s, ...) {
	print(sprintf(gsub("\n", "", s), ...), quote = FALSE)
}
