#' @title The gamma coefficient with the bias-corrected and accelerated boostrap confidence interval
#' @description The gamma coefficient with the bias-corrected and accelerated boostrap confidence interval
#' @description Described in Chapter 7 "The rxc Table"
#' @param n the observed table (an rxc matrix)
#' @param nboot number of bootstrap samples
#' @param alpha the nominal significance level, used to compute a 100(1-alpha) confidence interval
#' @param printresults display results (FALSE = no, TRUE = yes)
#' @importFrom boot boot boot.ci
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
#' @return a list with the gamma coefficient and the confidence interval limits
gamma_coefficient_rxc_bca <- function(n, nboot = 10000, alpha = 0.05, printresults = TRUE) {
	r <- nrow(n)
	c <- ncol(n)
	N <- sum(n)

	# Put the observed data into long format for bootstrapping the two samples
	Y1 <- rep(0, N)
	Y2 <- rep(0, N)
	id <- 0
	for (i in 1:r) {
		for (j in 1:c) {
			if (n[i, j] > 0) {
				for (k in 1:n[i, j]) {
					id <- id + 1
					Y1[id] <- i
					Y2[id] <- j
				}
			}
		}
	}

	# The estimate
	gamma <- gamma_coefficient_rxc(n, 0)$gamma

	# The CI bootstrap sample
	dat <- data.frame(Y1 = Y1, Y2 = Y2)
	ans.boot <- boot(dat, f.gcrb, R = nboot, stype = "i")
	ans.ci <- boot.ci(ans.boot, conf = 1 - alpha, type = "bca")
	L <- ans.ci$bca[4]
	U <- ans.ci$bca[5]

	if (printresults) {
		.print("The gamma coefficient w / BCa bootstrap CI: gamma = %7.4f (%g%% CI %7.4f to %7.4f)\n", gamma, 100 * (1 - alpha), L, U)
	}

	invisible(list(gamma = gamma, L = L, U = U))
}

f.gcrb <- function(dat, d) {
	Y1 <- dat[d, 1]
	Y2 <- dat[d, 2]
	n <- matrix(0, max(Y1), max(Y2))
	for (id in 1:length(Y1)) {
		n[Y1[id], Y2[id]] <- n[Y1[id], Y2[id]] + 1
	}
	res <- gamma_coefficient_rxc(n, printresults = FALSE)
	return(res$gamma)
}

.print <- function(s, ...) {
	print(sprintf(gsub("\n", "", s), ...), quote = FALSE)
}
