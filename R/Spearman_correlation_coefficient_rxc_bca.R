#' @title The Spearman correlation coefficient with the bias-corrected and accelerated
#' @description The Spearman correlation coefficient with the bias-corrected and accelerated
#' @description boostrap confidence interval
#' @description Described in Chapter 7 "The rxc Table"
#' @param n the observed table (an rxc matrix)
#' @param nboot number of bootstrap samples
#' @param alpha the nominal significance level, used to compute a 100(1-alpha) confidence interval
#' @param printresults display results (FALSE = no, TRUE = yes)
#' @examples
#' \dontrun{
#' # Colorectal cancer (Table 7.7)
#' n <- rbind(
#'   c(2, 4, 29, 19), c(7, 6, 116, 51), c(19, 27, 201, 76), c(18, 22, 133, 54)
#' )
#'
#' Spearman_correlation_coefficient_rxc_bca(n)
#' # Breast Tumor (Table 7.8)
#' n <- matrix(
#'   c(15, 35, 6, 9, 6, 2, 4, 2, 11, 11, 0, 0, 1, 10, 21),
#'   ncol = 5, byrow = TRUE
#' )
#' Spearman_correlation_coefficient_rxc_bca(n)
#'
#' # Self-rated health (Table 7.9)
#' n <- matrix(
#'   c(2, 3, 3, 3, 2, 58, 98, 14, 8, 162, 949, 252, 4, 48, 373, 369),
#'   ncol = 4, byrow = TRUE
#' )
#' Spearman_correlation_coefficient_rxc_bca(n)
#' }
#' @export
#' @return A list containing the statistic and the confindence interval limits
Spearman_correlation_coefficient_rxc_bca <- function(n, nboot = 10000, alpha = 0.05, printresults = TRUE) {
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
	rho <- Spearman_correlation_coefficient_rxc(n, alpha, printresults = FALSE)$rho

	# The CI bootstrap sample
	# funchandle = @(Y1,Y2) put_data_back_into_table_format(Y1, Y2)
	# L = ci(1)
	# U = ci(2)

	dat <- data.frame(Y1 = Y1, Y2 = Y2)
	ans.boot <- boot(dat, f.Sccrb, R = nboot, stype = "i", .param = list(alpha, r, c))
	ans.ci <- boot.ci(ans.boot, conf = 1 - alpha, type = "bca")
	L <- ans.ci$bca[4]
	U <- ans.ci$bca[5]

	if (printresults) {
		.print("The Spearman correlation w / BCa bootstrap CI: rho = %7.4f (%g%% CI %7.4f to %7.4f)\n", rho, 100 * (1 - alpha), L, U)
	}

	invisible(list(rho = rho, L = L, U = U))
}


# ===================================================
f.Sccrb <- function(dat, indx, .param) {
	# global alphaglobal r c
	alpha <- .param[[1]]
	r <- .param[[2]]
	c <- .param[[3]]
	Y1 <- dat$Y1[indx]
	Y2 <- dat$Y2[indx]
	n <- matrix(0, r, c)
	for (id in 1:length(Y1)) {
		n[Y1[id], Y2[id]] <- n[Y1[id], Y2[id]] + 1
	}
	rho <- Spearman_correlation_coefficient_rxc(n, alpha, printresults = FALSE)$rho
	return(rho)
}

.print <- function(s, ...) {
	print(sprintf(gsub("\n", "", s), ...), quote = FALSE)
}
