#' @title The 2x2 table tests
#' @description Wrapper for \code{_test_2x2} functions on Chapter 4.
#' @param n frequency matrix
#' @param gamma  parameter for the Berger and Boos procedure
#' @examples
#' # Example: A lady tasting a cup of tea
#' n <- rbind(c(3,1), c(1,3))
#' the_2x2_table_tests(n)
#'
#' # Example: Perondi et al. (2004)
#' n <- rbind(c(7,27), c(1,33))
#' the_2x2_table_tests(n)
#'
#' # Example: Lampasona et al. (2013)
#' n <- rbind(c(9,4), c(4,10))
#' the_2x2_table_tests(n)
#'
#' # Example: Ritland et al. (2007)
#' n <- rbind(c(0,16), c(15,57))
#' the_2x2_table_tests(n)
#'
#' @export
#' @return The value of gamma. This function should be called for its printed output.
the_2x2_table_tests <- function(n, gamma=0.0001) {
	pi1hat <- n[1, 1] / (n[1, 1] + n[1, 2])
	pi2hat <- n[2, 1] / (n[2, 1] + n[2, 2])

	print(sprintf('H_0: pi_1 = pi_2  vs  H_A: pi_1 = / = pi_2'), quote=FALSE)
	print(sprintf('Estimate of pi_1: %i / %i = %5.3f', n[1, 1], n[1, 1] + n[1, 2], pi1hat), quote=FALSE)
	print(sprintf('Estimate of pi_2: %i / %i = %5.3f', n[2, 1], n[2, 1] + n[2, 2], pi2hat), quote=FALSE)

	print('Test                                  P-value  (test statistic)', quote=FALSE)
	print('------------------------------------------------------------------', quote=FALSE)
	res <- Pearson_chi_squared_test_2x2(n, printresults=FALSE)
	P0 <- res$p.value
	T0 <- res$statistic
	df <- res$df
	print(sprintf('Pearson chi-squared                   %6.4f   (T = %5.3f, df = %i)', P0, T0, df), quote=FALSE)

	res <- Pearson_chi_squared_test_CC_2x2(n, printresults=FALSE)
	P0 <- res$p.value
	T0 <- res$statistic
	df <- res$df
	print(sprintf('Pearson chi-squared w / CC              %6.4f   (T = %5.3f, df = %i)', P0, T0, df), quote=FALSE)

	res <- LR_test_2x2(n, printresults=FALSE)
	P0 <- res$p.value
	T0 <- res$statistic
	df <- res$df
	print(sprintf('Likelihood ratio                      %6.4f   (T = %5.3f, df = %i)', P0, T0, df), quote=FALSE)

	res <- Z_unpooled_test_2x2(n, printresults=FALSE)
	P0 <- res$p.value
	Z0 <- res$statistic
	print(sprintf('Z-unpooled                            %6.4f   (Z = %5.3f)', P0, Z0), quote=FALSE)

	P0 <- Fisher_exact_test_2x2(n, 'hypergeometric', printresults=FALSE)
	print(sprintf('Fisher exact test (Fisher-Irwin)      %6.4f', P0), quote=FALSE)

	P0 <- Fisher_exact_test_2x2(n, 'Pearson', printresults=FALSE)
	print(sprintf('Fisher exact test (Pearson)           %6.4f', P0), quote=FALSE)

	P0 <- Fisher_exact_test_2x2(n, 'LR', printresults=FALSE)
	print(sprintf('Fisher exact test (LR)                %6.4f', P0), quote=FALSE)

	P0 <- Fisher_midP_test_2x2(n, 'hypergeometric', printresults=FALSE)
	print(sprintf('Fisher mid-P test (Fisher-Irwin)      %6.4f', P0), quote=FALSE)

	P0 <- Exact_unconditional_test_2x2(n, 'Pearson', gamma, printresults=FALSE)
	print(sprintf('Suissa-Shuster exact uncond.*         %6.4f', P0), quote=FALSE)

	P0 <- Exact_unconditional_test_2x2(n, 'LR', gamma, printresults=FALSE)
	print(sprintf('Exact uncond. w / LR statistic*         %6.4f', P0), quote=FALSE)

	P0 <- Exact_unconditional_test_2x2(n, 'unpooled', gamma, printresults=FALSE)
	print(sprintf('Exact uncond. w / unpooled Z statistic* %6.4f', P0), quote=FALSE)

	P0 <- Exact_unconditional_test_2x2(n, 'Fisher', gamma, printresults=FALSE)
	print(sprintf('Fisher-Boschloo exact uncond.*        %6.4f', P0), quote=FALSE)

	print('------------------------------------------------------------------', quote=FALSE)
	print(sprintf(' * gamma = %-10.8g', gamma), quote=FALSE)

}
