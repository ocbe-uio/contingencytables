#' @export
chapter <- function(chap_num) {
	if (missing(chap_num)) {
		stop("Please choose a chapter between 2 and 10.")
	}
	if (chap_num < 2 | chap_num > 10) {
		stop(
			"There is no Chapter ", chap_num,
			". Please choose a chapter between 2 and 10."
		)
	} else {
		chapters <- list(
			ch2 = list(
				AgrestiCoull_CI_1x2 = contingencytables:::AgrestiCoull_CI_1x2,
				Wald_CI_1x2 = contingencytables:::Wald_CI_1x2
			)
		)
		message("Loading functions from chapter ", chap_num)
		attach(list2env(chapters[[chap_num - 1]]))
	}
}