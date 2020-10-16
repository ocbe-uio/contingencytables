#' @title Load functions from a chapter
#' @description Loads all the functions associated with a specific book chapter.
#' @param chap_num Number of book chapter (from 2 to 10)
#' @return Loads the functions from that chapter
#' @author Waldir Leoncio
#' @references https://stackoverflow.com/q/64388629/1169233
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
				AgrestiCoull_CI_1x2 = AgrestiCoull_CI_1x2,
				Wald_CI_1x2 = Wald_CI_1x2
			)
		)
		message("Loading functions from chapter ", chap_num)
		attach(list2env(chapters[[chap_num - 1]]))
	}
}
# TODO: add unloading function?
# TODO: address generated check notes (post question on StackOverflow?)