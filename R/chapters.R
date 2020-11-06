#' @title Load functions from a chapter
#' @description Loads all the functions associated with a specific book chapter.
#' @param chap_num Number of book chapter (from 2 to 10)
#' @return Loads the functions from that chapter
#' @author Waldir Leoncio
#' @references https://stackoverflow.com/q/64388629/1169233
#' @export
load_chapter <- function(chap_num) {
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
				Arcsine_CI_1x2      = Arcsine_CI_1x2,
				Wald_CI_1x2         = Wald_CI_1x2
			)
		)
		chap_name <- paste("Chapter", chap_num)
		is_attached <- any(grepl(pattern = chap_name, x = search()))
		if (is_attached) {
			message(
				"Chapter functions already loaded. ",
				"You can unload them with unload_chapter(", chap_num,
				"). Exiting."
			)
		} else {
			message("Loading functions from chapter ", chap_num)
			attach(list2env(chapters[[chap_num - 1]]), name = chap_name)
		}
		# on.exit(detach(target))
	}
}

#' @title Unload functions from a chapter
#' @description Unloads all the functions associated with a specific book chapter.
#' @param chap_num Number of book chapter (from 2 to 10)
#' @return Unloads the functions from that chapter
#' @author Waldir Leoncio
#' @export
unload_chapter <- function(chap_num) {
	if (missing(chap_num)) {
		stop("Please choose a chapter between 2 and 10.")
	}
	if (chap_num < 2 | chap_num > 10) {
		stop(
			"There is no Chapter ", chap_num,
			". Please choose a chapter between 2 and 10."
		)
	} else {
		# TODO: this list is repeated. Merge!
		chapters <- list(
			ch2 = list(
				AgrestiCoull_CI_1x2 = AgrestiCoull_CI_1x2,
				Arcsine_CI_1x2      = Arcsine_CI_1x2,
				Wald_CI_1x2         = Wald_CI_1x2
			)
		)
		chap_name <- paste("Chapter", chap_num)
		is_attached <- any(grepl(pattern = chap_name, x = search()))
		if (!is_attached) {
			message(
				"Chapter functions not attached. ",
				"Use load_chapter(", chap_num, ") to attach them. Exiting."
			)
		} else {
			message("Unloading functions from chapter ", chap_num)
			detach(eval(chap_name), character.only = TRUE)
		}
	}
}

# TODO: address generated check notes (post question on StackOverflow?)