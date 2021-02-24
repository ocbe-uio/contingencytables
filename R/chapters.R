#' @title Load functions from a chapter
#' @description Loads all the functions associated with a specific book chapter.
#' @param chap_num Number of book chapter (from 2 to 10)
#' @return Loads the functions from that chapter
#' @author Waldir Leoncio
#' @references https://stackoverflow.com/q/64388629/1169233
#' @export
load_chapter <- function(chap_num) {
	validate_chapter_choice(chap_num)
	chapters <- list_chapter_functions()
	chap_name <- paste("Chapter", chap_num)
	is_attached <- any(grepl(pattern = chap_name, x = search()))
	if (is_attached) {
		message(
			"Chapter functions already loaded. Reloading. ",
			"You can unload them with unload_chapter(", chap_num, ")"
		)
		reload_chapter(chap_num)
	} else {
		message("Loading functions from chapter ", chap_num)
		attach(list2env(chapters[[chap_num - 1]]), name = chap_name)
	}
	# on.exit(detach(target)) # attempt to solve "attach" note
}

#' @title Unload functions from a chapter
#' @description Unloads all the functions associated with a specific book chapter.
#' @param chap_num Number of book chapter (from 2 to 10)
#' @return Unloads the functions from that chapter
#' @author Waldir Leoncio
#' @export
unload_chapter <- function(chap_num) {
	validate_chapter_choice(chap_num)
	chapters <- list_chapter_functions()
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

#' @title Reload chapter functions
#' @description Reloads functions from a chapter
#' @param chap_num Number of book chapter (from 2 to 10)
#' @author Waldir Leoncio
#' @export
#' @details This is a wrapper for `unload_chapter()` followed by
#' `load_chapter()`. It was made to ease the package development
#' process, but might be useful for end users who thinks something
#' might have gone wrong when loading/unloading a chapter and wishes
#' to reload it.
reload_chapter <- function(chap_num) {
	unload_chapter(chap_num)
	load_chapter(chap_num)
}

# ============================================================================ #
# Internal functions used in this file                                         #
# ============================================================================ #
validate_chapter_choice <- function(chap_num) {
	# Makes sure the user chooses a proper chapter number
	if (missing(chap_num)) stop("Please choose a chapter between 2 and 10.")
	if (chap_num < 2 | chap_num > 10) {
		stop(
			"There is no Chapter ", chap_num,
			". Please choose a chapter between 2 and 10."
		)
	}
}

list_chapter_functions <- function() {
	# Lists of functions pertaining to a certain chapter
	ch2 <- list(
		AgrestiCoull_CI_1x2         = AgrestiCoull_CI_1x2,
		Arcsine_CI_1x2              = Arcsine_CI_1x2,
		Wald_CI_1x2                 = Wald_CI_1x2,
		Blaker_exact_CI_1x2         = Blaker_exact_CI_1x2,
		Blaker_exact_test_1x2       = Blaker_exact_test_1x2,
		Blaker_midP_CI_1x2          = Blaker_midP_CI_1x2,
		Blaker_midP_test_1x2        = Blaker_midP_test_1x2,
		ClopperPearson_exact_CI_1x2 = ClopperPearson_exact_CI_1x2,
		ClopperPearson_midP_CI_1x2  = ClopperPearson_midP_CI_1x2,
		Exact_binomial_test_1x2     = Exact_binomial_test_1x2,
		Jeffreys_CI_1x2             = Jeffreys_CI_1x2,
		LR_CI_1x2                   = LR_CI_1x2,
		LR_test_1x2                 = LR_test_1x2,
		MidP_binomial_test_1x2      = MidP_binomial_test_1x2,
		Score_test_1x2              = Score_test_1x2,
		Score_test_CC_1x2           = Score_test_CC_1x2,
		Wald_CI_CC_1x2              = Wald_CI_CC_1x2,
		Wilson_score_CI_1x2         = Wilson_score_CI_1x2,
		Wilson_score_CI_CC_1x2      = Wilson_score_CI_CC_1x2,
		the_1x2_table_CIs           = the_1x2_table_CIs,
		Wald_test_1x2               = Wald_test_1x2,
		Wald_test_CC_1x2            = Wald_test_CC_1x2,
		the_1x2_table_tests         = the_1x2_table_tests
	)
	ch3 <- list(
		Chacko_test_1xc                = Chacko_test_1xc,
		Exact_multinomial_test_1xc     = Exact_multinomial_test_1xc,
		Gold_Wald_CIs_1xc              = Gold_Wald_CIs_1xc,
		Goodman_Wald_CIs_1xc           = Goodman_Wald_CIs_1xc,
		Goodman_Wald_CIs_for_diffs_1xc = Goodman_Wald_CIs_for_diffs_1xc,
		Goodman_Wilson_score_CIs_1xc   = Goodman_Wilson_score_CIs_1xc,
		LR_test_1xc                    = LR_test_1xc,
		MidP_multinomial_test_1xc      = MidP_multinomial_test_1xc,
		Pearson_chi_squared_test_1xc   = Pearson_chi_squared_test_1xc,
		QuesenberryHurst_Wilson_score_CIs_1xc =
			QuesenberryHurst_Wilson_score_CIs_1xc,
		the_1xc_table_CIs              = the_1xc_table_CIs,
		the_1xc_table_tests            = the_1xc_table_tests
	)
	ch4 <- list(
		Adjusted_inv_sinh_CI_OR_2x2        = Adjusted_inv_sinh_CI_OR_2x2,
		Adjusted_inv_sinh_CI_ratio_2x2     = Adjusted_inv_sinh_CI_ratio_2x2,
		Adjusted_log_CI_2x2                = Adjusted_log_CI_2x2,
		AgrestiCaffo_CI_2x2                = AgrestiCaffo_CI_2x2,
		Wald_CI_2x2                        = Wald_CI_2x2,
		BaptistaPike_exact_conditional_CI_2x2 =
			BaptistaPike_exact_conditional_CI_2x2,
		BaptistaPike_midP_CI_2x2           = BaptistaPike_midP_CI_2x2,
		Cornfield_exact_conditional_CI_2x2 = Cornfield_exact_conditional_CI_2x2,
		Cornfield_midP_CI_2x2              = Cornfield_midP_CI_2x2,Fisher_exact_test_2x2              = Fisher_exact_test_2x2,
		Exact_unconditional_test_2x2       = Exact_unconditional_test_2x2,Fisher_midP_test_2x2               = Fisher_midP_test_2x2,
		Gart_adjusted_logit_CI_2x2         = Gart_adjusted_logit_CI_2x2,
		Independence_smoothed_logit_CI_2x2 = Independence_smoothed_logit_CI_2x2,
		Inv_sinh_CI_OR_2x2                 = Inv_sinh_CI_OR_2x2,
		Inv_sinh_CI_ratio_2x2              = Inv_sinh_CI_ratio_2x2,
		Katz_log_CI_2x2                    = Katz_log_CI_2x2,
		Koopman_asymptotic_score_CI_2x2    = Koopman_asymptotic_score_CI_2x2,
		LR_test_2x2                        = LR_test_2x2,
		Mee_asymptotic_score_CI_2x2        = Mee_asymptotic_score_CI_2x2,
		MiettinenNurminen_asymptotic_score_CI_difference_2x2 =
			MiettinenNurminen_asymptotic_score_CI_difference_2x2
	)
	return(list(ch2 = ch2, ch3 = ch3, ch4 = ch4))
}

# TODO: address generated check notes (post question on StackOverflow?)