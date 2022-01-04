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
		QuesenberryHurst_Wilson_score_CIs_1xc = QuesenberryHurst_Wilson_score_CIs_1xc,
		the_1xc_table_CIs              = the_1xc_table_CIs,
		the_1xc_table_tests            = the_1xc_table_tests
	)
	ch4 <- list(
		Adjusted_inv_sinh_CI_OR_2x2        = Adjusted_inv_sinh_CI_OR_2x2,
		Adjusted_inv_sinh_CI_ratio_2x2     = Adjusted_inv_sinh_CI_ratio_2x2,
		Adjusted_log_CI_2x2                = Adjusted_log_CI_2x2,
		AgrestiCaffo_CI_2x2                = AgrestiCaffo_CI_2x2,
		Wald_CI_2x2                        = Wald_CI_2x2,
		BaptistaPike_exact_conditional_CI_2x2 = BaptistaPike_exact_conditional_CI_2x2,
		BaptistaPike_midP_CI_2x2           = BaptistaPike_midP_CI_2x2,
		Cornfield_exact_conditional_CI_2x2 = Cornfield_exact_conditional_CI_2x2,
		Cornfield_midP_CI_2x2              = Cornfield_midP_CI_2x2,
		Fisher_exact_test_2x2              = Fisher_exact_test_2x2,
		Exact_unconditional_test_2x2       = Exact_unconditional_test_2x2,
		Fisher_midP_test_2x2               = Fisher_midP_test_2x2,
		Gart_adjusted_logit_CI_2x2         = Gart_adjusted_logit_CI_2x2,
		Independence_smoothed_logit_CI_2x2 = Independence_smoothed_logit_CI_2x2,
		Inv_sinh_CI_OR_2x2                 = Inv_sinh_CI_OR_2x2,
		Inv_sinh_CI_ratio_2x2              = Inv_sinh_CI_ratio_2x2,
		Katz_log_CI_2x2                    = Katz_log_CI_2x2,
		Koopman_asymptotic_score_CI_2x2    = Koopman_asymptotic_score_CI_2x2,
		LR_test_2x2                        = LR_test_2x2,
		Mee_asymptotic_score_CI_2x2        = Mee_asymptotic_score_CI_2x2,
		MiettinenNurminen_asymptotic_score_CI_difference_2x2 = MiettinenNurminen_asymptotic_score_CI_difference_2x2,
		MiettinenNurminen_asymptotic_score_CI_OR_2x2 = MiettinenNurminen_asymptotic_score_CI_OR_2x2,
		MiettinenNurminen_asymptotic_score_CI_ratio_2x2 = MiettinenNurminen_asymptotic_score_CI_ratio_2x2,
		MOVER_R_Wilson_CI_OR_2x2           = MOVER_R_Wilson_CI_OR_2x2,
		MOVER_R_Wilson_CI_ratio_2x2        = MOVER_R_Wilson_CI_ratio_2x2,
		Newcombe_hybrid_score_CI_2x2       = Newcombe_hybrid_score_CI_2x2,
		Pearson_chi_squared_test_2x2       = Pearson_chi_squared_test_2x2,
		Pearson_chi_squared_test_CC_2x2    = Pearson_chi_squared_test_CC_2x2,
		PriceBonett_approximate_Bayes_CI_2x2 = PriceBonett_approximate_Bayes_CI_2x2,
		Wald_CI_CC_2x2                     = Wald_CI_CC_2x2,
		Woolf_logit_CI_2x2                 = Woolf_logit_CI_2x2,
		Uncorrected_asymptotic_score_CI_2x2 = Uncorrected_asymptotic_score_CI_2x2,
		Z_unpooled_test_2x2                = Z_unpooled_test_2x2,
		the_2x2_table_CIs_difference       = the_2x2_table_CIs_difference,
		the_2x2_table_CIs_OR               = the_2x2_table_CIs_OR,
		the_2x2_table_CIs_ratio            = the_2x2_table_CIs_ratio,
		the_2x2_table_tests                = the_2x2_table_tests
	)
	ch5 <- list(
		CochranArmitage_CI_rx2 = CochranArmitage_CI_rx2,
		CochranArmitage_exact_cond_midP_tests_rx2 = CochranArmitage_exact_cond_midP_tests_rx2,
		CochranArmitage_MH_tests_rx2 = CochranArmitage_MH_tests_rx2,
		Exact_cond_midP_unspecific_ordering_rx2 = Exact_cond_midP_unspecific_ordering_rx2,
		Pearson_LR_tests_unspecific_ordering_rx2 = Pearson_LR_tests_unspecific_ordering_rx2,
		the_rx2_table = the_rx2_table,
		Trend_estimate_CI_tests_rx2 = Trend_estimate_CI_tests_rx2
	)
	ch6 <- list(
		Brant_test_2xc = Brant_test_2xc,
		Cumulative_models_for_2xc = Cumulative_models_for_2xc,
		Exact_cond_midP_linear_rank_tests_2xc = Exact_cond_midP_linear_rank_tests_2xc,
		ClopperPearson_exact_CI_1x2_beta_version = ClopperPearson_exact_CI_1x2_beta_version,
		Exact_cond_midP_unspecific_ordering_rx2 = Exact_cond_midP_unspecific_ordering_rx2,
		MantelHaenszel_test_2xc = MantelHaenszel_test_2xc,
		Pearson_LR_tests_cum_OR_2xc = Pearson_LR_tests_cum_OR_2xc,
		Score_test_for_effect_in_the_probit_model_2xc = Score_test_for_effect_in_the_probit_model_2xc,
		the_2xc_table = the_2xc_table
	)
	ch7 <- list(
		Bonferroni_type_CIs_rxc = Bonferroni_type_CIs_rxc,
		Cumulative_models_for_rxc = Cumulative_models_for_rxc,
		Exact_cond_midP_tests_rxc = Exact_cond_midP_tests_rxc,
		FisherFreemanHalton_asymptotic_test_rxc = FisherFreemanHalton_asymptotic_test_rxc,
		gamma_coefficient_rxc_bca = gamma_coefficient_rxc_bca,
		gamma_coefficient_rxc = gamma_coefficient_rxc,
		JonckheereTerpstra_test_rxc = JonckheereTerpstra_test_rxc,
		Kendalls_tau_b_rxc = Kendalls_tau_b_rxc,
		Kendalls_tau_b_rxc_bca = Kendalls_tau_b_rxc_bca,
		KruskalWallis_asymptotic_test_rxc = KruskalWallis_asymptotic_test_rxc,
		linear_by_linear_test_rxc = linear_by_linear_test_rxc,
		Pearson_correlation_coefficient_rxc = Pearson_correlation_coefficient_rxc,
		Pearson_correlation_coefficient_rxc_bca = Pearson_correlation_coefficient_rxc_bca,
		Pearson_LR_tests_rxc = Pearson_LR_tests_rxc,
		Pearson_residuals_rxc = Pearson_residuals_rxc,
		Scheffe_type_CIs_rxc = Scheffe_type_CIs_rxc,
		Spearman_correlation_coefficient_rxc = Spearman_correlation_coefficient_rxc,
		Spearman_correlation_coefficient_rxc_bca = Spearman_correlation_coefficient_rxc_bca,
		the_rxc_table = the_rxc_table
	)
	ch8 <- list(
		BonettPrice_hybrid_Wilson_score_CI_CC_paired_2x2 = BonettPrice_hybrid_Wilson_score_CI_CC_paired_2x2,
		BonettPrice_hybrid_Wilson_score_CI_paired_2x2 = BonettPrice_hybrid_Wilson_score_CI_paired_2x2,
		ClopperPearson_exact_CI_1x2_beta_version = ClopperPearson_exact_CI_1x2_beta_version,
		McNemar_asymptotic_test_CC_paired_2x2 = McNemar_asymptotic_test_CC_paired_2x2,
		McNemar_asymptotic_test_paired_2x2 = McNemar_asymptotic_test_paired_2x2,
		McNemar_exact_cond_test_paired_2x2 = McNemar_exact_cond_test_paired_2x2,
		McNemar_exact_unconditional_test_paired_2x2 = McNemar_exact_unconditional_test_paired_2x2,
		McNemar_midP_test_paired_2x2 = McNemar_midP_test_paired_2x2,
		Tang_asymptotic_score_CI_paired_2x2 = Tang_asymptotic_score_CI_paired_2x2,
		Tango_asymptotic_score_CI_paired_2x2 = Tango_asymptotic_score_CI_paired_2x2,
		MOVER_Wilson_score_CI_paired_2x2 = MOVER_Wilson_score_CI_paired_2x2,
		Newcombe_square_and_add_CI_paired_2x2 = Newcombe_square_and_add_CI_paired_2x2,
		Transformed_Blaker_exact_CI_paired_2x2 = Transformed_Blaker_exact_CI_paired_2x2,
		Transformed_Clopper_Pearson_exact_CI_paired_2x2 = Transformed_Clopper_Pearson_exact_CI_paired_2x2,
		Transformed_Clopper_Pearson_midP_CI_paired_2x2 = Transformed_Clopper_Pearson_midP_CI_paired_2x2,
		Transformed_Wilson_score_CI_paired_2x2 = Transformed_Wilson_score_CI_paired_2x2,
		Wald_CI_diff_paired_2x2 = Wald_CI_diff_paired_2x2,
		Wald_CI_diff_CC_paired_2x2 = Wald_CI_diff_CC_paired_2x2,
		Wald_CI_AgrestiMin_paired_2x2 = Wald_CI_AgrestiMin_paired_2x2,
		Wald_CI_BonettPrice_paired_2x2 = Wald_CI_BonettPrice_paired_2x2,
		Wald_CI_OR_Laplace_paired_2x2 = Wald_CI_OR_Laplace_paired_2x2,
		Wald_CI_OR_paired_2x2 = Wald_CI_OR_paired_2x2,
		Wald_CI_ratio_paired_2x2 = Wald_CI_ratio_paired_2x2,
		the_paired_2x2_table_CIs_difference = the_paired_2x2_table_CIs_difference,
		the_paired_2x2_table_CIs_OR = the_paired_2x2_table_CIs_OR,
		the_paired_2x2_table_CIs_ratio = the_paired_2x2_table_CIs_ratio,
		the_paired_2x2_table_tests = the_paired_2x2_table_tests
	)
	ch9 <- list(
		Bhapkar_test_paired_cxc = Bhapkar_test_paired_cxc,
		Bonferroni_type_CIs_paired_cxc = Bonferroni_type_CIs_paired_cxc,
		FleissEveritt_test_paired_cxc = FleissEveritt_test_paired_cxc,
		FleissLevinPaik_test_paired_cxc = FleissLevinPaik_test_paired_cxc,
		McNemarBowker_test_paired_cxc = McNemarBowker_test_paired_cxc,
		Scheffe_type_CIs_paired_cxc = Scheffe_type_CIs_paired_cxc,
		Score_test_and_CI_marginal_mean_scores_paired_cxc = Score_test_and_CI_marginal_mean_scores_paired_cxc,
		Stuart_test_paired_cxc = Stuart_test_paired_cxc,
		Wald_test_and_CI_marginal_mean_ranks_paired_cxc = Wald_test_and_CI_marginal_mean_ranks_paired_cxc,
		Wald_test_and_CI_marginal_mean_scores_paired_cxc = Wald_test_and_CI_marginal_mean_scores_paired_cxc,
		the_paired_cxc_table_nominal = the_paired_cxc_table_nominal,
		the_paired_cxc_table_ordinal = the_paired_cxc_table_ordinal
	)
	ch10 <- list(
		BreslowDay_homogeneity_test_stratified_2x2 = BreslowDay_homogeneity_test_stratified_2x2,
		CochranMantelHaenszel_test_stratified_2x2 = CochranMantelHaenszel_test_stratified_2x2,
		MantelHaenszel_estimate_stratified_2x2 = MantelHaenszel_estimate_stratified_2x2
	)
	return(
		list(
			ch2 = ch2, ch3 = ch3, ch4 = ch4, ch5 = ch5,
			ch6 = ch6, ch7 = ch7,	ch8 = ch8, ch9 = ch9, ch10 = ch10
		)
	)
}
