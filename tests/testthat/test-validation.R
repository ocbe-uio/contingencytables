context("Argument validation")

test_that("Invalid arguments are picked up", {
  expect_error(Adjusted_inv_sinh_CI_OR_2x2(-ritland_2007), "counts")
  expect_error(Adjusted_inv_sinh_CI_OR_2x2(ritland_2007, -3), "positive")
  expect_error(Adjusted_inv_sinh_CI_OR_2x2(ritland_2007, alpha = 1.1), "prob")
  expect_error(Adjusted_inv_sinh_CI_ratio_2x2(perondi_2004, alpha = 1.1), "pro")
  expect_error(Adjusted_log_CI_2x2(perondi_2004, alpha = 1.1), "pr")
  expect_error(AgrestiCaffo_CI_2x2(perondi_2004, alpha = 1.1), "pr")
  expect_error(AgrestiCoull_CI_1x2(perondi_2004, alpha = 1.1), "pr")
  expect_error(Arcsine_CI_1x2(perondi_2004, alpha = 1.1), "pr")
  expect_error(
    BaptistaPike_exact_conditional_CI_2x2(perondi_2004, alpha = 1.1), "pr"
  )
  expect_error(BaptistaPike_midP_CI_2x2(perondi_2004, alpha = 1.1), "pr")
  expect_error(Bhapkar_test_paired_cxc(-perondi_2004), "counts")
  expect_error(
    Blaker_exact_CI_1x2(ligarden_2010["X"], ligarden_2010["n"], alpha = 1.1),
    "pr"
  )
  expect_error(
    Blaker_exact_test_1x2(ligarden_2010["X"], ligarden_2010["n"], pi0 = 1.1),
    "pr"
  )
  expect_error(
    Blaker_midP_CI_1x2(ligarden_2010["X"], ligarden_2010["n"], alpha = 1.1),
    "pr"
  )
  expect_error(
    Blaker_midP_test_1x2(ligarden_2010["X"], ligarden_2010["n"], pi0 = 1.1),
    "pr"
  )
  expect_error(
    BonettPrice_hybrid_Wilson_score_CI_CC_paired_2x2(cavo_2012, -0.1), "prob"
  )
  expect_error(
    BonettPrice_hybrid_Wilson_score_CI_paired_2x2(cavo_2012, -0.1), "prob"
  )
  expect_error(Bonferroni_type_CIs_paired_cxc(cavo_2012, -0.1), "prob")
  expect_error(Bonferroni_type_CIs_rxc(cavo_2012, -0.1), "prob")
  expect_error(Brant_test_2xc(-fontanella_2008), "counts")
  expect_error(
    BreslowDay_homogeneity_test_stratified_2x2(-fontanella_2008), "counts"
  )
  expect_error(Chacko_test_1xc(-fontanella_2008), "counts")
  expect_error(
    ClopperPearson_exact_CI_1x2_beta_version(
      ligarden_2010["X"], ligarden_2010["n"], 1.1
    ),
    "pr"
  )
  expect_error(
    ClopperPearson_exact_CI_1x2(ligarden_2010["X"], ligarden_2010["n"], 1.1),
    "pr"
  )
  expect_error(
    ClopperPearson_midP_CI_1x2(ligarden_2010["X"], ligarden_2010["n"], 1.1),
    "pr"
  )
  expect_error(Cochran_Q_test_stratified_2x2(hine_1989, "probit"), "logit")
  expect_error(Cochran_Q_test_stratified_2x2(hine_1989, "log", "AA"), "MH")
  expect_error(CochranArmitage_CI_rx2(indredavik_2008, 1:5, 2), "prob")
  expect_error(
    CochranArmitage_exact_cond_midP_tests_rx2(-indredavik_2008, 1:5),
    "Should be counts"
  )
  expect_error(
    CochranArmitage_MH_tests_rx2(-indredavik_2008, 1:5),
    "Should be counts"
  )
  expect_error(
    CochranMantelHaenszel_test_stratified_2x2(-hine_1989), "Should be counts"
  )
  expect_error(
    Cornfield_exact_conditional_CI_2x2(-hine_1989), "Should be counts"
  )
  expect_error(
    Cornfield_midP_CI_2x2(tea, 1.1), "Should be probability"
  )
  expect_error(
    Cumulative_models_for_2xc(lydersen_2012a, alpha = 1.1), "probability"
  )
  expect_error(
    Cumulative_models_for_2xc(lydersen_2012a, "log"),
    "linkfunction contains invalid values. Should be logit, probit or identity"
  )
  expect_error(
    Cumulative_models_for_rxc(lydersen_2012a, "logit", alpha = 1.1), "probability"
  )
  expect_error(
    Cumulative_models_for_rxc(lydersen_2012a, "log"),
    "linkfunction contains invalid values. Should be logit, probit or identity"
  )
  expect_error(
    Exact_binomial_test_1x2(ligarden_2010["X"], ligarden_2010["n"], pi0 = -0.5),
    "Should be probability"
  )
  expect_error(
     Exact_cond_midP_linear_rank_tests_2xc(-lydersen_2012a), "Should be counts"
  )
  expect_error(
    Exact_cond_midP_tests_rxc(-table_7.3), "Should be counts"
  )
  expect_error(
    Exact_cond_midP_unspecific_ordering_rx2(t(lydersen_2012a), "dec"),
    "Should be increasing or decreasing"
  )
  expect_error(
     Exact_multinomial_test_1xc(snp6498169$subset$n, -snp6498169$subset$pi0),
    "Should be probability"
  )
  expect_error(
    Exact_unconditional_test_2x2(tea, "LER"),
    "Should be Pearson, LR, unpooled, Fisher."
  )
  expect_error(
    Fisher_exact_test_2x2(tea, "Fisher"),
    "Should be Pearson, hypergeometric, LR."
  )
  expect_error(
    Fisher_midP_test_2x2(tea, "Fisher"),
    "Should be Pearson, hypergeometric, LR."
  )
  expect_error(
    FisherFreemanHalton_asymptotic_test_rxc(-table_7.3),
    "Should be counts."
  )
  expect_error(
    FleissEveritt_test_paired_cxc(-fleiss_2003),
    "Should be counts."
  )
  expect_error(
    FleissLevinPaik_test_paired_cxc(-peterson_2007[-4, -4]),
    "Should be counts."
  )
  expect_error(
    gamma_coefficient_rxc_bca(table_7.7, -80),
    "nboot contains invalid values. Should be counts."
  )
  expect_error(
    gamma_coefficient_rxc(-table_7.7),
    "n contains invalid values. Should be counts."
  )
  expect_error(
    Gart_adjusted_logit_CI_2x2(-lampasona_2013),
    "n contains invalid values. Should be counts."
  )
  expect_error(
    Gold_Wald_CIs_1xc(snp6498169$complete$n, 1.1),
    "alpha contains invalid values. Should be probability."
  )
  expect_error(
    Goodman_Wald_CIs_1xc(n = -snp6498169$complete$n),
    "n contains invalid values. Should be counts."
  )
  expect_error(
    Goodman_Wald_CIs_for_diffs_1xc(n = snp6498169$complete$n, .1, "Scheffield"),
    "adjustment contains invalid values. Should be Bonferroni, Scheffe."
  )
  expect_error(
    Goodman_Wilson_score_CIs_1xc(-snp6498169$complete$n),
    "n contains invalid values. Should be counts."
  )
  expect_error(
    Independence_smoothed_logit_CI_2x2(-ritland_2007),
    "n contains invalid values. Should be counts."
  )
  expect_error(
    Inv_sinh_CI_OR_2x2(-ritland_2007),
    "n contains invalid values. Should be counts."
  )
  expect_error(
    Inv_sinh_CI_ratio_2x2(-ritland_2007),
    "n contains invalid values. Should be counts."
  )
  expect_error(
    InverseVariance_estimate_stratified_2x2(hine_1989, "probit"),
    "link contains invalid values. Should be linear, log or logit."
  )
  expect_error(
    Jeffreys_CI_1x2(-ligarden_2010["X"], ligarden_2010["n"]),
    "X contains invalid values. Should be counts."
  )
  expect_error(
    JonckheereTerpstra_test_rxc(-table_7.7),
    "n contains invalid values. Should be counts."
  )
  expect_error(
    Katz_log_CI_2x2(-ritland_2007),
    "n contains invalid values. Should be counts."
  )
  expect_error(
    Kendalls_tau_b_rxc_bca(table_7.9, -1),
    "nboot contains invalid values. Should be counts."
  )
  expect_error(
    Kendalls_tau_b_rxc(-table_7.8),
    "n contains invalid values. Should be counts."
  )
  expect_error(
    Koopman_asymptotic_score_CI_2x2(-ritland_2007),
    "n contains invalid values. Should be counts."
  )
  expect_error(
    KruskalWallis_asymptotic_test_rxc(-table_7.6),
    "n contains invalid values. Should be counts."
  )
  expect_error(
    linear_by_linear_test_rxc(-table_7.7),
    "n contains invalid values. Should be counts."
  )
  expect_error(
    LR_CI_1x2(ligarden_2010["X"], -ligarden_2010["n"]),
    "n contains invalid values. Should be counts."
  )
  expect_error(
    LR_test_1x2(ligarden_2010["X"], ligarden_2010["n"], pi0 = -.5),
    "pi0 contains invalid values. Should be probability."
  )
  expect_error(
    LR_test_1xc(n = snp6498169$subset$n, pi0 = -snp6498169$subset$pi0),
    "pi0 contains invalid values. Should be probability."
  )
  expect_error(
    LR_test_2x2(-ritland_2007),
    "n contains invalid values. Should be counts."
  )
  expect_error(
    MantelHaenszel_estimate_stratified_2x2(hine_1989, 1),
    "link contains invalid values. Should be linear, log or logit."
  )
  expect_error(
    MantelHaenszel_test_2xc(-lydersen_2012a),
    "n contains invalid values. Should be counts."
  )
  expect_error(
    McNemar_asymptotic_test_CC_paired_2x2(-ezra_2010),
    "n contains invalid values. Should be counts."
  )
  expect_error(
    McNemar_asymptotic_test_paired_2x2(-ezra_2010),
    "n contains invalid values. Should be counts."
  )
  expect_error(
    McNemar_exact_cond_test_paired_2x2(-ezra_2010),
    "n contains invalid values. Should be counts."
  )
  expect_error(
    McNemar_exact_unconditional_test_paired_2x2(-bentur_2009),
    "n contains invalid values. Should be counts."
  )
  expect_error(
    McNemar_midP_test_paired_2x2(-ezra_2010),
    "n contains invalid values. Should be counts."
  )
  expect_error(
    McNemarBowker_test_paired_cxc(-peterson_2007),
    "n contains invalid values. Should be counts."
  )
  expect_error(
   Mee_asymptotic_score_CI_2x2(ritland_2007, 9) ,
    "alpha contains invalid values. Should be probability."
  )
  expect_error(
    MidP_binomial_test_1x2(ligarden_2010["X"], ligarden_2010["n"], pi0 = -.5),
    "pi0 contains invalid values. Should be probability."
  )
  expect_error(
    MidP_multinomial_test_1xc(-snp6498169$subset$n, snp6498169$subset$pi0),
    "n contains invalid values. Should be counts."
  )
  expect_error(
    MiettinenNurminen_asymptotic_score_CI_difference_2x2(-perondi_2004),
    "n contains invalid values. Should be counts."
  )
  expect_error(
    MiettinenNurminen_asymptotic_score_CI_OR_2x2(-ritland_2007),
    "n contains invalid values. Should be counts."
  )
  expect_error(
    MiettinenNurminen_asymptotic_score_CI_ratio_2x2(-ritland_2007),
    "n contains invalid values. Should be counts."
  )
  expect_error(
    ML_estimates_and_CIs_stratified_2x2(hine_1989, "probit"),
    "link contains invalid values. Should be linear, log or logit."
  )
  expect_error(
    MOVER_R_Wilson_CI_OR_2x2(-ritland_2007),
    "n contains invalid values. Should be counts."
  )
  expect_error(
    MOVER_R_Wilson_CI_ratio_2x2(-ritland_2007),
    "n contains invalid values. Should be counts."
  )
  expect_error(
    MOVER_Wilson_score_CI_paired_2x2(cavo_2012, 5),
    "alpha contains invalid values. Should be probability."
  )
  expect_error(
    Newcombe_hybrid_score_CI_2x2(ritland_2007, 1.1),
    "alpha contains invalid values. Should be probability."
  )
  expect_error(
    Newcombe_square_and_add_CI_paired_2x2(cavo_2012, 3),
    "alpha contains invalid values. Should be probability."
  )
  expect_error(
    Pearson_chi_squared_test_1xc(snp6498169$subset$n, -snp6498169$subset$pi0),
    "pi0 contains invalid values. Should be probability."
  )
  expect_error(
    Pearson_chi_squared_test_2x2(-ritland_2007),
    "n contains invalid values. Should be counts."
  )
  expect_error(
    Pearson_chi_squared_test_CC_2x2(-tea),
    "n contains invalid values. Should be counts."
  )
  expect_error(
    Pearson_correlation_coefficient_rxc_bca(table_7.7, -1),
    "nboot contains invalid values. Should be counts."
  )
  expect_error(
    Pearson_correlation_coefficient_rxc(table_7.7, alpha = 1.1),
    "alpha contains invalid values. Should be probability."
  )
  expect_error(
    Pearson_LR_homogeneity_test_stratified_2x2(doll_hill_1950, "probit"),
    "link contains invalid values. Should be linear, log or logit."
  )
  expect_error(
    Pearson_LR_test_common_effect_stratified_2x2(doll_hill_1950, "probit"),
    "link contains invalid values. Should be linear, log or logit."
  )
  expect_error(
    Pearson_LR_tests_cum_OR_2xc(lydersen_2012a, "blah"),
    "direction contains invalid values. Should be increasing or decreasing."
  )
  expect_error(
    Pearson_LR_tests_rxc(-table_7.5),
    "n contains invalid values. Should be counts."
  )
  expect_error(
    Pearson_LR_tests_unspecific_ordering_rx2(indredavik_2008, "decrease"),
    "direction contains invalid values. Should be increasing or decreasing."
  )
  expect_error(
    Pearson_residuals_rxc(-table_7.5),
    "n contains invalid values. Should be counts."
  )
  expect_error(
    Peto_homogeneity_test_stratified_2x2(hine_1989 / 3),
    "n contains invalid values. Should be counts."
  )
  expect_error(
    Peto_OR_estimate_stratified_2x2(hine_1989 / 7),
    "n contains invalid values. Should be counts."
  )
  expect_error(
    PriceBonett_approximate_Bayes_CI_2x2(ritland_2007 / 8),
    "n contains invalid values. Should be counts."
  )
  expect_error(
    QuesenberryHurst_Wilson_score_CIs_1xc(snp6498169$complete$n, 5),
    "alpha contains invalid values. Should be probability."
  )
  expect_error(
    RBG_test_and_CI_stratified_2x2(doll_hill_1950, 54),
    "alpha contains invalid values. Should be probability."
  )
  expect_error(
    Scheffe_type_CIs_paired_cxc(peterson_2007 / 7),
    "n contains invalid values. Should be counts."
  )
  expect_error(
    Scheffe_type_CIs_rxc(table_7.3, "A"),
    "alpha contains invalid values. Should be probability."
  )
  expect_error(
    Score_test_1x2(ligarden_2010["X"], ligarden_2010["n"], pi0 = 5),
    "pi0 contains invalid values. Should be probability."
  )
  expect_error(
    Score_test_and_CI_marginal_mean_scores_paired_cxc(
      fischer_1999, c(8, 3.5, 0, -3.5, -8), pi
    ),
    "alpha contains invalid values. Should be probability."
  )
  expect_error(
    Score_test_CC_1x2(ligarden_2010["X"], ligarden_2010["n"], pi0 = 5),
    "pi0 contains invalid values. Should be probability."
  )
  expect_error(
    Score_test_for_effect_in_the_probit_model_2xc(
      -fontanella_2008, c(-1.246452, -0.5097363, 0.2087471)
    ),
    "n contains invalid values. Should be counts."
  )
  expect_error(
    Spearman_correlation_coefficient_rxc_bca(-table_7.7),
    "n contains invalid values. Should be counts."
  )
  expect_error(
    Spearman_correlation_coefficient_rxc(table_7.7, 3),
    "alpha contains invalid values. Should be probability."
  )
  expect_error(
    stratified_2x2_tables(hine_1989 / 7),
    "n contains invalid values. Should be counts."
  )
  expect_error(
    Stuart_test_paired_cxc(peterson_2007 / 7),
    "n contains invalid values. Should be counts."
  )
  expect_error(
    Tang_asymptotic_score_CI_paired_2x2(cavo_2012 / 7),
    "n contains invalid values. Should be counts."
  )
  expect_error(
    Tango_asymptotic_score_CI_paired_2x2(cavo_2012 / 6),
    "n contains invalid values. Should be counts."
  )
  expect_error(
    the_1x2_table_CIs(ligarden_2010["X"], -ligarden_2010["n"]),
    "n contains invalid values. Should be counts."
  )
  expect_error(
    the_1x2_table_tests(ligarden_2010["X"], ligarden_2010["n"], pi0 = -0.5),
    "pi0 contains invalid values. Should be probability."
  )
  expect_error(
    the_1xc_table_CIs(-snp6498169$complete$n),
   "n contains invalid values. Should be counts."
  )
  expect_error(
    the_1xc_table_tests(-snp6498169$complete$n, snp6498169$complete$pi0),
    "n contains invalid values. Should be counts."
  )
  expect_error(
    the_2x2_table_CIs_difference(perondi_2004, 7),
    "alpha contains invalid values. Should be probability."
  )
  expect_error(
    the_2x2_table_CIs_OR(ritland_2007, 7),
    "alpha contains invalid values. Should be probability."
  )
  expect_error(
    the_2x2_table_CIs_ratio(ritland_2007 / 6),
    "n contains invalid values. Should be counts."
  )
  expect_error(
    the_2x2_table_tests(lampasona_2013 / 7),
    "n contains invalid values. Should be counts."
  )
  expect_error(
    the_2xc_table(lydersen_2012a, direction = "decrease"),
    "direction contains invalid values. Should be increasing or decreasing."
  )
  expect_error(
    the_paired_2x2_table_CIs_difference(cavo_2012 / 7),
    "n contains invalid values. Should be counts."
  )
  expect_error(
    the_paired_2x2_table_CIs_OR(ezra_2010 / 7),
    "n contains invalid values. Should be counts."
  )
  expect_error(
    the_paired_2x2_table_CIs_ratio(cavo_2012 / 7),
    "n contains invalid values. Should be counts."
  )
  expect_error(
    the_paired_2x2_table_tests(bentur_2009 / 8),
    "n contains invalid values. Should be counts."
  )
  expect_error(
    the_paired_cxc_table_nominal(peterson_2007, 6),
    "alpha contains invalid values. Should be probability."
  )
  expect_error(
    the_paired_cxc_table_ordinal(-fischer_1999, c(8, 3.5, 0, -3.5, -8)),
    "n contains invalid values. Should be counts."
  )
  expect_error(
    the_rx2_table(-mills_graubard_1987, .4, "increasing"),
    "n contains invalid values. Should be counts."
  )
  expect_error(
    the_rx2_table(indredavik_2008, 4),
    "alpha contains invalid values. Should be probability."
  )
  expect_error(
    the_rxc_table(-table_7.9),
    "n contains invalid values. Should be counts."
  )
  expect_error(
    Transformed_Blaker_exact_CI_paired_2x2(ezra_2010/ 7),
    "n contains invalid values. Should be counts."
  )
  expect_error(
    Transformed_Clopper_Pearson_exact_CI_paired_2x2(ezra_2010 / 7),
    "n contains invalid values. Should be counts."
  )
  expect_error(
    Transformed_Clopper_Pearson_midP_CI_paired_2x2(ezra_2010 / 7),
    "n contains invalid values. Should be counts."
  )
  expect_error(
    Transformed_Wilson_score_CI_paired_2x2(ezra_2010/ 6),
    "n contains invalid values. Should be counts."
  )
  expect_error(
     Trend_estimate_CI_tests_rx2(indredavik_2008, 1:5, "logit", 5),
    "alpha contains invalid values. Should be probability."
  )
  expect_error(
    Uncorrected_asymptotic_score_CI_2x2(ritland_2007 / 8),
    "n contains invalid values. Should be counts."
  )
  expect_error(
    Wald_CI_1x2(singh_2010["1st", "X"], -singh_2010["1st", "n"]),
    "n contains invalid values. Should be counts."
  )
  expect_error(
    Wald_CI_2x2(ritland_2007, 6),
    "alpha contains invalid values. Should be probability."
  )
  expect_error(
    Wald_CI_AgrestiMin_paired_2x2(cavo_2012 / 7),
    "n contains invalid values. Should be counts."
  )
  expect_error(
    Wald_CI_BonettPrice_paired_2x2(cavo_2012, 6),
    "alpha contains invalid values. Should be probability."
  )
  expect_error(
    Wald_CI_CC_1x2(ligarden_2010["X"] / 7, ligarden_2010["n"]),
    "X contains invalid values. Should be counts."
  )
  expect_error(
     Wald_CI_CC_2x2(ritland_2007 / 8),
    "n contains invalid values. Should be counts."
  )
  expect_error(
    Wald_CI_diff_CC_paired_2x2(cavo_2012 / 6),
    "n contains invalid values. Should be counts."
  )
  expect_error(
    Wald_CI_diff_paired_2x2(cavo_2012 / 7),
    "n contains invalid values. Should be counts."
  )
  expect_error(
    Wald_CI_OR_Laplace_paired_2x2(ezra_2010, 5),
    "alpha contains invalid values. Should be probability."
  )
  expect_error(
    Wald_CI_OR_paired_2x2(ezra_2010 / 7),
    "n contains invalid values. Should be counts."
  )
  expect_error(
    Wald_CI_ratio_paired_2x2(cavo_2012 / 8),
    "n contains invalid values. Should be counts."
  )
  expect_error(
    Wald_test_1x2(ligarden_2010["X"], -ligarden_2010["n"], pi0 = 0.1),
    "n contains invalid values. Should be counts."
  )
  expect_error(
    Wald_test_and_CI_common_diff_stratified_2x2(hine_1989, "HH"),
    "estimatetype contains invalid values. Should be MH or IV."
  )
  expect_error(
    Wald_test_and_CI_common_ratio_stratified_2x2(-hine_1989),
    "n contains invalid values. Should be counts."
  )
  expect_error(
    Wald_test_and_CI_marginal_mean_ranks_paired_cxc(fischer_1999 / 7),
    "n contains invalid values. Should be counts."
  )
  expect_error(
    Wald_test_and_CI_marginal_mean_scores_paired_cxc(
      fischer_1999, c(8, 3.5, 0, -3.5, -8), 4
    ),
    "alpha contains invalid values. Should be probability."
  )
  expect_error(
    Wald_test_CC_1x2(ligarden_2010["X"], ligarden_2010["n"], pi0 = -1),
    "pi0 contains invalid values. Should be probability."
  )
  expect_error(
    Wilson_score_CI_1x2(singh_2010["1st", "X"], -singh_2010["1st", "n"]),
    "n contains invalid values. Should be counts."
  )
  expect_error(
    Wilson_score_CI_CC_1x2(ligarden_2010["X"] / 7, ligarden_2010["n"]),
    "X contains invalid values. Should be counts."
  )
  expect_error(
    Woolf_logit_CI_2x2(ritland_2007, 7),
    "alpha contains invalid values. Should be probability."
  )
  expect_error(
    Woolf_test_and_CI_stratified_2x2(hine_1989 / 7),
    "n contains invalid values. Should be counts."
  )
  expect_error(
    Z_unpooled_test_2x2(ritland_2007 / 8),
    "n contains invalid values. Should be counts."
  )
})
