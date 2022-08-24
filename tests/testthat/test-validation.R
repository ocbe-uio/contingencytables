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
    Cumulative_models_for_2xc(lydersen_2012a, "log"), "Should be logit or pr"
  )
  expect_error(
    Cumulative_models_for_rxc(lydersen_2012a, "logit", alpha = 1.1), "probability"
  )
  expect_error(
    Cumulative_models_for_rxc(lydersen_2012a, "log"), "Should be logit or pr"
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
    gamma_coefficient_rxc_bca(table_7.7, 0),
    "nboot contains invalid values. Should be positive."
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
    Kendalls_tau_b_rxc_bca(table_7.9, 0),
    "nboot contains invalid values. Should be positive."
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
})
