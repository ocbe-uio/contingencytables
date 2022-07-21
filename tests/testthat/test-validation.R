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
})
