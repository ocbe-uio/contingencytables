linear_by_linear_test_rxc(ritland_2007)
n3 <- floor(table_7.7 / 10)  # the table of test-ch7.R, four empty cells
Pearson_correlation_coefficient_rxc(n3)
Kendalls_tau_b_rxc(n3)
set.seed(1562)
Kendalls_tau_b_rxc_bca(n3, nboot = 200)  # as in test-ch7.R
linear_by_linear_test_rxc(table_7.8, 1:3, 1:5)  # help example, scores given
Pearson_correlation_coefficient_rxc(table_7.8)
