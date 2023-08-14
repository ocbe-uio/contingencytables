# contingencytables (development version)

* Improved error handling (#46)
* Optimized `Exact_multinomial_test_1xc()` and `MidP_multinomial_test_1xc()` (#13)
* Optimized `Exact_cond_midP_linear_rank_tests_2xc()` (#39)
* Added default value to `a` as scores for rows (outcomes) (#33)
* Fixed `Exact_cond_midP_tests_rxc()` and `FisherFreemanHalton_asymptotic_test_rxc()` for margins with larger (171+) observations (credit to @bjbiggerstaff)

# contingencytables 2.0.0

* Created objects for all common example inputs (#25)
* Added validation to all exported function arguments (#14)
* Refactored code (#27)
* Fixed examples
* Unwrapped several examples from `\dontrun{}` (#28)
* Improved output of all functions (#31)

# contingencytables 1.0.4

* Fixed bug on `BaptistaPike_midP_CI_2x2()` (issue #37)

# contingencytables 1.0.3

* Fixed bug on `Chacko_test_1xc()` (issue #36)

# contingencytables 1.0.2

* Fixed bug on `Pearson_chi_squared_test_CC_2x2()` (see issue #34 for details)
* Refactoring (see issues #26 and #30 for details)
* Fixes bug on `the_2x2_table_CIs_ratio()` (see commit 9d3212f for details)
* Improvements to Github documentation and CI workflow

# contingencytables 1.0.1

* Added a `NEWS.md` file to track changes to the package.
* Fixes bug (see issue #32)

# contingencytables 1.0.0

* Initial CRAN release.
