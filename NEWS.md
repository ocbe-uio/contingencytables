# contingencytables 3.0.1

* Adapted unit tests to new version of `boot`

# contingencytables 3.0.0

* Reached 100% test coverage (#52)
* Removed unused code
* Improved handling of edge cases
* Restricted `Cumulative_models_for_rxc()` and `Cumulative_models_for_2xc()` to 2 columns
* Added `plot()` method for `Exact_unconditional_test_2x2()` and `McNemar_exact_unconditional_test_paired_2x2()` (#53)
* Standardized names of objects in the output (#35)

# contingencytables 2.2.1

* Improved internals: refactoring (#41), increased test coverage (#52)
* Fixed upper CI limit on `stratified_2x2_tables()` (thanks to Zhen Wang)

# contingencytables 2.2.0

* Added argument to control `num_pi_values` in `McNemar_exact_unconditional_test_paired_2x2()` and `the_paired_2x2_table_tests()` (#50)
* Improved validation of the `direction` argument (#45)
* Optimized `Exact_cond_midP_unspecific_ordering_rx2()` (#44)
* Optimized internal functions affecting several external functions (#49)
* Removed duplicated code
* Fixed bug on `Fisher_midP_test_2x2()` (#51)

# contingencytables 2.1.1

* Minor styling fixes to code (#48)
* Fixed package documentation

# contingencytables 2.1.0

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
