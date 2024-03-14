# contingencytables 3.0.0

* Reached 100% test coverage (#52)
* Removed unused code
* Improved handling of edge cases
* Restricted `Cumulative_models_for_rxc()` and `Cumulative_models_for_2xc()` to 2 columns
* Added `plot()` method for `Exact_unconditional_test_2x2()` and `McNemar_exact_unconditional_test_paired_2x2()` (#53)
* Standardized names of objects in the output (#35)

# Test environments
- R-hub windows-x86_64-devel (r-devel)
- R-hub ubuntu-gcc-release (r-release)
- R-hub fedora-clang-devel (r-devel)

# R CMD check results
❯ On windows-x86_64-devel (r-devel)
  checking for non-standard things in the check directory ... NOTE
  Found the following files/directories:
    ''NULL''

❯ On windows-x86_64-devel (r-devel)
  checking for detritus in the temp directory ... NOTE
  Found the following files/directories:
    'lastMiKTeXException'

❯ On ubuntu-gcc-release (r-release)
  checking examples ... [34s/93s] NOTE
  Examples with CPU (user + system) or elapsed time > 5s
                                             user system elapsed
  MidP_multinomial_test_1xc                 3.908  0.001  11.123
  Exact_multinomial_test_1xc                3.810  0.004  10.800
  CochranArmitage_exact_cond_midP_tests_rx2 3.323  0.007   8.749

❯ On ubuntu-gcc-release (r-release), fedora-clang-devel (r-devel)
  checking HTML version of manual ... NOTE
  Skipping checking HTML validation: no command 'tidy' found

❯ On fedora-clang-devel (r-devel)
  checking examples ... [35s/98s] NOTE
  Examples with CPU (user + system) or elapsed time > 5s
                                             user system elapsed
  MidP_multinomial_test_1xc                 4.161  0.008  11.308
  Exact_multinomial_test_1xc                4.079  0.012  10.254
  CochranArmitage_exact_cond_midP_tests_rx2 3.521  0.003  10.113
  the_rxc_table                             2.002  0.008   5.574

0 errors ✔ | 0 warnings ✔ | 5 notes ✖
