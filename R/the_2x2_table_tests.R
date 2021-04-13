source("/Users/ole/Documents/Petter Laake/chap4_translation/Pearson_chi_squared_test_2x2.R")
source("/Users/ole/Documents/Petter Laake/chap4_translation/Pearson_chi_squared_test_CC_2x2.R")
source("/Users/ole/Documents/Petter Laake/chap4_translation/LR_test_2x2.R")
source("/Users/ole/Documents/Petter Laake/chap4_translation/Z_unpooled_test_2x2.R")
source("/Users/ole/Documents/Petter Laake/chap4_translation/Fisher_exact_test_2x2.R")
source("/Users/ole/Documents/Petter Laake/chap4_translation/Fisher_midP_test_2x2.R")
source("/Users/ole/Documents/Petter Laake/chap4_translation/Exact_unconditional_test_2x2.R")

the_2x2_table_tests = function(n, gamma=0.0001) {
    # gamma : parameter for the Berger and Boos procedure

    if (missing(n)) {
        # n = rbind(c(3,1), c(1,3))       # Example: A lady tasting a cup of tea
        n = rbind(c(7,27), c(1,33))   # Example: Perondi et al. (2004)
        # n = rbind(c(9,4), c(4,10))    # Example: Lampasona et al. (2013)
        # n = rbind(c(0,16), c(15,57))  # Example: Ritland et al. (2007)
    }

    pi1hat = n[1,1]/(n[1,1] + n[1,2])
    pi2hat = n[2,1]/(n[2,1] + n[2,2])

    print(sprintf('H_0: pi_1 = pi_2  vs  H_A: pi_1 =/= pi_2'), quote=F)
    print(sprintf('Estimate of pi_1: %i/%i = %5.3f', n[1,1], n[1,1] + n[1,2], pi1hat), quote=F)
    print(sprintf('Estimate of pi_2: %i/%i = %5.3f', n[2,1], n[2,1] + n[2,2], pi2hat), quote=F)

    print('Test                                  P-value  (test statistic)', quote=F)
    print('------------------------------------------------------------------', quote=F)
    res = Pearson_chi_squared_test_2x2(n, printresults=F)
    P0 = res$p.value
    T0 = res$statistic
    df = res$df
    print(sprintf('Pearson chi-squared                   %6.4f   (T = %5.3f, df = %i)', P0, T0, df), quote=F)

    res = Pearson_chi_squared_test_CC_2x2(n, printresults=F)
    P0 = res$p.value
    T0 = res$statistic
    df = res$df
    print(sprintf('Pearson chi-squared w/CC              %6.4f   (T = %5.3f, df = %i)', P0, T0, df), quote=F)

    res = LR_test_2x2(n, printresults=F)
    P0 = res$p.value
    T0 = res$statistic
    df = res$df
    print(sprintf('Likelihood ratio                      %6.4f   (T = %5.3f, df = %i)', P0, T0, df), quote=F)

    res = Z_unpooled_test_2x2(n, printresults=F)
    P0 = res$p.value
    Z0 = res$statistic
    print(sprintf('Z-unpooled                            %6.4f   (Z = %5.3f)', P0, Z0), quote=F)

    P0 = Fisher_exact_test_2x2(n, 'hypergeometric', printresults=F)
    print(sprintf('Fisher exact test (Fisher-Irwin)      %6.4f', P0), quote=F)

    P0 = Fisher_exact_test_2x2(n, 'Pearson', printresults=F)
    print(sprintf('Fisher exact test (Pearson)           %6.4f', P0), quote=F)

    P0 = Fisher_exact_test_2x2(n, 'LR', printresults=F)
    print(sprintf('Fisher exact test (LR)                %6.4f', P0), quote=F)

    P0 = Fisher_midP_test_2x2(n, 'hypergeometric', printresults=F)
    print(sprintf('Fisher mid-P test (Fisher-Irwin)      %6.4f', P0), quote=F)

    P0 = Exact_unconditional_test_2x2(n, 'Pearson', gamma, printresults=F)
    print(sprintf('Suissa-Shuster exact uncond.*         %6.4f', P0), quote=F)

    P0 = Exact_unconditional_test_2x2(n, 'LR', gamma, printresults=F)
    print(sprintf('Exact uncond. w/LR statistic*         %6.4f', P0), quote=F)

    P0 = Exact_unconditional_test_2x2(n, 'unpooled', gamma, printresults=F)
    print(sprintf('Exact uncond. w/unpooled Z statistic* %6.4f', P0), quote=F)

    P0 = Exact_unconditional_test_2x2(n, 'Fisher', gamma, printresults=F)
    print(sprintf('Fisher-Boschloo exact uncond.*        %6.4f', P0), quote=F)

    print('------------------------------------------------------------------', quote=F)
    print(sprintf('*gamma = %-10.8g', gamma), quote=F)

}
