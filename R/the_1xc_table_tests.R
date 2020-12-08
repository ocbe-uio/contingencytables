setwd("/Users/ole/Documents/Petter Laake/chap3")
source("Chacko_test_1xc.R")
source("Pearson_chi_squared_test_1xc.R")
source("LR_test_1xc.R")
source("Exact_multinomial_test_1xc.R")
source("MidP_multinomial_test_1xc.R")

the_1xc_table_tests = function(n, pi0) {

    if (missing(pi0)) {
        n = c(276, 380, 118) # Example: Genotype counts for SNP rs 6498169 in RA patients
        pi0 = c(0.402, 0.479, 0.119)
        chacko.test=F
        
        # n = c(6, 1, 3) # Example: subset of 10 patients
        # pi0 = c(0.402, 0.479, 0.119)
        # chacko.test=F
        
        # n = c(1, 4, 3, 11, 9) # Example for the Chacko test: Hypothetical experiment
        # chacko.test = T
    }

    if (chacko.test) {
        Chacko_test_1xc(n, printresults=T)
    } else {
        c0 = length(n)
        N = sum(n)
        for (i in 1:c0) {
            print(sprintf('Estimate of pi_%i: %2g/%2g = %5.3f (pi_%i,0 = %5.3f)', i, n[i], N, n[i]/N, i, pi0[i]), quote=F)
        }
        print(sprintf('Method                 P-value  (test statistic)'), quote=F)
        print('---------------------------------------------------', quote=F)

        res = Pearson_chi_squared_test_1xc(n, pi0, printresults=F)
        print(sprintf('Pearson chi-squared    %6.4f   (T = %5.3f, df = %i)', res$P, res$T, res$df), quote=F)

        res =  LR_test_1xc(n, pi0, printresults=F)
        print(sprintf('Likelihood ratio       %6.4f   (T = %5.3f, df = %i)', res$P, res$T, res$df), quote=F)

        if (N < 774) {
            res = Exact_multinomial_test_1xc(n, pi0, printresults=F)
            print(sprintf('Exact multinomial      %6.4f\n', res), quote=F)

            res = MidP_multinomial_test_1xc(n, pi0, printresults=F)
            print(sprintf('Mid-P multinomial      %6.4f\n', res), quote=F)
        }
    
        print('---------------------------------------------------', quote=F)    
    }

}
