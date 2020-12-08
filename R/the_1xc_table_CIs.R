setwd("/Users/ole/Documents/Petter Laake/chap3")
source("Gold_Wald_CIs_1xc.R")
source("Goodman_Wald_CIs_1xc.R")
source("QuesenberryHurst_Wilson_score_CIs_1xc.R")
source("Goodman_Wilson_score_CIs_1xc.R")
source("Goodman_Wald_CIs_for_diffs_1xc.R")

the_1xc_table_CIs = function(n, alpha=0.05) {

    if (missing(n)) {
        n = c(276, 380, 118) # Example: Genotype counts for SNP rs 6498169 in RA patients
    }

    c0 = length(n)

    print(sprintf('Interval method                 Simultaneous CIs     width'), quote=F)
    print('----------------------------------------------------------', quote=F)

    res1 = Gold_Wald_CIs_1xc(n, alpha, printresults=F)
    res2 = Goodman_Wald_CIs_1xc(n, alpha, printresults=F)
    res3 = QuesenberryHurst_Wilson_score_CIs_1xc(n, alpha, printresults=F)
    res4 = Goodman_Wilson_score_CIs_1xc(n, alpha, printresults=F)
    for (i in 1:c0) {
        print(sprintf('Estimate of pi_%i: %6.4f', i, res1$estimate[i]), quote=F)
        print(sprintf('Gold Wald                       %6.4f to %6.4f    %6.4f', 
            res1$lower[i], res1$upper[i], res1$upper[i]-res1$lower[i]), quote=F)
        print(sprintf('Goodman Wald                    %6.4f to %6.4f    %6.4f', 
            res2$lower[i], res2$upper[i], res2$upper[i]-res2$lower[i]), quote=F)
        print(sprintf('Quesenberry-Hurst Wilson score  %6.4f to %6.4f    %6.4f', 
            res3$lower[i], res3$upper[i], res3$upper[i]-res3$lower[i]), quote=F)
        print(sprintf('Goodman Wilson score            %6.4f to %6.4f    %6.4f', 
            res4$lower[i], res4$upper[i], res4$upper[i]-res4$lower[i]), quote=F)
    }
    print('----------------------------------------------------------', quote=F)

    Goodman_Wald_CIs_for_diffs_1xc(n, alpha, 'Bonferroni')

}