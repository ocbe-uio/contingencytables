source("/Users/ole/Documents/Petter Laake/chap4_translation/Wald_CI_2x2.R")
source("/Users/ole/Documents/Petter Laake/chap4_translation/Wald_CI_CC_2x2.R")
source("/Users/ole/Documents/Petter Laake/chap4_translation/AgrestiCaffo_CI_2x2.R")
source("/Users/ole/Documents/Petter Laake/chap4_translation/Newcombe_hybrid_score_CI_2x2.R")
source("/Users/ole/Documents/Petter Laake/chap4_translation/Mee_asymptotic_score_CI_2x2.R")
source("/Users/ole/Documents/Petter Laake/chap4_translation/MiettinenNurminen_asymptotic_score_CI_difference_2x2.R")

the_2x2_table_CIs_difference = function(n, alpha=0.05) {

    if (missing(n)) {
        # An RCT of high vs standard dose of epinephrine (Perondi et al., 2004)
        n = matrix(c(7, 27, 1, 33), nrow=2, byrow=T) 
        # The association between CHRNA4 genotype and XFS (Ritland et al., 2007) 
        # n = matrix(c(0, 16, 15, 57), nrow=2, byrow=T) 
    }

    pi1hat = n[1,1]/(n[1,1] + n[1,2])
    pi2hat = n[2,1]/(n[2,1] + n[2,2])
    deltahat = pi1hat - pi2hat

    print(sprintf('Estimate of pi_1: %i/%i = %5.3f', n[1,1], n[1,1] + n[1,2], pi1hat), quote=F)
    print(sprintf('Estimate of pi_2: %i/%i = %5.3f', n[2,1], n[2,1] + n[2,2], pi2hat), quote=F)
    print(sprintf('Estimate of delta = pi_1 - pi_2: %5.3f', deltahat), quote=F)

    print(sprintf('Interval method                           %i%% CI         width', 100*(1-alpha)), quote=F)
    print('--------------------------------------------------------------', quote=F)

    # [L, U, ~] = Wald_CI_2x2(n, alpha, 0)
    res = Wald_CI_2x2(n, alpha, printresults=F)
    print(sprintf('Wald                                %7.4f to %7.4f %7.3f', res$lower, res$upper, res$upper - res$lower), quote=F)

    # [L, U, ~] = Wald_CI_CC_2x2(n, alpha, printresults=F)
    res = Wald_CI_CC_2x2(n, alpha, printresults=F)
    print(sprintf('Wald with continuity correction     %7.4f to %7.4f %7.3f', res$lower, res$upper, res$upper - res$lower), quote=F)

    # [L, U, ~] = AgrestiCaffo_CI_2x2(n, alpha, printresults=F)
    res = AgrestiCaffo_CI_2x2(n, alpha, printresults=F)
    print(sprintf('Agresti-Caffo                       %7.4f to %7.4f %7.3f', res$lower, res$upper, res$upper - res$lower), quote=F)

    # [L, U, ~] = Newcombe_hybrid_score_CI_2x2(n, alpha, printresults=F)
    res = Newcombe_hybrid_score_CI_2x2(n, alpha, printresults=F)
    print(sprintf('Newcombe hybrid score               %7.4f to %7.4f %7.3f', res$lower, res$upper, res$upper - res$lower), quote=F)

    # [L, U, ~] = Mee_asymptotic_score_CI_2x2(n, alpha, printresults=F)
    res = Mee_asymptotic_score_CI_2x2(n, alpha, printresults=F)
    print(sprintf('Mee asymptotic score                %7.4f to %7.4f %7.3f', res$lower, res$upper, res$upper - res$lower), quote=F)

    # [L, U, ~] = MiettinenNurminen_asymptotic_score_CI_difference_2x2(n, alpha, printresults=F)
    res = MiettinenNurminen_asymptotic_score_CI_difference_2x2(n, alpha, printresults=F)
    print(sprintf('Miettinen-Nurminen asymptotic score %7.4f to %7.4f %7.3f', res$lower, res$upper, res$upper - res$lower), quote=F)

    print('--------------------------------------------------------------', quote=F)

}
