source("/Users/ole/Documents/Petter Laake/chap4_translation/Woolf_logit_CI_2x2.R")
source("/Users/ole/Documents/Petter Laake/chap4_translation/Gart_adjusted_logit_CI_2x2.R")
source("/Users/ole/Documents/Petter Laake/chap4_translation/Independence_smoothed_logit_CI_2x2.R")
source("/Users/ole/Documents/Petter Laake/chap4_translation/Inv_sinh_CI_OR_2x2.R")
source("/Users/ole/Documents/Petter Laake/chap4_translation/Adjusted_inv_sinh_CI_OR_2x2.R")
source("/Users/ole/Documents/Petter Laake/chap4_translation/MOVER_R_Wilson_CI_OR_2x2.R")
source("/Users/ole/Documents/Petter Laake/chap4_translation/MiettinenNurminen_asymptotic_score_CI_OR_2x2.R")
source("/Users/ole/Documents/Petter Laake/chap4_translation/Uncorrected_asymptotic_score_CI_2x2.R")
source("/Users/ole/Documents/Petter Laake/chap4_translation/Cornfield_exact_conditional_CI_2x2.R")
source("/Users/ole/Documents/Petter Laake/chap4_translation/BaptistaPike_exact_conditional_CI_2x2.R")
source("/Users/ole/Documents/Petter Laake/chap4_translation/Cornfield_midP_CI_2x2.R")
source("/Users/ole/Documents/Petter Laake/chap4_translation/BaptistaPike_midP_CI_2x2.R")

the_2x2_table_CIs_OR = function(n, alpha=0.05) {

    if (missing(n)) {
        n = rbind(c(3,1), c(1,3))       # Example: A lady tasting a cup of tea
        # n = rbind(c(7,27), c(1,33))   # Example: Perondi et al. (2004)
        # n = rbind(c(9,4), c(4,10))    # Example: Lampasona et al. (2013)
        # n = rbind(c(0,16), c(15,57))  # Example: Ritland et al. (2007)
    }

    pi1hat = n[1,1]/(n[1,1] + n[1,2])
    pi2hat = n[2,1]/(n[2,1] + n[2,2])
    thetahat = n[1,1]*n[2,2]/(n[1,2]*n[2,1])

    print(sprintf('Estimate of pi_1: %i/%i = %5.3f', n[1,1], n[1,1] + n[1,2], pi1hat), quote=F)
    print(sprintf('Estimate of pi_2: %i/%i = %5.3f', n[2,1], n[2,1] + n[2,2], pi2hat), quote=F)
    print(sprintf('Estimate of theta = (pi_1/(1-pi_1))/(pi_2/(1-pi_2)): %5.3f', thetahat), quote=F)

    print(sprintf('Interval method                            %i%% CI      Log width', 100*(1-alpha)), quote=F)
    print('----------------------------------------------------------------', quote=F)

    res = Woolf_logit_CI_2x2(n, alpha, printresults=F)
    L = res$lower; U = res$upper
    print(sprintf('Woolf logit                           %6.3f to %6.3f  %7.3f', L, U, log(U) - log(L)), quote=F)

    res = Gart_adjusted_logit_CI_2x2(n, alpha, printresults=F)
    L = res$lower; U = res$upper
    print(sprintf('Gart adjusted logit                   %6.3f to %6.3f  %7.3f', L, U, log(U) - log(L)), quote=F)

    res = Independence_smoothed_logit_CI_2x2(n, alpha, printresults=F)
    L = res$lower; U = res$upper
    print(sprintf('Independence-smoothed logit           %6.3f to %6.3f  %7.3f', L, U, log(U) - log(L)), quote=F)

    res = Inv_sinh_CI_OR_2x2(n, alpha, printresults=F)
    L = res$lower; U = res$upper
    print(sprintf('Inverse sinh                          %6.3f to %6.3f  %7.3f', L, U, log(U) - log(L)), quote=F)

    res = Adjusted_inv_sinh_CI_OR_2x2(n, 0.45, 0.25, alpha, printresults=F)
    L = res$lower; U = res$upper
    print(sprintf('Adjusted inverse sinh (0.45, 0.25)    %6.3f to %6.3f  %7.3f', L, U, log(U) - log(L)), quote=F)

    res = Adjusted_inv_sinh_CI_OR_2x2(n, 0.6, 0.4, alpha, printresults=F)
    L = res$lower; U = res$upper
    print(sprintf('Adjusted inverse sinh (0.6, 0.4)      %6.3f to %6.3f  %7.3f', L, U, log(U) - log(L)), quote=F)

    res = MOVER_R_Wilson_CI_OR_2x2(n, alpha, printresults=F)
    L = res$lower; U = res$upper
    print(sprintf('MOVER-R Wilson                        %6.3f to %6.3f  %7.3f', L, U, log(U) - log(L)), quote=F)

    res = MiettinenNurminen_asymptotic_score_CI_OR_2x2(n, alpha, printresults=F)
    L = res$lower; U = res$upper
    print(sprintf('Miettinen-Nurminen asymptotic score   %6.3f to %6.3f  %7.3f', L, U, log(U) - log(L)), quote=F)

    res = Uncorrected_asymptotic_score_CI_2x2(n, alpha, printresults=F)
    L = res$lower; U = res$upper
    print(sprintf('Uncorrected asymptotic score          %6.3f to %6.3f  %7.3f', L, U, log(U) - log(L)), quote=F)

    res = Cornfield_exact_conditional_CI_2x2(n, alpha, printresults=F)
    L = res$lower; U = res$upper
    print(sprintf('Cornfield exact conditional           %6.3f to %6.3f  %7.3f', L, U, log(U) - log(L)), quote=F)

    res = BaptistaPike_exact_conditional_CI_2x2(n, alpha, printresults=F)
    L = res$lower; U = res$upper
    print(sprintf('Baptista-Pike exact conditional       %6.3f to %6.3f  %7.3f', L, U, log(U) - log(L)), quote=F)

    res = Cornfield_midP_CI_2x2(n, alpha, printresults=F)
    L = res$lower; U = res$upper
    print(sprintf('Cornfield mid-P                       %6.3f to %6.3f  %7.3f', L, U, log(U) - log(L)), quote=F)

    res = BaptistaPike_midP_CI_2x2(n, alpha, printresults=F)
    L = res$lower; U = res$upper
    print(sprintf('Baptista-Pike mid-P                   %6.3f to %6.3f  %7.3f', L, U, log(U) - log(L)), quote=F)

    print('----------------------------------------------------------------', quote=F)

}
