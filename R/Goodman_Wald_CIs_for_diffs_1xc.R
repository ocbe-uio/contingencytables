Goodman_Wald_CIs_for_diffs_1xc = function(n, alpha=0.05, adjustment="Bonferroni", printresults=T) {

    # function [L, U, diffs] = Goodman_Wald_CIs_for_diffs_1xc(n, alpha, adjustment, printresults)
    # The Goodman Wald simultaneous intervals for the differences between the
    # multinomial probabilities (with Scheffé or Bonferroni adjustment)
    # Described in Chapter 3 "The 1xc Table and the Multinomial Distribution"
    #
    # Input arguments
    # ---------------
    # n: the observed counts (a 1xc vector, where c is the number of categories)
    # alpha: the nominal level, e.g. 0.05 for 95# CIs
    # adjustment: Scheffé or Bonferroni adjustment ("Scheffe" or "Bonferroni")
    # printresults: display results (F = no, T = yes)

    if (missing(n)) {
        n = c(276, 380, 118) # Example: Genotype counts for SNP rs 6498169 in RA patients
    }

    c0 = length(n)
    N = sum(n)

    # Estimates of the multinomial probabilities
    pihat = n/N

    # Simultaneous confidence intervals with ScheffŽ or Bonferroni adjustment
    L = rep(0, c0*(c0-1)/2)
    U = rep(0, c0*(c0-1)/2)
    diffs = rep(0, c0*(c0-1)/2)
    Scheffe = qchisq(1-alpha, c0-1)
    Bonferroni = qchisq(1-alpha/c0, 1)

    if (printresults) {
        print(sprintf('The Goodman Wald simultaneous intervals for differences (%s)', adjustment), quote=F)
    }
    k = 0
    for (i in 1:(c0-1)) {
        for (j in (i+1):c0) {
            k = k + 1
            diffs[k] = pihat[i] - pihat[j]
            if (adjustment == 'Scheffe') {
                L[k] = diffs[k] - sqrt(Scheffe*(pihat[i] + pihat[j] - (diffs[k])^2)/N)
                U[k] = diffs[k] + sqrt(Scheffe*(pihat[i] + pihat[j] - (diffs[k])^2)/N)
            } else if (adjustment == 'Bonferroni') {
                L[k] = diffs[k] - sqrt(Bonferroni*(pihat[i] + pihat[j] - (diffs[k])^2)/N)
                U[k] = diffs[k] + sqrt(Bonferroni*(pihat[i] + pihat[j] - (diffs[k])^2)/N)
            }
            if (printresults) {
                print(sprintf('  pi_%i - pi_%i: estimate = %6.4f (%6.4f to %6.4f)', 
                    i, j, diffs[k], L[k], U[k]), quote=F)
            }
        }
    }

    res = data.frame(lower=L, upper=U, estimate=diffs)
    invisible(res)
}


