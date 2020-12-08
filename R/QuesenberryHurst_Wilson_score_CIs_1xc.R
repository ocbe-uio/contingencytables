QuesenberryHurst_Wilson_score_CIs_1xc = function(n, alpha=0.05, printresults=T) {

    # function [L, U, pihat] = QuesenberryHurst_Wilson_score_CIs_1xc(n, alpha, printresults)
    # The Quesenberry-Hurst Wilson score simultaneous intervals for the multinomial probabilities
    # (with Scheffé adjustment)
    # Described in Chapter 3 "The 1xc Table and the Multinomial Distribution"
    # 
    # Input arguments
    # ---------------
    # n: the observed counts (a 1xc vector, where c is the number of categories)
    # alpha: the nominal level, e.g. 0.05 for 95% CIs 
    # printresults: display results (F = no, T = yes)

    if (missing(n)) {
        n = c(276, 380, 118) # Example: Genotype counts for SNP rs 6498169 in RA patients
    }

    c0 = length(n)
    N = sum(n)

    # Estimates of the multinomial probabilities
    pihat = n/N

    # Simultaneous confidence intervals with ScheffŽ adjustment
    L = rep(0, c0)
    U = rep(0, c0)
    Scheffe = qchisq(1-alpha, c0-1)
    for (i in 1:c0) {
        L[i] = (Scheffe + 2*N*pihat[i] - sqrt(Scheffe^2 + 4*N*Scheffe*pihat[i]*(1 - pihat[i])))/(2*Scheffe + 2*N)
        U[i] = (Scheffe + 2*N*pihat[i] + sqrt(Scheffe^2 + 4*N*Scheffe*pihat[i]*(1 - pihat[i])))/(2*Scheffe + 2*N)
    }

    if (printresults) {
        print(sprintf('The Quesenberry-Hurst Wilson score simultaneous intervals'), quote=F)
        for (i in 1:c0) {
            print(sprintf('  pi_%i: estimate = %6.4f (%6.4f to %6.4f)',
                i, pihat[i], L[i], U[i]), quote=F)
        }
    }
    
    res = data.frame(lower=L, upper=U, estimate=pihat)
    invisible(res)        

}
