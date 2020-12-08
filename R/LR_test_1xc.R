LR_test_1xc = function(n, pi0, printresults=T) {

    # function [P, T, df] =  LR_test_1xc(n, pi0, printresults)
    # The likelihood ratio test for multinomial probabilities
    # Described in Chapter 3 "The 1xc Table and the Multinomial Distribution"
    #
    # Input arguments
    # ---------------
    # n: the observed counts (a 1xc vector, where c is the number of categories)
    # pi0: given probabilities (a 1xc vector)
    # printresults: display results (F = no, T = yes)

    if (missing(pi0)) {
        n = c(276, 380, 118) # Example: Genotype counts for SNP rs 6498169 in RA patients
        pi0 = c(0.402, 0.479, 0.119)
        # n = c(6, 1, 3) # Example: subset of 10 patients
        # pi0 = c(0.402, 0.479, 0.119)
    }

    c0 = length(n)
    N = sum(n)

    # The likelihood ratio test statistic
    T0 = 0
    for (i in 1:c0) {
        if (n[i] > 0) {
            T0 = T0 + n[i]*log(n[i]/(N*pi0[i]))
        }
    }
    T0 = 2*T0

    # The two-sided P-value (reference distribution: chi-squared with c-1 degrees of freedom)
    df = c0-1
    P = 1 - pchisq(T0, df)

    if (printresults) {
        print(sprintf('The likelihood ratio test: P = %7.5f, T = %5.3f (df = %i)', P, T0, df), quote=F)
    }
    
    res = data.frame(P=P, T=T0, df=df)
    invisible(res)

}