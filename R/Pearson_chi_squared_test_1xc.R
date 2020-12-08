Pearson_chi_squared_test_1xc = function(n, pi0, printresults=T) {

    # function [P, T, df] = Pearson_chi_squared_test_1xc(n, pi0, printresults)
    # The Pearson chi-squared test for multinomial probabilities
    # Described in Chapter 3 "The 1xc Table and the Multinomial Distribution"
    #
    # Input arguments
    # ---------------
    # n: the observed counts (a 1xc vector, where c is the number of categories)
    # pi0: given probabilities (a 1xc vector)
    # printresults: display results (F = no, T = yes)

    if (missing(n)) {
        n = c(276, 380, 118)  # Example: Genotype counts for SNP rs 6498169 in RA patients
        pi0 = c(0.402, 0.479, 0.119)
        # n = c(6, 1, 3) # Example: subset of 10 patients
        # pi0 = c(0.402, 0.479, 0.119)
    }

    c0 = length(n)
    N = sum(n)

    # The Pearson chi-squared test statistic
    T0 = sum(((n - N*pi0)^2)/(N*pi0))

    # The two-sided P-value (reference distribution: chi-squared with c-1 degrees of freedom)
    df = c0-1
    P = 1 - pchisq(T0, df)

    if (printresults) {
        print(sprintf('The Pearson chi-squared test: P = %7.5f, T = %5.3f (df = %i)', P, T0, df), quote=F)
    }

    res = data.frame(P=P, T=T0, df=df) 
    invisible(res)

}

