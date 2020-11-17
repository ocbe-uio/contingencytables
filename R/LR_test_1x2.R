LR_test_1x2 = function(X, n, pi0, printresults=T) {

    # The likelihood ratio test for the binomial probability (pi)
    # H_0: pi = pi0  vs  H_A: pi ~= pi0 (two-sided)
    # Described in Chapter 2 "The 1x2 Table and the Binomial Distribution"
    #
    # Input arguments
    # ---------------
    # X: the number of successes
    # n: the total number of observations
    # pi0: a given probability
    # printresults: display results (0 = no, 1 = yes)

    if(missing(pi0)) {
        X = 250; n = 533; pi0 = 0.513; # Example: The number of 1st order male births (Singh et al. 2010)
     #  X = 204; n = 412; pi0 = 0.513; # Example: The number of 2nd order male births (Singh et al. 2010)
     #  X = 103; n = 167; pi0 = 0.513; # Example: The number of 3rd order male births (Singh et al. 2010)
     #  X = 33; n = 45; pi0 = 0.513;   # Example: The number of 4th order male births (Singh et al. 2010)
     #  X = 13; n = 16; pi0 = 0.5;     # Example: Ligarden et al. (2010)
    }

    # Estimate of the binomial probability (pihat)
    estimate = X/n

    # The likelihood ratio test statistic
    T0 = 2*(X*log(estimate/pi0) + (n - X)*log((1 - estimate)/(1 - pi0)))

    # When the test statistic is uncomputable, set T0 = 0
    if (X == 0 || X == n) {
        T0 = 0
    }

    # The two-sided P-value (reference distribution: chi-squared with 1 degree
    # of freedom)
    df = 1
    # P = 1 - chi2cdf(T0, df);
    P = 1 - pchisq(T0, df)

    if (printresults) {
        print(sprintf('The likelihood ratio test: P = %7.5f, T = %5.3f (df = %i)', P, T0, df))
    }
    
    res = c(P, T0, df)
    names(res) = c("p.value", "statistic", "df")
    invisible(res)

}