LR_test_2x2 = function(n, printresults=T) {

    # function [P, T, df] = LR_test_2x2(n, printresults)
    # The likelihood ratio test for association in 2x2 tables
    # Described in Chapter 4 "The 2x2 Table"
    #
    # Input arguments
    # ---------------
    # n: the observed counts (a 2x2 matrix)
    # printresults: display results (F = no, T = yes)

    if (missing(n)) {
        n = rbind(c(3,1), c(1,3))       # Example: A lady tasting a cup of tea
        # n = rbind(c(7,27), c(1,33))   # Example: Perondi et al. (2004)
        # n = rbind(c(9,4), c(4,10))    # Example: Lampasona et al. (2013)
        # n = rbind(c(0,16), c(15,57))  # Example: Ritland et al. (2007)
    }

    # The estimated expected counts
    N = sum(n)
    m = outer(apply(n,1,sum), apply(n,2,sum))/N

    # The likelihood ratio statistic
    T0 = 0
    for (i in 1:2) {
        for (j in 1:2) {
            if (n[i,j] > 0) {
                T0 = T0 + n[i,j]*log(n[i,j]/m[i,j])
            }
        }
    }
    T0 = 2 * T0

    # The two-sided P-value (reference distribution: chi-squared with 1 degree of freedom)
    df = 1
    P = 1 - pchisq(T0, df)

    # Handle cases where the P-value is not computable
    if (is.na(P)) {
        P = 1.0
    }

    if (printresults) {
        print(sprintf('The likelihood ratio test: P = %7.5f, T = %5.3f (df = %i)', P, T0, df), quote=F)
    }
    
    res = data.frame(p.value=P, statistic=T0, df=df)
    invisible(res)
    
}
