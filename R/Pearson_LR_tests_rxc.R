# function results = Pearson_LR_tests_rxc(n, printresults)

Pearson_LR_tests_rxc = function(n, printresults=T) {

    # The Pearson chi-squared and likelihood ratio tests for association in rxc tables
    # Described in Chapter 7 "The rxc Table"
    #
    # Input arguments
    # ---------------
    # n: the observed counts (an rxc matrix)
    # printresults: display results 

    if (missing(n)) {
        # Examples from Chapter 5 (ordered rx2 tables)
        # Alcohol consumption and malformations (Mills and Graubard, 1987):
        # n = matrix(c(48, 17066, 38, 14464, 5, 788, 1, 126, 1, 37), byrow=T, ncol=2) 
        # Elevated troponin T levels in stroke patients (Indredavik et al., 2008):
        # n = matrix(c(8, 53, 10, 48, 11, 100, 22, 102, 6, 129), byrow=T, ncol=2) 

        # Examples from Chapter 6 (ordered 2xc tables)
        # The Adolescent Placement Study (Fontanella et al., 2008):
        # n = matrix(c(8, 28, 72, 126, 46, 73, 69, 86), byrow=T, ncol=4) 
        # Postoperative nausea (Lydersen et al., 2012a):
        # n = matrix(c(14, 10, 3, 2, 11, 7, 8, 4), byrow=T, ncol=4) 

        # Examples from Chapter 7 (unordered rxc tables)
        # Treatment for ear infection (van Balen et al., 2003):
        n = matrix(c(40, 25, 54, 7, 63, 10), byrow=T, ncol=2)
        # Psychiatric diagnoses vs PA (Mangerud et al., 2004):
        # n = matrix(c(62, 21, 97, 48, 10, 12, 30, 7, 132, 78, 34, 17), byrow=T, ncol=2) 
        # Psychiatric diag. vs BMI (Mangerud et al., 2004):
        # n = matrix(c(3, 55, 23, 8, 102, 36, 6, 14, 1, 5, 21, 12, 19, 130, 64, 7, 26, 18), byrow=T, ncol=3) 
    }

    r = nrow(n)
    c0 = ncol(n)
    nip = apply(n, 1, sum)
    npj = apply(n, 2, sum)
    N = sum(n)

    # The Pearson chi-squared and likelihood ratio statistics
    m = matrix(0, r, c0)
    T_Pearson = 0
    T_LR = 0
    for (i in 1:r) {
        for (j in 1:c0) {
            m[i,j] = nip[i]*npj[j]/N
            T_Pearson = T_Pearson + ((n[i,j] - m[i,j])^2)/m[i,j]
            if (n[i,j] > 0) {
                T_LR = T_LR + n[i,j]*log(n[i,j]/m[i,j])
            }
        }
    }
    T_LR = 2*T_LR

    # The two-sided P-values (reference distribution: chi-squared with (r-1)(c-1) degrees of freedom)
    df = (r-1)*(c0-1)
    P_Pearson = 1 - pchisq(T_Pearson, df)
    P_LR = 1 - pchisq(T_LR, df)

    # Output arguments (observed statistics, degrees of freedom, and P-values)
    results = list()
    results$T_Pearson = T_Pearson
    results$df_Pearson = df
    results$P_Pearson = P_Pearson
    results$T_LR = T_LR
    results$df_LR = df
    results$P_LR = P_LR

    if (printresults) {
        print(sprintf('Pearson chi-squared test: T = %6.3f, df = %g, P = %7.5f', T_Pearson, df, P_Pearson))
        print(sprintf('Likelihood ratio test:    T = %6.3f, df = %g, P = %7.5f', T_LR, df, P_LR))
    }
    
    invisible(results)
}

