Mee_asymptotic_score_CI_2x2 = function(n, alpha=0.05, printresults=T) {

    # function [L, U, estimate] = Mee_asymptotic_score_CI_2x2(n, alpha, printresults)
    # The Mee asymptotic score confidence interval for the difference between probabilities
    # Described in Chapter 4 "The 2x2 Table"
    # 
    # Input arguments
    # ---------------
    # n: the observed counts (a 2x2 matrix)
    # alpha: the nominal level, e.g. 0.05 for 95% CIs 
    # printresults: display results (F = no, T = yes)

    if (missing(n)) {
      # An RCT of high vs standard dose of epinephrine (Perondi et al., 2004):
        n = matrix(c(7,27,1,33), nrow=2, byrow=T) 
      # The association between CHRNA4 genotype and XFS (Ritland et al., 2007): 
      # n = matrix(c(0,16,15,57), nrow=2, byrow=T) 
    }
    
    # global n11 n21 n1p n2p alphaglobal pi1hat pi2hat limit

    n11 = n[1,1]
    n21 = n[2,1]
    n1p = n[1,1] + n[1,2]
    n2p = n[2,1] + n[2,2]
    # alphaglobal = alpha

    # Estimates of the two probabilities of success
    pi1hat = n[1,1]/n1p
    pi2hat = n[2,1]/n2p

    # Estimate of the difference between probabilities (deltahat)
    estimate = pi1hat - pi2hat

    # Options for Matlab's fzero command
    # options = optimset('Display', 'off', 'TolX', 0.0000001)
    tol = 0.0000001
    delta0 = -0.99999 
    delta1 = 0.99999

    # Lower CI limit
    # limit = 'lower'
    if (estimate == -1) {
        L = -1
    } else {
        L = uniroot(calculate_limit_lower, c(delta0, estimate), n11=n11, n21=n21, 
            n1p=n1p, n2p=n2p, pi1hat=pi1hat, pi2hat=pi2hat, alpha=alpha, tol=tol)$root
    }

    # Upper CI limit
    # limit = 'upper'
    if (estimate == 1) {
        U = 1
    } else {
        U = uniroot(calculate_limit_upper, c(estimate, delta1), n11=n11, n21=n21, 
            n1p=n1p, n2p=n2p, pi1hat=pi1hat, pi2hat=pi2hat, alpha=alpha, tol=tol)$root
    }

    if (printresults) {
        print(sprintf('Mee asymptotic score CI: estimate = %6.4f (%g%% CI %6.4f to %6.4f)',
            estimate, 100*(1 - alpha), L, U), quote=F)
    }
    
    res = data.frame(lower=L, upper=U, estimate=estimate)
    invisible(res)        

}


# ==================================
calculate_limit_lower = function(delta0, n11, n21, n1p, n2p, pi1hat, pi2hat, alpha) {
    # global n11 n21 n1p n2p alphaglobal pi1hat pi2hat limit
    ml.res = ML_estimates(n11, n21, n1p, n2p, delta0)
    T0 = score_test_statistic(pi1hat, pi2hat, delta0, ml.res$p1hat, ml.res$p2hat, n1p, n2p)
    if (is.na(T0)) {
        T0 = 0
    }
    z = qnorm(1-alpha/2, 0, 1)
    f = T0 - z
    return(f)
}

# ==================================
calculate_limit_upper = function(delta0, n11, n21, n1p, n2p, pi1hat, pi2hat, alpha) {
    # global n11 n21 n1p n2p alphaglobal pi1hat pi2hat limit
    ml.res = ML_estimates(n11, n21, n1p, n2p, delta0)
    T0 = score_test_statistic(pi1hat, pi2hat, delta0, ml.res$p1hat, ml.res$p2hat, n1p, n2p)
    if (is.na(T0)) {
        T0 = 0
    }
    z = qnorm(1-alpha/2, 0, 1)
    f = T0 + z
    return(f)
}

# ================================================================
# function [p1hat, p2hat] = ML_estimates(n11, n21, n1p, n2p, delta0)
ML_estimates = function(n11, n21, n1p, n2p, delta0) {
    L3 = n1p + n2p
    L2 = (n1p + 2*n2p)*delta0 - (n1p + n2p) - (n11 + n21)
    L1 = (n2p*delta0 - (n1p + n2p) - 2*n21)*delta0 + (n11 + n21)
    L0 = n21*delta0*(1 - delta0)
    q = L2^3/((3*L3)^3) - L1*L2/(6*L3^2) + L0/(2*L3)
    p = sign(q)*sqrt(L2^2/(3*L3)^2 - L1/(3*L3))
    a = (1/3)*(pi + acos(q/p^3))
    p2hat = 2*p*cos(a) - L2/(3*L3)
    p1hat = p2hat + delta0
    res = data.frame(p1hat=p1hat, p2hat=p2hat)
    return(res)
}

# ===============================================================================
score_test_statistic = function(pi1hat, pi2hat, delta0, p1hat, p2hat, n1p, n2p) {
    T0 = (pi1hat - pi2hat - delta0)/sqrt(p1hat*(1 - p1hat)/n1p + p2hat*(1 - p2hat)/n2p)
    return(T0)
}
