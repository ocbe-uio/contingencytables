MiettinenNurminen_asymptotic_score_CI_OR_2x2 = function(n, alpha=0.05, printresults=T) {

    # function [L, U, estimate] = MiettinenNurminen_asymptotic_score_CI_OR_2x2(n, alpha, printresults)
    # The Miettinen-Nurminen asymptotic score confidence interval for the odds ratio
    # Described in Chapter 4 "The 2x2 Table"
    # 
    # Input arguments
    # ---------------
    # n: the observed counts (a 2x2 matrix)
    # alpha: the nominal level, e.g. 0.05 for 95% CIs 
    # printresults: display results (F = no, T = yes)

    if (missing(n)) {
        # A case-control study of GADA exposure on IPEX syndrome (Lampasona et al., 2013):
        n = matrix(c(9,4,4,10), nrow=2, byrow=T) 
        # The association between CHRNA4 genotype and XFS (Ritland et al., 2007):
        # n = matrix(c(0,16,15,57), nrow=2, byrow=T) 
    }

    # global n11 n21 n1p n2p alphaglobal limit

    n11 = n[1,1]
    n21 = n[2,1]
    n1p = n[1,1] + n[1,2]
    n2p = n[2,1] + n[2,2]
    # alphaglobal = alpha

    # Estimate of the odds ratio (thetahat)
    estimate = n[1,1]*n[2,2]/(n[1,2]*n[2,1])

    # Options for Matlab's fzero command
    # options = optimset('Display', 'off', 'TolX', 0.0000001)
    tol = 0.0000001
    theta0 = 0.00001 
    theta1 = 100000

    # Lower CI limit
    # limit = 'lower' 
    if (is.na(estimate) || estimate==Inf) {
        # [L, ~, exitflag] = fzero(@calculate_limit, [theta0, theta1], options)
        L = uniroot(calculate_limit_lower, c(theta0, theta1), n11=n11, n21=n21, n1p=n1p,
                n2p=n2p, alpha=alpha, tol=tol)$root
    } else if (estimate == 0) {
        L = 0 
        # exitflag = 1
    } else {
        # [L, ~, exitflag] = fzero(@calculate_limit, [theta0, estimate], options)
        L = uniroot(calculate_limit_lower, c(theta0, estimate), n11=n11, n21=n21, n1p=n1p,
                n2p=n2p, alpha=alpha, tol=tol)$root
    }
    # if exitflag ~= 1, display_warning(exitflag), }

    # Upper CI limit
    # limit = 'upper'
    if (n[2,1] == 0 || n[1,2] == 0) {
        U = Inf 
        # exitflag = 1
    } else if (estimate == 0) {
        # [U, ~, exitflag] = fzero(@calculate_limit, [theta0, theta1], options) 
        U = uniroot(calculate_limit_upper, c(theta0, theta1), n11=n11, n21=n21, n1p=n1p,
                n2p=n2p, alpha=alpha, tol=tol)$root
    } else {
        # [U, ~, exitflag] = fzero(@calculate_limit, [estimate, theta1], options)
        U = uniroot(calculate_limit_upper, c(estimate, theta1), n11=n11, n21=n21, n1p=n1p,
                n2p=n2p, alpha=alpha, tol=tol)$root
    }
    # if exitflag ~= 1, display_warning(exitflag), }

    if (printresults) {
        print(sprintf('Mietinen-Nurminen asymptotic score CI: estimate = %6.4f (%g%% CI %6.4f to %6.4f)',
            estimate, 100*(1 - alpha), L, U), quote=F)
    }
    
    res = data.frame(lower=L, upper=U, estimate=estimate)
    invisible(res)        
}


# ==================================
calculate_limit_lower = function(theta0, n11, n21, n1p, n2p, alpha) {
    # global n11 n21 n1p n2p alphaglobal limit
    T0 = score_test_statistic(theta0, n11, n21, n1p, n2p)
    if (is.na(T0)) {
        T0 = 0
    }
    f = T0 - qnorm(1-alpha/2, 0, 1)
    return(f)
}

# ==================================
calculate_limit_upper = function(theta0, n11, n21, n1p, n2p, alpha) {
    # global n11 n21 n1p n2p alphaglobal limit
    T0 = score_test_statistic(theta0, n11, n21, n1p, n2p)
    if (is.na(T0)) {
        T0 = 0
    }
    f = T0 + qnorm(1-alpha/2, 0, 1)
    return(f)
}

# ===========================================================
score_test_statistic = function(theta0, n11, n21, n1p, n2p) {
    res = ML_estimates(theta0, n11, n21, n1p, n2p)
    T0 = (n1p*(n11/n1p - res$p1hat))*sqrt(1/(n1p*res$p1hat*(1 - res$p1hat)) + 1/(n2p*res$p2hat*(1 - res$p2hat)))
    T0 = T0*sqrt(1 - 1/(n1p + n2p))
    return(T0)
}

# ================================================================
ML_estimates = function(theta0, n11, n21, n1p, n2p) {
    A = n2p*(theta0 - 1)
    B = n1p*theta0 + n2p - (n11 + n21)*(theta0 - 1)
    C = -(n11 + n21)
    p2hat = (-B + sqrt(B^2 - 4*A*C))/(2*A)
    p1hat = p2hat*theta0/(1 + p2hat*(theta0 - 1))
    res = data.frame(p1hat=p1hat, p2hat=p2hat)
    return(res)
}
