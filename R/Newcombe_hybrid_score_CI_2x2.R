Newcombe_hybrid_score_CI_2x2 = function(n, alpha=0.05, printresults=T) {

    # function [L, U, estimate] = Newcombe_hybrid_score_CI_2x2(n, alpha, printresults)
    # The Newcombe hybrid score confidence interval for the difference between probabilities
    # Described in Chapter 4 "The 2x2 Table"
    # 
    # Dependencies: "Wilson_score_CI_1x2.m"
    #
    # Input arguments
    # ---------------
    # n: the observed counts (a 2x2 matrix)
    # alpha: the nominal level, e.g. 0.05 for 95% CIs 
    # printresults: display results (F = no, T = yes)

    if (missing(n)) {
        n = matrix(c(7, 27, 1, 33), nrow=2, byrow=T)  # An RCT of high vs standard dose of epinephrine (Perondi et al., 2004)
        # n = matrix(c(0, 16, 15, 57), nrow=2, byrow=T) # The association between CHRNA4 genotype and XFS (Ritland et al., 2007)
    }

    n1p = n[1,1] + n[1,2]
    n2p = n[2,1] + n[2,2]

    # Estimates of the two probabilities of success
    pi1hat = n[1,1]/n1p
    pi2hat = n[2,1]/n2p

    # Estimate of the difference between probabilities (deltahat)
    estimate = pi1hat - pi2hat

    # Use Wilson score CIs for the two probabilities of success
    #[l1, u1, ~] = Wilson_score_CI_1x2(n[1,1], n1p, alpha, 0)
    #[l2, u2, ~] = Wilson_score_CI_1x2(n[2,1], n2p, alpha, 0)
    res1 = Wilson_score_CI_1x2(n[1,1], n1p, alpha, printresults=F)
    res2 = Wilson_score_CI_1x2(n[2,1], n2p, alpha, printresults=F)
    L = estimate - sqrt((pi1hat-res1$lower)^2 + (res2$upper-pi2hat)^2)
    U = estimate + sqrt((pi2hat-res2$lower)^2 + (res1$upper-pi1hat)^2)

    # Fix overshoot by truncation
    L = max(-1, L)
    U = min(U, 1)

    if (printresults) {
        print(sprintf('The Newcombe hybrid score CI: estimate = %6.4f (%g%% CI %6.4f to %6.4f)', 
            estimate, 100*(1 - alpha), L, U), quote=F)
    }
    
    res = data.frame(lower=L, upper=U, estimate=estimate)
    invisible(res)        
    
}
