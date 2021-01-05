Adjusted_inv_sinh_CI_OR_2x2 = function(n, psi1=0.45, psi2=0.25, alpha=0.05, printresults=T) {

    # function c(L, U, estimate] = Adjusted_inv_sinh_CI_OR_2x2(n, psi1, psi2, alpha, printresults)
    # The adjusted inverse hyperbolic sine confidence interval for the odds ratio
    # Described in Chapter 4 "The 2x2 Table"
    #
    # Input arguments
    # ---------------
    # n: the observed counts (a 2x2 matrix)
    # psi1, psi2: pseudo-frequencies (both should be > 0)
    # alpha: the nominal level, e.g. 0.05 for 95% CIs
    # printresults: display results (F = no, T = yes)

    if (missing(n)) {
        # A case-control study of GADA exposure on IPEX syndrome (Lampasona et al., 2013):
        n = matrix(c(9,4,4,10), nrow=2, byrow=T)
        # The association between CHRNA4 genotype and XFS (Ritland et al., 2007):
        # n = matrix(c(0,16,15,57), nrow=2, byrow=T)
    }

    # Estimate of the odds ratio (thetahat)
    estimate = n[1,1]*n[2,2]/(n[1,2]*n[2,1])

    # Adjusted estimate
    thetatilde = (n[1,1] + psi1)*(n[2,2] + psi1)/((n[1,2] + psi1)*(n[2,1] + psi1))

    # The upper alpha/2 percentile of the standard normal distribution
    z = qnorm(1-alpha/2, 0, 1)

    # Calculate the confidence limits
    tmp = asinh(0.5*z*sqrt(1/(n[1,1] + psi2) + 1/(n[1,2] + psi2) + 1/(n[2,1] + psi2) + 1/(n[2,2] + psi2)))
    L = exp(log(thetatilde) - 2*tmp)
    U = exp(log(thetatilde) + 2*tmp)

    if (printresults) {
        print(sprintf('The adjusted inverse sinh CI: estimate = %6.4f (%g%% CI %6.4f to %6.4f)',
            estimate, 100*(1 - alpha), L, U), quote=F)
    }

    res = data.frame(lower=L, upper=U, estimate=estimate)
    invisible(res)
}

