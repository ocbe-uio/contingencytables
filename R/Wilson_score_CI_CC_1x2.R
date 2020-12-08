Wilson_score_CI_CC_1x2 = function(X, n, alpha=0.05, printresults=T) {

    # The Wilson score confidence interval with continuity correction for the binomial probability
    # Described in Chapter 2 "The 1x2 Table and the Binomial Distribution"
    # 
    # Reference: Wilson EB (1927) Probable inference, the law of succession, and
    # statistical inference. Journal of the American Statistical Association;
    # 22:209-212
    # 
    # Input arguments
    # ---------------
    # X: the number of successes
    # n: the total number of observations
    # alpha: the nominal level, e.g. 0.05 for 95% CIs 
    # printresults: display results (0 = no, 1 = yes)

    if (missing(n)) {
        X = 250; n = 533; # Example: The number of 1st order male births (Singh et al. 2010)
      #  X = 204; n = 412; # Example: The number of 2nd order male births (Singh et al. 2010)
      #  X = 103; n = 167; # Example: The number of 3rd order male births (Singh et al. 2010)
      #  X = 33; n = 45;   # Example: The number of 4th order male births (Singh et al. 2010)
      #  X = 13; n = 16;   # Example: Ligarden et al. (2010)
    }

    # Estimate of the binomial probability (pihat)
    estimate = X/n

    # The upper alpha/2 percentile of the standard normal distribution
    z = qnorm(1 - alpha/2, 0, 1)

    # Calculate the confidence limits
    L = (2*n*estimate + z^2 - 1 - z*sqrt(z^2 - 2 - 1/n + 
            4*estimate*n*(1-estimate) + 4*estimate))/(2*(n + z^2))
    U = (2*n*estimate + z^2 + 1 + z*sqrt(z^2 + 2 - 1/n + 
            4*estimate*n*(1-estimate) - 4*estimate))/(2*(n + z^2))

    if (printresults) {
        print(sprintf('The Wilson score CI: estimate = %6.4f (%g%% CI %6.4f to %6.4f)',
            estimate, 100*(1 - alpha), L, U))
    }

    res = c(L, U, estimate)
    names(res) = c("lower", "upper", "estimate")
    invisible(res)

}
