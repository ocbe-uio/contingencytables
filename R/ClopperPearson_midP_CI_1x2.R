#' @title The Clopper-Pearson mid-P confidence interval
#' @description The Clopper-Pearson mid-P confidence interval for the binomial probability
#' Described in Chapter 2 "The 1x2 Table and the Binomial Distribution"
#'
#' @param X the number of successes
#' @param n the total number of observations
#' @param alpha the nominal level, e.g. 0.05 for 95% CIs
#' @param printresults display results (F = no, T = yes)
#' @return A vector containing lower, upper and point estimates of the statistic
#' @examples
#'
#' # The number of 1st order male births (Singh et al. 2010)
#' ClopperPearson_midP_CI_1x2(X=250, n=533)
#' # The number of 2nd order male births (Singh et al. 2010)
#' ClopperPearson_midP_CI_1x2(X=204, n=412)
#' # The number of 3rd order male births (Singh et al. 2010)
#' ClopperPearson_midP_CI_1x2(X=103, n=167)
#' # The number of 4th order male births (Singh et al. 2010)
#' ClopperPearson_midP_CI_1x2(X=33, n=45)
#' # Ligarden et al. (2010)
#' ClopperPearson_midP_CI_1x2(X=13, n=16)
#' @export
ClopperPearson_midP_CI_1x2 = function(X, n, alpha=0.05, printresults=TRUE) {
    # Define global variables that are needed in the functions below
    # global Xglobal nglobal alphaglobal
    # Xglobal = X;
    # nglobal = n;
    # alphaglobal = alpha;

    # Estimate of the binomial probability (pihat)
    estimate = X/n

    # Use Matlabs fzero function to solve the equations for the confidence limits
    tol = 0.00000001
    # options = optimset('Display', 'off', 'TolX', tol);

    # Find the lower CI limit
    if (estimate == 0) {
        L = 0
    } else {
        L = uniroot(calculate_L_CP2, interval=c(0,1), X=X, n=n, alpha=alpha, tol=tol)$root
    }

    # Find the upper CI limit
    if (estimate == 1) {
        U = 1
    } else {
        U = uniroot(calculate_U_CP2, interval=c(0,1), X=X, n=n, alpha=alpha, tol=tol)$root
    }

    if (printresults) {
        print(sprintf('The Clopper Pearson mid-P CI: estimate = %6.4f (%g%% CI %6.4f to %6.4f)',
            estimate, 100*(1 - alpha), L, U), quote=F)
    }

    res = c(L, U, estimate)
    names(res) = c("lower", "upper", "estimate")
    invisible(res)

}

calculate_L_CP2 = function(L0, X, n, alpha) {
    # global Xglobal nglobal alphaglobal
    T0 = sum(dbinom(X:n, n, L0))
    Tobs = dbinom(X, n, L0)
    L = T0 - Tobs/2 - alpha/2
    return(L)
}

calculate_U_CP2 = function(U0, X, n, alpha) {
    # global Xglobal nglobal alphaglobal
    T0 = sum(dbinom(0:X, n, U0))
    Tobs = dbinom(X, n, U0)
    U = T0 - Tobs/2 - alpha/2
    return(U)
}
