 Blaker_exact_CI_1x2 = function(X, n, alpha=0.05, printresults=T) {
 	# The Blaker exact confidence interval for the binomial probability
 	# Described in Chapter 2 "The 1x2 Table and the Binomial Distribution"
 	#
 	# Reference: Blaker H (2000) Confidence curves and improved exact
 	# confidence intervals for discrete distributions. The Canadian Journal of
 	# Statistics; 28:783-798
 	#
 	# Input arguments
 	# ---------------
 	# X: the number of successes
 	# n: the total number of observations
 	# alpha: the nominal level, e.g. 0.05 for 95% CIs
 	# printresults: display results (F = no, T = yes)
 	
 	if (missing(n)) {
 	#    X = 250; n = 533; # Example: The number of 1st order male births (Singh et al. 2010)
 	#    X = 204; n = 412; # Example: The number of 2nd order male births (Singh et al. 2010)
 	#    X = 103; n = 167; # Example: The number of 3rd order male births (Singh et al. 2010)
 	#    X = 33; n = 45;   # Example: The number of 4th order male births (Singh et al. 2010)
 	     X = 13; n = 16;   # Example: Ligarden et al. (2010)
 	}
 	
 	# Estimate of the binomial probability (pihat)
 	estimate = X/n
 	
 	# Solve the equations for the confidence limits
 	tol = 0.00000001
 	
 	# Find the lower CI limit
 	if (estimate == 0) {
 	    L = 0
 	} else {
 		param = list(X=X, n=n, alpha=alpha)
 		L = uniroot(calculate_limitX, c(0, X/n), .param=param, tol=tol)$root
 	}
 	
 	# Find the upper CI limit
 	if(estimate == 1) {
 	    U = 1
 	} else {
 		param = list(X=X, n=n, alpha=alpha)
 		U = uniroot(calculate_limitX, c(X/n, 1), .param=param, tol=tol)$root
 	}
 	
 	if (printresults) {
     .print('The Blaker exact CI: estimate = %6.4f (%g%% CI %6.4f to %6.4f)\n', estimate, 100*(1 - alpha), L, U)
 	}
 	
 	invisible(list(L=L, U=U, estimate=estimate))
}
 	
 	
 	# ===============================
 calculate_limitX = function(pi0, .param) {
 	Xglobal = .param$X
 	nglobal = .param$n 
 	alphaglobal = .param$alpha
 	Pvalues = dbinom(0:nglobal, nglobal, pi0)
 	gammaobs = min(c(sum(Pvalues[(Xglobal+1):(nglobal+1)]), sum(Pvalues[1:(Xglobal+1)])))
 	T0 = 0
 	for (k in 0:nglobal) {
 	    gammak = min(c(sum(Pvalues[(k+1):(nglobal+1)]), sum(Pvalues[1:(k+1)])))
 	    if(gammak <= gammaobs) {
 	        T0 = T0 + dbinom(k, nglobal, pi0)
 	    }
 	}
 	f = T0 - alphaglobal
 	return(f)
 }
 	
 .print = function(s, ...) {
	   print(sprintf(gsub('\n','',s), ...), quote=F)
 }
