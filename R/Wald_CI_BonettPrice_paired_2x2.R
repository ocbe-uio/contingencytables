 Wald_CI_BonettPrice_paired_2x2 = function(n, alpha=0.05, printresults=T) {
 	# The Wald confidence interval for the difference between paired probabilities
 	# with the pseudo-frequency adjustment suggested by Bonett and Price(2012)
 	# Described in Chapter 8 "The Paired 2x2 Table"
 	#
 	# Input arguments
 	# ---------------
 	# n: the observed counts (a 2x2 matrix)
 	# alpha: the nominal level, e.g. 0.05 for 95% CIs
 	# printresults: display results (F = no, T = yes)
 	
 	if (missing(n)) {
 		#    n = [1 1; 7 12]; # Airway hyper-responsiveness before and after stem cell transplantation (Bentur et al., 2009)
 		n = rbind(c(59,6),c(16,80))  # Airway hyper-responsiveness before and after stem cell transplantation (Bentur et al., 2009)
 	}
 	
 	# Estimate of the difference between probabilities (deltahat)
 	N = sum(n)
 	estimate = (n[1,2] - n[2,1])/N
 	
 	# Calculate the Laplace estimates
 	pi12tilde = (n[1,2] + 1)/(N + 2)
 	pi21tilde = (n[2,1] + 1)/(N + 2)
 	
 	# Standard error of the estimate
 	SE = sqrt((pi12tilde + pi21tilde - (pi12tilde - pi21tilde)^2)/(N + 2))
 	
 	# The upper alpha/2 percentile of the standard normal distribution
 	z = qnorm(1 - alpha/2, 0, 1)
 	
 	# Calculate the confidence limits
 	L = pi12tilde - pi21tilde - z*SE
 	U = pi12tilde - pi21tilde + z*SE
 	
 	# Fix overshoot by truncation
 	L = max(-1, L)
 	U = min(U, 1)
 	
 	if (printresults) {
     .print('The Wald CI with Bonett-Price adjustment: estimate = %6.4f (%g%% CI %6.4f to %6.4f)\n', estimate, 100*(1 - alpha), L, U)
 	}
 	
 	invisible(list(L=L,U=U,estimate=estimate))
 }
 	
 .print = function(s, ...) {
	   print(sprintf(gsub('\n','',s), ...), quote=F)
 }
