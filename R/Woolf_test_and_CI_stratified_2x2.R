 Woolf_test_and_CI_stratified_2x2 = function(n, alpha=0.05, printresults=T) {
 	# The Woolf test and CI for a common odds ratio
 	# (A Wald-type test and CI based on the inverse variance estimate)
 	# Described in Chapter 10 "Stratified 2x2 Tables and Meta-Analysis"
 	#
 	# Dependencies
 	# ------------
 	# "InverseVariance_estimate_stratified_2x2.m"
 	#
 	# Input arguments
 	# ---------------
 	# n: the observed table (a 2x2xk matrix, where k is the number of strata)
 	# alpha: the nominal level, e.g. 0.05 for 95% CIs
 	# printresults: display results (F = no, T = yes)
 	
 	if (missing(n)) {
 	    # Smoking and lung cancer (Doll and Hill, 1950)
 		#     n = zeros(2, 2, 2)
 		#     n(:,:,1) = [647 622; 2 27]
 		#     n(:,:,2) = [41 28; 19 32]
 	
 	    # Prophylactice use of Lidocaine in myocardial infarction (Hine et al., 1989)
	    n = array(0, dim=c(2,2,6))
 	    n[,,1] = rbind(c(2,37),c(1,42))
 	    n[,,2] = rbind(c(4,40),c(4,40))
 	    n[,,3] = rbind(c(6,101),c(4,106))
 	    n[,,4] = rbind(c(7,96),c(5,95))
 	    n[,,5] = rbind(c(7,103),c(3,103))
 	    n[,,6] = rbind(c(11,143),c(4,142))
 	}
 	
 	# Get the inverse variance overall estimate and weights
 	tmp  = InverseVariance_estimate_stratified_2x2(n, 'logit', F)
	thetahatIV = tmp[[1]]; v = tmp[[3]]
 	
 	# Estimate the standard error
 	SElog = 1/sqrt(sum(v))
 	
 	# The Wald test statistic
 	Z = log(thetahatIV)/SElog
 	
 	# The two-sided P-value (reference distribution: standard normal)
 	P = 2*(1 - pnorm(abs(Z), 0, 1))
 	
 	# The upper alpha/2 percentile of the standard normal distribution
 	z_alpha = qnorm(1 - alpha/2, 0, 1)
 	
 	# Calculate the confidence limits
 	L = thetahatIV*exp(-z_alpha*SElog)
 	U = thetahatIV*exp(z_alpha*SElog)
 	
 	if (printresults) {
     .print('The Woolf test: P = %7.5f, Z = %6.3f\n', P, Z)
     .print('The Woolf CI: thetahatIV = %6.4f (%g%% CI %6.4f to %6.4f)\n', thetahatIV, 100*(1 - alpha), L, U)
 	}
 	
 	invisible(list(P=P,Z=Z,L=L,U=U,thetahatIV=thetahatIV))
 }

 	
 .print = function(s, ...) {
	   print(sprintf(gsub('\n','',s), ...), quote=F)
 }
