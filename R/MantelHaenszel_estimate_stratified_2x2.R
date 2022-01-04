 MantelHaenszel_estimate_stratified_2x2 = function(n, link="logit", printresults=T) {
 	# The Mantel-Haenszel estimate of the overall effect across strata
 	# Described in Chapter 10 "Stratified 2x2 Tables and Meta-Analysis"
 	#
 	# Input arguments
 	# ---------------
 	# n: the observed table (a 2x2xk matrix, where k is the number of strata)
 	# link: the link function ('linear', 'log', or 'logit')
 	# printresults: display results (F = no, T = yes)

 	if (missing(n)) {
 	    # Smoking and lung cancer (Doll and Hill, 1950)
 		#  n = zeros(2, 2, 2)
 		#  n(:,:,1) = [647 622; 2 27]
 		#  n(:,:,2) = [41 28; 19 32]
 	
 	    # Prophylactice use of Lidocaine in myocardial infarction (Hine et al., 1989)
	    n = array(0, dim=c(2,2,6))
 	    n[,,1] = rbind(c(2,37),c(1,42))
 	    n[,,2] = rbind(c(4,40),c(4,40))
 	    n[,,3] = rbind(c(6,101),c(4,106))
 	    n[,,4] = rbind(c(7,96),c(5,95))
 	    n[,,5] = rbind(c(7,103),c(3,103))
 	    n[,,6] = rbind(c(11,143),c(4,142))
 	}
 	
 	n1pk = apply(n[1,,], 2, sum)
 	n2pk = apply(n[2,,], 2, sum)
 	nppk = apply(n, 3, sum)
 	
 	# Calculate stratum-specific effect estimates
 	if (identical(link, 'linear')) {
 	    psihat = n[1,1,]/n1pk - n[2,1,]/n2pk
 	} else if (identical(link, 'log')) {
 	    psihat = (n[1,1,]/n1pk)/(n[2,1,]/n2pk)
 	} else if (identical(link, 'logit')) {
 	    psihat = (n[1,1,]*n[2,2,])/(n[1,2,]*n[2,1,])
 	}
 	
 	# Calculate weights
 	if (identical(link, 'linear')) {
 	    w = n1pk*n2pk/nppk
 	} else if (identical(link, 'log')) {
 	    w = n1pk*n[2,1,]/nppk
 	} else if (identical(link, 'logit')) {
 	    w = n[1,2,]*n[2,1,]/nppk
 	}
 	
 	# The Mantel-Haenszel estimate of the overall effect
 	estimate = sum(w*psihat)/sum(w)
 	
 	if (printresults) {
    	.print('The Mantel-Haenszel estimate = %7.4f\n', estimate)
 	}
 	
 	invisible(list(estimate=estimate,psihat=psihat,w=w))
}
 	
.print = function(s, ...) {
	print(sprintf(gsub('\n','',s), ...), quote=F)
}
