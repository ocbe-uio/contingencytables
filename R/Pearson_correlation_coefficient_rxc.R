 Pearson_correlation_coefficient_rxc = function(n, a, b, alpha=0.05, printresults=T) {
 	# The Pearson correlation coefficient
 	# Described in Chapter 7 "The rxc Table"
 	#
 	# Input arguments
 	# ---------------
 	# n: the observed table (an rxc matrix)
 	# a: scores assigned to the rows
 	# b: scores assigned to the columns
 	# alpha: the nominal significance level, used to compute a 100(1-alpha) confidence interval
 	# printresults: display results (0 = no, 1 = yes)
 	
 	if (missing(n)) {
 	    n = rbind(c(2,4,29,19),c(7,6,116,51),c(19,27,201,76),c(18,22,133,54)) # Colorectal cancer (Table 7.7)
 	#    n = [15 35 6 9 6; 2 4 2 11 11; 0 0 1 10 21]; # Breast Tumor (Table 7.8)
 	#    n = [2 3 3 3; 2 58 98 14; 8 162 949 252; 4 48 373 369]; # Self-rated health (Table 7.9)
 		a = 1:nrow(n)
 		b = 1:ncol(n)
 	} else if (missing(a)) {
 		a = 1:nrow(n)
 		b = 1:ncol(n) 		
 	} else if (missing(b)) {
 		b = 1:ncol(n) 		 	
 	}
 
 	# If no scores are given, use equally spaced scores
 	r = nrow(n); c = ncol(n)
 
 	N = sum(n)
 	
 	# Put the observed data into long format
 	Y1 = rep(0, N); Y2 = rep(0, N); id = 0
 	for (i in 1:r) {
 	    for (j in 1:c) {
 	        for (k in 1:n[i,j]) {
 	            id = id + 1
 	            Y1[id] = a[i]
 	            Y2[id] = b[j]
 	        }
 	    }
 	}
 	
 	# The covariance matrix of Y1 and Y2
 	covar = cov(cbind(Y1, Y2))
 	
 	# Estimate of the Pearson correlation coefficient
 	rP = covar[1,2]/sqrt(covar[1,1]*covar[2,2])
 	
 	# Fisher Z transformation
 	z = atanh(rP)
 	# Or, equivalently
 	#z = 0.5*log((1+rP)/(1-rP))
 	
 	# The 1-alpha percentile of the standard normal distribution
 	z_alpha = qnorm(1-alpha/2, 0, 1)
 	
 	# Confidence interval for z
 	l = z - z_alpha/sqrt(N - 3)
 	u = z + z_alpha/sqrt(N - 3)
 	
 	# Back transform to obtain the CI for the Pearson correlation coefficient
 	L = (exp(2*l) - 1)/(exp(2*l) + 1)
 	U = (exp(2*u) - 1)/(exp(2*u) + 1)
 	
 	if (printresults) {
     .print('The Pearson correlation coefficient: r = %7.4f (%g%% CI %7.4f to %7.4f)\n', rP, 100*(1-alpha), L, U)
 	}
 	
 	invisible(list(rP=rP, L=L, U=U))
 }
 
 .print = function(s, ...) {
	   print(sprintf(gsub('\n','',s), ...), quote=F)
 }
