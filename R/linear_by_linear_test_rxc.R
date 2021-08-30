linear_by_linear_test_rxc = function(n, a, b, printresults=T) {
 	# The linear-by-linear test for association
 	# Described in Chapter 7 "The rxc Table"
 	#
 	# Input arguments
 	# ---------------
 	# n: the observed table (an rxc matrix)
 	# a: scores assigned to the rows
 	# b: scores assigned to the columns
 	# printresults: display results (0 = no, 1 = yes)
 	
 
 	if (missing(n)) {
 	    n = rbind(c(2,4,29,19),c(7,6,116,51),c(19,27,201,76),c(18,22,133,54)) # Colorectal cancer (Jullumstrø et al., 2009)
 	#    n = [15 35 6 9 6; 2 4 2 11 11; 0 0 1 10 21]; # Breast Tumor (Bofin et al., 2004)
 	#    n = [2 3 3 3; 2 58 98 14; 8 162 949 252; 4 48 373 369]; # Self-rated health (Breidablik et al., 2008)
 		a = 1:ncol(n)
 		b = 1:nrow(n)
 	} else if (missing(a)) {
 		a = 1:ncol(n)
 		b = 1:nrow(n)		
 	} else if (missing(b)) {
 		b = 1:nrow(n)
 	}
 	
 	# If no scores are given, use equally spaced scores
 	r = nrow(n); c = ncol(n)
 	
 	N = sum(n)
 	
 	# Put the observed data into long format
 	Y1 = rep(0, N)
 	Y2 = rep(0, N)
 	id = 0
 	for (i in 1:r) {
 	    for (j in 1:c) {
 	        for (k in 1:n[i,j]) {
 	            id = id + 1
 	            Y1[id] = a[i]
 	            Y2[id] = b[j]
 	        }
 	    }
 	}
 	
 	# The Pearson correlation coefficient
 	r = cor(Y1, Y2)
 	
 	# The linear-by-linear test statistic
 	Z = sqrt(N - 1)*r
 	P = 2*(1 - pnorm(abs(Z), 0, 1))
 	
 	# Alternative version with chi-squared 1 dof distribution
 	#T = (N - 1)*r^2
 	#P = 1 - pchisq(T, 1)
 	
 	if (printresults) {
     .print('The linear-by-linear test for association: P = %8.6f, Z = %6.3f\n', P, Z)
 	}
 	
	invisible(list(P=P,Z=Z))
}

.print = function(s, ...) {
	print(sprintf(gsub('\n','',s), ...), quote=F)
}
