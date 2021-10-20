 McNemar_asymptotic_test_CC_paired_2x2 = function(n, printresults=T) {
 	# The McNemar asymptotic test with continuity correction
 	# Described in Chapter 8 "The Paired 2x2 Table"
 	#
 	# Input arguments
 	# ---------------
 	# n: the observed table (a 2x2 matrix)
 	# printresults: display results (F = no, T = yes)
 	
 
 	if (missing(n)) {
 	    n = rbind(c(1,1),c(7,12)) # Airway hyper-responsiveness before and after stem cell transplantation (Bentur et al., 2009)
 		#     n = [59 6; 16 80]; # Complete response before and after consolidation therapy (Cavo et al., 2012)
 		#     n = [7 25; 2 68]; # Floppy eyelid syndrome vs obstructive sleep apnea (Ezra et al., 2010)
 	}
 	
 	# The number of discordant pairs
 	nd = n[1,2] + n[2,1]
 	if(nd == 0) {
 	    if (printresults) {
         .print('No discordant pairs\n')
         .print('P = 1.0\n')
 	    }
 	    P = 1
 	    return
 	}
 	
 	# The McNemar test statistic with continuity correction
 	Z = (abs(n[1,2] - n[2,1]) - 1)/sqrt(nd)
 	
 	# Reference distribution: standard normal
 	P = 2*(1 - pnorm(abs(Z), 0, 1))
 	
 	if (printresults) {
     .print('The asymptotic McNemar test with continuity correction: P = %8.6f, Z = %6.3f\n', P, Z)
 	}
 	
 	invisible(list(P=P,Z=Z))
 }

 	
 .print = function(s, ...) {
	   print(sprintf(gsub('\n','',s), ...), quote=F)
 }
