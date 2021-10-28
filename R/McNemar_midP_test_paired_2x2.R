 McNemar_midP_test_paired_2x2 = function(n, printresults=T) {
 	# The McNemar mid-P test
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
 	
 	if (n[1,2] == n[2,1]) {
 	    midP = 1 - 0.5*dbinom(n[1,2], n[1,2] + n[2,1], 0.5)
 	} else {
 	    P = 2*pbinom(min(n[1,2], n[2,1]), n[1,2] + n[2,1], 0.5)
 	    P = min(P, 1)
 	    midP = P - dbinom(n[1,2], n[1,2] + n[2,1], 0.5)
 	}
 	
 	if (printresults) {
     .print('The McNemar mid-P test: P = %8.6f\n', midP)
 	}
 	
 	invisible(midP)
 }
 	
 .print = function(s, ...) {
	   print(sprintf(gsub('\n','',s), ...), quote=F)
 }
