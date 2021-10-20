 McNemar_exact_cond_test_paired_2x2 = function(n, printresults=T) {
 	# The McNemar exact conditional test
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
 	
 	
 	# Exact p-value based on the binomial distribution
 	P = 2*pbinom(min(n[1,2], n[2,1]), n[1,2] + n[2,1], 0.5)
 	P = min(P, 1)
 	
 	
 	# Alternative formulation
 	# -----------------------
 	# nd = n[1,2] + n[2,1]
 	# P = 0
 	# for x12 = 0:min(n[1,2], n[2,1]),
 	#     P = P + choose(nd, x12)*(1/2)^nd
 	# end
 	# P = 2*P
 	#
 	
 	# Another alternative formulation
 	# -------------------------------
 	# nd = n[1,2] + n[2,1]
 	# Tobs = n[1,2] - n[2,1]
 	# P = 0
 	# for i = 0:nd,
 	#     T0 = i - (nd-i)
 	#     if abs(T0) >= abs(Tobs),
 	#         P = P + dbinom(i, nd, 0.5)
 	#     end
 	# end
 	
 	if (printresults) {
     .print('The McNemar exact conditional test: P = %8.6f\n', P)
 	}
 	
 	invisible(P)
 }
 	
 .print = function(s, ...) {
	   print(sprintf(gsub('\n','',s), ...), quote=F)
 }
