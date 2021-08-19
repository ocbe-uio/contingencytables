.print = function(s, ...) {
	print(sprintf(gsub('\n','',s), ...), quote=F)
}
	
Exact_cond_midP_linear_rank_tests_2xc = function(n, b, printresults=T) {
	# Exact conditional and mid-P linear rank tests
	# Described in Chapter 6 "The Ordered 2xc Table"
	#
	# Input arguments
	# ---------------
	# n: the observed table (a 2xc matrix)
	# b: scores assigned to the columns (if b=0, midranks will be used as scores)
	# printresults: display results (F = no, T = yes)

	if (missing(b)) {
	   b = 0
	} 
	if (missing(n)) {
		#  n = rbind(c(8, 28, 72, 126),c(46, 73, 69, 86)) # The Adolescent Placement Study (Fontanella et al., 2008)
	    n = rbind(c(14, 10, 3, 2), c(11, 7, 8, 4)) # Postoperative nausea (Lydersen et al., 2012a)
	}
	
	c0 = ncol(n)
	nip = apply(n, 1, sum)
	npj = apply(n, 2, sum)
	N = sum(n)
	
	# Use scores = midranks as default
	if (b == 0) {
	    b = rep(0, c0)
	    for (j in 1:c0) {
	    	a0 = ifelse(j>1, sum(npj[1:(j-1)]), 0)
	    	b0 = 1 + sum(npj[1:j])
	        b[j] = 0.5 * (a0 + b0)
	    }
	}
	
	# Calculate all nchoosek beforehand
	npj_choose_x1j = matrix(0, c0, max(npj)+1)
	for (j in 1:c0) {
	    for (x1j in 0:npj[j]) {
	        npj_choose_x1j[j, x1j+1] = choose(npj[j], x1j)
	    }
	}
	N_choose_n1p = choose(N, nip[1])
	
	# The observed value of the test statistic
	Tobs = linear_rank_test_statistic(n[1,], b)
	
	# Calculate the smallest one-sided P-value and the point probability
	# Need separate functions for different values of c (the number of columns)
	if (c0 == 3) {
		tmp = calc_Pvalue_2x3(Tobs, nip, npj, N_choose_n1p, npj_choose_x1j, b)
	    one_sided_P = tmp$one_sided_P
	    point_prob = tmp$point_prob
	} else if (c0 == 4) {
		tmp = calc_Pvalue_2x4(Tobs, nip, npj, N_choose_n1p, npj_choose_x1j, b)
	    one_sided_P = tmp$one_sided_P
	    point_prob = tmp$point_prob
	}
	
	# Two-sided twice-the-smallest tail P-value and mid-P value
	P = 2 * one_sided_P
	midP = 2 * (one_sided_P - 0.5 * point_prob)
	
	if (printresults) {
	    .print('Exact cond. linear rank test: P = %7.5f\n', P)
	    .print('Mid-P linear rank test:   mid-P = %7.5f\n', midP)
	}
	
	invisible(data.frame(P=P, midP=midP))
}

# Brute force calculations of the one-sided P-values. Return the smallest one.
# This function assumes c=3 columns

calc_Pvalue_2x3 = function(Tobs, nip, npj, N_choose_n1p, npj_choose_x1j, b) {
	left_sided_P = 0
	right_sided_P = 0
	point_prob = 0
	for (x1 in 0:min(c(npj[1], nip[1]))) {
	    for (x2 in 0:min(c(npj[2], nip[1]-x1))) {
	        x3 = nip[1] - x1 - x2
	        if (x3 > npj[3]) {
	        	next
	        }
	        x = c(x1, x2, x3)
	        T0 = linear_rank_test_statistic(x, b)
	        f = calc_prob(x, 3, N_choose_n1p, npj_choose_x1j)
	        if (T0 == Tobs) {
	            point_prob = point_prob + f
	        } else if (T0 < Tobs) {
	            left_sided_P = left_sided_P + f
	        } else if (T0 > Tobs) {
	            right_sided_P = right_sided_P + f
	        }
	    }
	}
	one_sided_P = min(c(left_sided_P, right_sided_P)) + point_prob
	data.frame(one_sided_P=one_sided_P, point_prob=point_prob)
}
	
# Brute force calculations of the one-sided P-values. Return the smallest one.
# This function assumes c=4 columns

calc_Pvalue_2x4 = function(Tobs, nip, npj, N_choose_n1p, npj_choose_x1j, b) {
	left_sided_P = 0
	right_sided_P = 0
	point_prob = 0
	for (x1 in 0:min(c(npj[1], nip[1]))) {
	    for (x2 in 0:min(c(npj[2], nip[1]-x1))) {
	        for (x3 in 0:min(c(npj[3], nip[1]-x1-x2))) {
	            x4 = nip[1] - x1 - x2 - x3
	            if (x4 > npj[4]) {
	            	next
	            }
	            x = c(x1, x2, x3, x4)
	            T0 = linear_rank_test_statistic(x, b)
	            f = calc_prob(x, 4, N_choose_n1p, npj_choose_x1j)
	            if (T0 == Tobs) {
	            	point_prob = point_prob + f
	        	} else if (T0 < Tobs) {
	            	left_sided_P = left_sided_P + f
	        	} else if (T0 > Tobs) {
	            	right_sided_P = right_sided_P + f
	        	}
	        }
	    }
	}
	one_sided_P = min(c(left_sided_P, right_sided_P)) + point_prob
	data.frame(one_sided_P=one_sided_P, point_prob=point_prob)
}
	
# The linear rank test statistic
linear_rank_test_statistic = function(x, b) {
	T0 = sum(x * b)
	return(T0)
}
	
# Calculate the probability of table x
# (multiple hypergeometric distribution)

calc_prob = function(x, c, N_choose_n1p, npj_choose_x1j) {
	f = 1
	for (j in 1:c) {
	    f = f * npj_choose_x1j[j, x[j]+1]
	}
	f = f/N_choose_n1p
	return(f)
}
	
	