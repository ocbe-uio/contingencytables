 Pearson_correlation_coefficient_rxc_bca = function(n, nboot, a, b, alpha=0.05, printresults=T) {
 	# The Pearson correlation coefficient with the bias-corrected and accelerated
 	# boostrap confidence interval
 	# Described in Chapter 7 "The rxc Table"
 	#
 	# Dependencies: Pearson_correlation_coefficient_rxc.m
 	#
 	# Input arguments
 	# ---------------
 	# n: the observed table (an rxc matrix)
 	# nboot: number of bootstrap samples
 	# a: scores assigned to the rows
 	# b: scores assigned to the columns
 	# alpha: the nominal significance level, used to compute a 100(1-alpha) confidence interval
 	# printresults: display results (F = no, T = yes)

 	if (missing(n)) {
 	    n = rbind(c(2,4,29,19),c(7,6,116,51),c(19,27,201,76),c(18,22,133,54)) # Colorectal cancer (Table 7.7)
 	#    n = [15 35 6 9 6; 2 4 2 11 11; 0 0 1 10 21]; # Breast Tumor (Table 7.8)
 	#    n = [2 3 3 3; 2 58 98 14; 8 162 949 252; 4 48 373 369]; # Self-rated health (Table 7.9)
 		nboot = 10000
 		a = 1:nrow(n)
 		b = 1:ncol(n)
 	} else if (missing(nboot)) {
 		nboot = 10000
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

 	# Put the observed data into long format for bootstrapping the two samples
 	Y1 = rep(0, N); Y2 = rep(0, N); id = 0
 	for (i in 1:r) {
 	    for (j in 1:c) {
 	        for (k in 1:n[i,j]) {
 	            id = id + 1
 	            Y1[id] = i
 	            Y2[id] = j
 	        }
 	    }
 	}

 	# The estimate
 	rP = Pearson_correlation_coefficient_rxc(n, a, b, alpha, printresults=F)$rP

 	# The CI bootstrap sample
 	# funchandle = @(Y1,Y2) put_data_back_into_table_format(Y1, Y2)
 	# ci = bootci(nboot, {funchandle, Y1, Y2}, 'alpha', alpha, 'type', 'bca')
 	# L = ci[1]
 	# U = ci[1]

 	dat = data.frame(Y1=Y1, Y2=Y2)
 	ans.boot = boot(dat, f.Pccrb, R=nboot, stype="i", .param=list(a,b,alpha,r,c))
 	ans.ci = boot.ci(ans.boot, conf=1-alpha, type="bca")
 	L = ans.ci$bca[4]
 	U = ans.ci$bca[5]

 	if (printresults) {
     .print('The Pearson correlation w/BCa bootstrap CI: r = %7.4f (%g%% CI %7.4f to %7.4f)\n', rP, 100*(1-alpha), L, U)
 	}

 	invisible(list(rP=rP, L=L, U=U))
 }


# ===================================================
 f.Pccrb = function(dat, indx, .param) {
 	# global aglobal bglobal alphaglobal r c
 	a = .param[[1]]; b = .param[[2]]; alpha = .param[[3]]
 	r = .param[[4]]; c = .param[[5]]
 	n = matrix(0,r,c)
 	Y1 = dat$Y1[indx]
 	Y2 = dat$Y2[indx]
 	for (id in 1:length(Y1)) {
 	     n[Y1[id], Y2[id]] = n[Y1[id], Y2[id]] + 1
 	}
 	rP = Pearson_correlation_coefficient_rxc(n, a, b, alpha, printresults=F)$rP
 	return(rP)
 }

 .print = function(s, ...) {
	   print(sprintf(gsub('\n','',s), ...), quote=F)
 }
