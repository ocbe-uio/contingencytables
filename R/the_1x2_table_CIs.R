the_1x2_table_CIs = function(X, n, alpha=0.05) {

    if (missing(n)) {
      #    X = 250; n = 533; # Example: The number of 1st order male births (Singh et al. 2010)
      #    X = 204; n = 412; # Example: The number of 2nd order male births (Singh et al. 2010)
      #    X = 103; n = 167; # Example: The number of 3rd order male births (Singh et al. 2010)
      #    X = 33; n = 45;   # Example: The number of 4th order male births (Singh et al. 2010)
      X = 13; n = 16;   # Example: Ligarden et al. (2010)
    }

    estimate = X/n
    print(sprintf('Estimate of pi: %i/%i = %5.3f', X, n, estimate), quote=F)

    print(sprintf('Interval method                  %i%% CI        width', 100*(1-alpha)), quote=F)
    print('----------------------------------------------------', quote=F)

    res = Wald_CI_1x2(X, n, alpha, F)
    print(sprintf('Wald                         %5.3f to %5.3f %8.3f', res[1], res[2], res[2] - res[1]), quote=F)

    res = Wald_CI_CC_1x2(X, n, alpha, F)
    print(sprintf('Wald with CC                 %5.3f to %5.3f %8.3f', res[1], res[2], res[2] - res[1]), quote=F)

    res = LR_CI_1x2(X, n, alpha, F)
    print(sprintf('Likelihood ratio             %5.3f to %5.3f %8.3f', res[1], res[2], res[2] - res[1]), quote=F)

    res = Wilson_score_CI_1x2(X, n, alpha, F)
    print(sprintf('Wilson score                 %5.3f to %5.3f %8.3f', res[1], res[2], res[2] - res[1]), quote=F)

    res = Wilson_score_CI_CC_1x2(X, n, alpha, F)
    print(sprintf('Wilson score with CC         %5.3f to %5.3f %8.3f', res[1], res[2], res[2] - res[1]), quote=F)

    res = AgrestiCoull_CI_1x2(X, n, alpha, F)
    print(sprintf('Agresti-Coull                %5.3f to %5.3f %8.3f', res[1], res[2], res[2] - res[1]), quote=F)

    res = Jeffreys_CI_1x2(X, n, alpha, F)
    print(sprintf('Jeffreys                     %5.3f to %5.3f %8.3f', res[1], res[2], res[2] - res[1]), quote=F)

    res = Arcsine_CI_1x2(X, n, alpha, F)
    print(sprintf('Arcsine (Anscombe)           %5.3f to %5.3f %8.3f', res[1], res[2], res[2] - res[1]), quote=F)

    res = ClopperPearson_exact_CI_1x2(X, n, alpha, F)
    print(sprintf('Clopper-Pearson exact        %5.3f to %5.3f %8.3f', res[1], res[2], res[2] - res[1]), quote=F)

    #res = ClopperPearson_exact_CI_1x2_beta_version(X, n, alpha, F)
    #print(sprintf('Clopper-Pearson exact (beta) %5.3f to %5.3f %8.3f', res[1], res[2], res[2] - res[1]), quote=F)

    res = Blaker_exact_CI_1x2(X, n, alpha, F)
    print(sprintf('Blaker exact                 %5.3f to %5.3f %8.3f', res[1], res[2], res[2] - res[1]), quote=F)

    res = ClopperPearson_midP_CI_1x2(X, n, alpha, F)
    print(sprintf('Clopper-Pearson mid-p        %5.3f to %5.3f %8.3f', res[1], res[2], res[2] - res[1]), quote=F)

    res = Blaker_midP_CI_1x2(X, n, alpha, F)
    print(sprintf('Blaker mid-P                 %5.3f to %5.3f %8.3f', res[1], res[2], res[2] - res[1]), quote=F)

    print('----------------------------------------------------', quote=F)
    print('CC = continuity correction', quote=F)

}
