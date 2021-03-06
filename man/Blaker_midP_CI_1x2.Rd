% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Blaker_midP_CI_1x2.R
\name{Blaker_midP_CI_1x2}
\alias{Blaker_midP_CI_1x2}
\title{The Blaker mid-P confidence interval for the binomial probability}
\usage{
Blaker_midP_CI_1x2(X, n, alpha = 0.05, printresults = TRUE)
}
\arguments{
\item{X}{the number of successes}

\item{n}{the total number of observations}

\item{alpha}{the nominal level, e.g. 0.05 for 95\% CIs}

\item{printresults}{display results (F = no, T = yes)}
}
\description{
The Blaker mid-P confidence interval for the binomial
probability
Described in Chapter 2 "The 1x2 Table and the Binomial Distribution"
}
\examples{
load_chapter(2)
# The number of 1st order male births (Singh et al. 2010)
Blaker_midP_CI_1x2(X=250, n=533)
# The number of 2nd order male births (Singh et al. 2010)
Blaker_midP_CI_1x2(X=204, n=412)
# The number of 3rd order male births (Singh et al. 2010)
Blaker_midP_CI_1x2(X=103, n=167)
# The number of 4th order male births (Singh et al. 2010)
Blaker_midP_CI_1x2(X=33, n=45)
# Ligarden et al. (2010)
Blaker_midP_CI_1x2(X=13, n=16)
unload_chapter(2)
}
\references{
Blaker H (2000) Confidence curves and improved exact
confidence intervals for discrete distributions. The Canadian Journal of
Statistics; 28:783-798
}
