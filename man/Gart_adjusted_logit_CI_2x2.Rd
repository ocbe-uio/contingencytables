% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Gart_adjusted_logit_CI_2x2.R
\name{Gart_adjusted_logit_CI_2x2}
\alias{Gart_adjusted_logit_CI_2x2}
\title{The Gart adjusted logit confidence interval for the odds ratio}
\usage{
Gart_adjusted_logit_CI_2x2(n, alpha = 0.05, printresults = TRUE)
}
\arguments{
\item{n}{the observed table (a 2x2 matrix)}

\item{alpha}{the nominal level, e.g. 0.05 for 95\% CIs}

\item{printresults}{display results (F = no, T = yes)}
}
\description{
The Gart adjusted logit confidence interval for the odds ratio

Described in Chapter 4 "The 2x2 Table"
}
\examples{
load_chapter(4)
# A case-control study of GADA exposure on IPEX syndrome (Lampasona et al., 2013):
n <- matrix(c(9,4,4,10), nrow=2, byrow=TRUE)
Gart_adjusted_logit_CI_2x2(n)
# The association between CHRNA4 genotype and XFS (Ritland et al., 2007):
n <- matrix(c(0,16,15,57), nrow=2, byrow=TRUE)
Gart_adjusted_logit_CI_2x2(n)
unload_chapter(4)
}
