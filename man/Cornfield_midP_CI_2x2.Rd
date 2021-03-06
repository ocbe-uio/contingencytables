% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Cornfield_midP_CI_2x2.R
\name{Cornfield_midP_CI_2x2}
\alias{Cornfield_midP_CI_2x2}
\title{The Cornfield mid-P confidence interval for the odds ratio}
\usage{
Cornfield_midP_CI_2x2(n, alpha = 0.05, printresults = TRUE)
}
\arguments{
\item{n}{the observed table (a 2x2 matrix)}

\item{alpha}{the nominal level, e.g. 0.05 for 95# CIs}

\item{printresults}{display results (0 = no, 1 = yes)}
}
\description{
The Cornfield mid-P confidence interval for the odds ratio

Described in Chapter 4 "The 2x2 Table"
}
\examples{
load_chapter(4)
n <- rbind(c(3,1), c(1,3))     # Example: A lady tasting a cup of tea
Cornfield_midP_CI_2x2(n)
n <- rbind(c(7,27), c(1,33))   # Example: Perondi et al. (2004)
Cornfield_midP_CI_2x2(n)
n <- rbind(c(9,4), c(4,10))    # Example: Lampasona et al. (2013)
Cornfield_midP_CI_2x2(n)
n <- rbind(c(0,16), c(15,57))  # Example: Ritland et al. (2007)
Cornfield_midP_CI_2x2(n)
unload_chapter(4)
}
