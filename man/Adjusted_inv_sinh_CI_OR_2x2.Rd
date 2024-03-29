% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Adjusted_inv_sinh_CI_OR_2x2.R
\name{Adjusted_inv_sinh_CI_OR_2x2}
\alias{Adjusted_inv_sinh_CI_OR_2x2}
\title{The adjusted inverse hyperbolic sine confidence interval for the odds
ratio}
\usage{
Adjusted_inv_sinh_CI_OR_2x2(n, psi1 = 0.45, psi2 = 0.25, alpha = 0.05)
}
\arguments{
\item{n}{the observed counts (a 2x2 matrix)}

\item{psi1}{pseudo-frequency (should be > 0)}

\item{psi2}{pseudo-frequency (should be > 0)}

\item{alpha}{the nominal level, e.g. 0.05 for 95\% CIs}
}
\value{
An object of the \link{contingencytables_result} class,
basically a subclass of \code{\link[base:list]{base::list()}}. Use the \code{\link[utils:str]{utils::str()}} function
to see the specific elements returned.
}
\description{
The adjusted inverse hyperbolic sine confidence interval for
the odds ratio.

Described in Chapter 4 "The 2x2 Table"
}
\examples{
Adjusted_inv_sinh_CI_OR_2x2(lampasona_2013)
Adjusted_inv_sinh_CI_OR_2x2(ritland_2007)
}
