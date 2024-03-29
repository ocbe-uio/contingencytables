% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Exact_cond_midP_unspecific_ordering_rx2.R
\name{Exact_cond_midP_unspecific_ordering_rx2}
\alias{Exact_cond_midP_unspecific_ordering_rx2}
\title{The exact conditional and mid-P tests for unspecific ordering}
\usage{
Exact_cond_midP_unspecific_ordering_rx2(n, direction, statistic = "Pearson")
}
\arguments{
\item{n}{the observed counts (an rx2 matrix)}

\item{direction}{the direction of the success probabilities ("increasing"
or "decreasing")}

\item{statistic}{the Pearson test statistic ("Pearson") or the likelihood
ratio test statistic ("LR"). Can also be used for cumulative ORs in
2xc tables with "PearsonCumOR" or "LRCumOR".}
}
\value{
An object of the \link{contingencytables_result} class,
basically a subclass of \code{\link[base:list]{base::list()}}. Use the \code{\link[utils:str]{utils::str()}} function
to see the specific elements returned.
}
\description{
The exact conditional and mid-P tests for unspecific ordering.
May also be used for 2xc tables, after flipping rows and columns (i.e. if
n is a 2xc table, call this function with n' (the transpose of n) as
the first argument).

Described in Chapter 5 "The Ordered rx2 Table"
}
\examples{
# Chapter 6: Postoperative nausea (Lydersen et al., 2012a)
n <- t(lydersen_2012a)
Exact_cond_midP_unspecific_ordering_rx2(n, "decreasing")
Exact_cond_midP_unspecific_ordering_rx2(n, "decreasing", "PearsonCumOR")
}
