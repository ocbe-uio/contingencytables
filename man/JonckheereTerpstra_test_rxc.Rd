% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/JonckheereTerpstra_test_rxc.R
\name{JonckheereTerpstra_test_rxc}
\alias{JonckheereTerpstra_test_rxc}
\title{The Jonckheere-Terpstra test for association}
\usage{
JonckheereTerpstra_test_rxc(n)
}
\arguments{
\item{n}{the observed table (an rxc matrix)}
}
\value{
An object of the \link{contingencytables_result} class,
basically a subclass of \code{\link[base:list]{base::list()}}. Use the \code{\link[utils:str]{utils::str()}} function
to see the specific elements returned.
}
\description{
The Jonckheere-Terpstra test for association

Described in Chapter 7 "The rxc Table"
}
\examples{
JonckheereTerpstra_test_rxc(table_7.7)
JonckheereTerpstra_test_rxc(table_7.8)
JonckheereTerpstra_test_rxc(table_7.9)
}
