% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CochranMantelHaenszel_test_stratified_2x2.R
\name{CochranMantelHaenszel_test_stratified_2x2}
\alias{CochranMantelHaenszel_test_stratified_2x2}
\title{The Cochran-Mantel-Haenszel test of a common odds ratio}
\usage{
CochranMantelHaenszel_test_stratified_2x2(n, printresults = TRUE)
}
\arguments{
\item{n}{the observed table (a 2x2xk matrix, where k is the number of strata)}

\item{printresults}{display results (FALSE = no, TRUE = yes)}
}
\description{
The Cochran-Mantel-Haenszel test of a common odds ratio

Described in Chapter 10 "Stratified 2x2 Tables and Meta-Analysis"
}
\examples{
load_chapter(10)
# Smoking and lung cancer (Doll and Hill, 1950)
n <- array(dim = c(2, 2, 2))
n[, , 1] <- matrix(c(647, 622, 2, 27), 2, byrow = TRUE)
n[, , 2] <- matrix(c(41, 28, 19, 32), 2, byrow = TRUE)
CochranMantelHaenszel_test_stratified_2x2(n)

# Prophylactice use of Lidocaine in myocardial infarction (Hine et al., 1989)
n <- array(0, dim = c(2, 2, 6))
n[, , 1] <- rbind(c(2, 37), c(1, 42))
n[, , 2] <- rbind(c(4, 40), c(4, 40))
n[, , 3] <- rbind(c(6, 101), c(4, 106))
n[, , 4] <- rbind(c(7, 96), c(5, 95))
n[, , 5] <- rbind(c(7, 103), c(3, 103))
n[, , 6] <- rbind(c(11, 143), c(4, 142))
CochranMantelHaenszel_test_stratified_2x2(n)

unload_chapter(10)
}