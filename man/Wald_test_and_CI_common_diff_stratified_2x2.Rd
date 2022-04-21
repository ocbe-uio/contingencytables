% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Wald_test_and_CI_common_diff_stratified_2x2.R
\name{Wald_test_and_CI_common_diff_stratified_2x2}
\alias{Wald_test_and_CI_common_diff_stratified_2x2}
\title{The Wald test and CI for a common difference between probabilities}
\usage{
Wald_test_and_CI_common_diff_stratified_2x2(
  n,
  estimatetype = "MH",
  alpha = 0.05,
  printresults = TRUE
)
}
\arguments{
\item{n}{the observed table (a 2x2xk matrix, where k is the number of strata)}

\item{estimatetype}{Mantel-Haenszel or inverse variance estimate ('MH' or 'IV')}

\item{alpha}{the nominal level, e.g. 0.05 for 95\% CIs}

\item{printresults}{display results (FALSE = no, TRUE = yes)}
}
\value{
A list containing the two-sided p-value, the Wald test statistic, and the lower, upper and point estimates for delta
}
\description{
The Wald test and CI for a common difference between probabilities

based on either the Mantel-Haenszel or inverse variance estimate

Described in Chapter 10 "Stratified 2x2 Tables and Meta-Analysis"
}
\examples{
# Smoking and lung cancer (Doll and Hill, 1950)
n <- array(dim = c(2, 2, 2))
n[, , 1] <- matrix(c(647, 622, 2, 27), 2, byrow = TRUE)
n[, , 2] <- matrix(c(41, 28, 19, 32), 2, byrow = TRUE)
Wald_test_and_CI_common_diff_stratified_2x2(n)

# Prophylactice use of Lidocaine in myocardial infarction (Hine et al., 1989)
n <- array(0, dim = c(2, 2, 6))
n[, , 1] <- rbind(c(2, 37), c(1, 42))
n[, , 2] <- rbind(c(4, 40), c(4, 40))
n[, , 3] <- rbind(c(6, 101), c(4, 106))
n[, , 4] <- rbind(c(7, 96), c(5, 95))
n[, , 5] <- rbind(c(7, 103), c(3, 103))
n[, , 6] <- rbind(c(11, 143), c(4, 142))
Wald_test_and_CI_common_diff_stratified_2x2(n)

}