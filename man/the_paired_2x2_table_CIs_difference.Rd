% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/the_paired_2x2_table_CIs_difference.R
\name{the_paired_2x2_table_CIs_difference}
\alias{the_paired_2x2_table_CIs_difference}
\title{The Paired 2x2 table CIs difference}
\usage{
the_paired_2x2_table_CIs_difference(n, alpha = 0.05)
}
\arguments{
\item{n}{frequency matrix}

\item{alpha}{type I error}
}
\value{
NULL. This function should be called for its printed output.
}
\description{
The Paired 2x2 table CIs difference
}
\examples{
# Airway hyper-responsiveness before and after stem cell transplantation
# (Bentur et al., 2009)
the_paired_2x2_table_CIs_difference(bentur_2009)

# Complete response before and after consolidation therapy
# (Cavo et al., 2012)
the_paired_2x2_table_CIs_difference(cavo_2012)

}
