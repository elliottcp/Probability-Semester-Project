% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Script - Project Part 1.R
\name{THEFUNCTION1D}
\alias{THEFUNCTION1D}
\title{THEFUNCTION1D}
\usage{
THEFUNCTION1D(n = 1, pdf, lower_bound = 0, upper_bound = 1, C = 1)
}
\arguments{
\item{n}{The desired number of samples from the given distribution.}

\item{pdf}{A probability density function.  The user is responsible for ensuring it is valid.}

\item{lower_bound}{The lower limit of possible sample values.}

\item{upper_bound}{The upper limit of possible sample values.}

\item{C}{A real number greater than all possible values of the pdf.}
}
\value{
Random sample in the form of a numeric vector.
}
\description{
This function produces a random sample for any given continuous, finite probability distribution.
}
\examples{

pdf <- function(x) {dnorm(x,0,1)}
sample <- THEFUNCTION1D(pdf, n = 100000, lower_bound = -5, upper_bound = 5, C = 0.4)

pdf <- function(x) {dunif(x,-10,10)}
sample <- THEFUNCTION1D(pdf, n = 200, lower_bound = -10, upper_bound = 10, C = 0.06)

If the function produces an error, check for faulty parameters,
such as n less than 1, non-numeric lower_bound, non-numeric
upper_bound, non-numeric C, or lower_bound greater than the upper_bound.
}
