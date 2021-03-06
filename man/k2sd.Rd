% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utilities.R
\name{k2sd}
\alias{k2sd}
\title{Standard deviation of von Mises kappa parameter
Returns the standard deviation of a wrapped normal distribution
corresponding to a von Mises concentration parameter of kappa}
\source{
The code has been adapted from Matlab code written by Paul Bays
(https://paulbays.com).
}
\usage{
k2sd(k)
}
\arguments{
\item{k}{A numeric value for concentration parameter kappa of the von
Mises distribution}
}
\description{
Standard deviation of von Mises kappa parameter
Returns the standard deviation of a wrapped normal distribution
corresponding to a von Mises concentration parameter of kappa
}
\examples{
sd <- k2sd(8.4)

}
