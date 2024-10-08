% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/negbinAR.R
\name{rnbinomAR}
\alias{rnbinomAR}
\title{Generate autocorrelated random numbers with negative binomial distribution}
\usage{
rnbinomAR(n, size, prob, mu, rho, return_Y = FALSE)
}
\arguments{
\item{n}{number of observations.  If \code{length(n) > 1}, the length
    is taken to be the number required.}

\item{size}{target for number of successful trials, or dispersion
    parameter (the shape parameter of the gamma mixing distribution).
    Must be strictly positive, need not be integer.}

\item{prob}{probability of success in each trial. \code{0 < prob <= 1}.}

\item{mu}{alternative parametrization via mean: see \sQuote{Details}.}

\item{rho}{autocorrelation at lag 1}

\item{return_Y}{whether to return the intermediate Y distribution used to generate X. default FALSE.}
}
\value{
vector of length n
}
\description{
Generate autocorrelated random numbers with negative binomial distribution
}
\examples{
rnbinomAR(100, size = 1.3, mu = 3, rho = 0.8)
}
