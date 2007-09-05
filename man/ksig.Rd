\name{ksig}
\alias{ksig}

\title{Test of Kolmogorov-Smirnov for the inverse Gaussian type distribution}

\description{
The function ksig gives the values for the KS test assuming an IGTD with parameters mu, lambda
and an specific kernel. In addition, optionally, this function allows one to show a comparative graph between
the empirical and theoretical cdfs for a given data set.
}

\usage{
ksig(x, kernel = "normal", alternative = "two.sided", graph = TRUE, xlab = NULL, ylab = NULL)
}

\arguments{
  \item{x}{Vector of observations.}
  \item{kernel}{Kernel of the pdf of the associated symmetrical distribution by means of which the
  IGTD is obtained. The kernels: \code{"Laplace"}, \code{"logistic"}, \code{"normal"} and \code{"t"} are available.}
  \item{alternative}{indicates the alternative hypothesis and must be one of \code{"two.sided"} (default), \code{"less"}, or \code{"greater"}. }
  \item{graph}{Logical; if TRUE (default), the cdf plot is provided.}
  \item{xlab}{A title for the x axis.}
  \item{ylab}{A title for the y axis.}
}

\details{
The Kolmogorov-Smirnov test is a goodness-of-fit technique based on the maximum distance between the empirical
and theoretical cdfs.
}

\value{
The function \code{ksig()} carries out the KS test for the IGTD.
}

\author{
Víctor Leiva <victor.leiva@uv.cl>, Hugo Hernández <hugo.hernandez@msn.com>, and Antonio Sanhueza <asanhue@ufro.cl>.
}

\keyword{univar}
