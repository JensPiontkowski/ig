\name{ACIig}
\alias{ACIig}

\title{Approximate confidence region for the parameters of the IGTD}

\description{
The function \code{ACIig()} produces a plot of an approximate confidence region and computes approximate confidence
intervals (ACI) for the parameters mu and lambda of the IGTD from a sample of observations.
}

\usage{
ACIig(x, kernel = "normal", conf.level = 95, chart = c(NULL, NULL, NULL, NULL), col = 1)
}

\arguments{
  \item{x}{Vector of observations.}
  \item{kernel}{Kernel of the pdf of the associated symmetrical distribution by means of which the IGTD is obtained.
  The kernels: \code{"Laplace"}, \code{"logistic"}, \code{"normal"} and \code{"t"} are available.}
  \item{conf.level}{Confidence level of the region.}
  \item{chart}{Vector of limits of the graphs. It is a vector of the type: \code{c(xmin,xmax,ymin,ymax)}.}
  \item{col}{Color of an approximate confidence region in the plot.}
}

\details{
In order to construct a confidence region for \eqn{\mu} and \eqn{\lambda}, we use the asymptotic normality of the MLE.
}

\value{
\code{ACIig()} shows a plot of an approximate confidence region and computes ACI for the parameters of the IGTD from a sample of observations.
}

\references{
Sanhueza, A., Leiva, V. and Balakrishnan, N. (2007). A new class of inverse Gaussian type distributions. Metrika (in press).
}

\author{
Víctor Leiva <victor.leiva@uv.cl>, Hugo Hernández <hugo.hernandez@msn.com>, and Antonio Sanhueza <asanhue@ufro.cl>.
}

\examples{
## Generates a sample from the IGTD
## Estimates the parameters of the IGTD with g="normal" by means of a confidence region
x<-rig(300,mu=1,lambda=1,kernel="normal")
ACIig(x, kernel="normal")
}

\keyword{univar}
