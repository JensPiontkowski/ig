\name{ppig}
\alias{ppig}

\title{Probability versus probability (PP) plot for the IGTD}

\description{
The function \code{ppig()} produces a PP plot for the IGTD based on their MLE. Also, a line going through the first and the third quartile can be sketched. In addition, the coefficient of determination of least squares for the fit line is given.
}

\usage{
ppig(x, kernel = "normal", line = FALSE, xlab = "Empirical distribution function", ylab = "Theorical distribution function")
}

\arguments{
  \item{x}{Vector of observations.}
  \item{kernel}{Kernel of the pdf of the associated symmetrical distribution by means of which the IGTD is obtained.
  The kernels: \code{"Laplace"}, \code{"logistic"}, \code{"normal"} and \code{"t"} are available.}
  \item{line}{Logical; if TRUE (default), a line going by the first and third quartile is sketched.}
  \item{xlab}{A title for the x axis.}
  \item{ylab}{A title for the y axis.}
}

\value{
The function \code{ppig()} carries out a PP plot for the IGTD.
}

\author{
Víctor Leiva <victor.leiva@uv.cl>, Hugo Hernández <hugo.hernandez@msn.com>, and Antonio Sanhueza <asanhue@ufro.cl>.
}

\examples{
## Generates a sample from the IGTD
## Produces a PP plot for the IGTD with g="normal"
x<-rig(300,mu=1,lambda=1,kernel="normal")
ppig(x,kernel="normal", line=TRUE)
}

\keyword{univar}
