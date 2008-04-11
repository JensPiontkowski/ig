\name{aciigt}

\alias{aciigt}

\title{Approximate confidence region for the parameters of the IGTD}

\description{
  The function \code{aciigt()} produces a plot of an approximate confidence 
  region and computes approximate confidence intervals (ACI) for the parameters
  mu and lambda of the IGTD from a sample of observations.
}

\usage{
  aciigt(x, kernel = "normal", confLevel = 95, 
         chart = c(NULL, NULL, NULL, NULL), 
         colourRegion = 1,
         colourEstimates = 2)
}

\arguments{
  \item{x}{Vector of observations.}
  \item{kernel}{Kernel of the pdf of the associated symmetrical distribution by 
                means of which the IGTD is obtained. The kernels: 
                \code{"laplace"}, \code{"logistic"}, \code{"normal"} and 
                \code{"t"} are available.}
  \item{confLevel}{Confidence level of the region.}
  \item{chart}{Vector of limits of the graphs. It is a vector of the type: \cr
               \code{c(xmin, xmax, ymin, ymax)}.
}
  \item{colourRegion}{Color of an approximate confidence region in the plot.}
  \item{colourEstimates}{Color of MLE estimators in the plot.}
}

\details{
  In order to construct a confidence region for \eqn{\mu} and \eqn{\lambda}, 
  we use the asymptotic normality of the MLEs.
}

\value{ 
 \code{aciigt()} shows a plot of an approximate confidence region and computes 
  approximate confidence intervals (ACI) for the parameters of the IGTD 
  considering the established confidence level from a sample of observatons giving 
  results according to the following list:
  \item{muEstimate}{Return the value of the MLE of mu.}
  \item{muAci}{Return 95\% ACI for mu.}
  \item{lambdaEstimate}{Return the value of the MLE of lambda.}
  \item{lambdaAci}{Return 95\% ACI for lambda.}
  }

\references{Sanhueza, A., Leiva, V., Balakrishnan, N. (2008). A new class of 
            inverse Gaussian type distributions. Metrika (in press).}

\author{
Víctor Leiva <victor.leiva@uv.cl; victor.leiva@yahoo.com>, \cr
Hugo Hernández <hugo.hernandez.p@gmail.com> and \cr
Antonio Sanhueza <asanhueza@ufro.cl>.
}

\examples{
## Generates a sample from the IGTD with normal kernel
x <- rigt(300, mu = 1.0, lambda = 1.0, kernel = "normal")

## Estimates the parameters of the IGTD with normal kernel by means of a 
## 95% approximate confidence region and produces a graphical confedence 
## region
aciigt(x, kernel = "normal", confLevel = 95, colourRegion = 3)
}

\keyword{univar}
\keyword{htest}
\keyword{math}