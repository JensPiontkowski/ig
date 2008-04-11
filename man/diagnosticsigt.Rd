\name{diagnosticsigt}

\alias{diagnosticsigt}

\title{Influence diagnostics plots for the IGTD}

\description{
  The function \code{diagnosticsigt()} produces an index plot of total 
  local influence.
}

\usage{
  diagnosticsigt(x, kernel = "normal", mainTitle = "", yRange = NULL)
}

\arguments{
  \item{x}{Vector of observations.}
  \item{kernel}{Kernel of the pdf of the associated symmetrical distribution by 
                means of which the IGTD is obtained. The kernels: 
                \code{"laplace"}, \code{"logistic"}, \code{"normal"} and 
                \code{"t"} are available.}
  \item{mainTitle}{An overall title for the plot.}
  \item{yRange}{Limit for the y axis.}
}

\details{
  The local influence diagnostics method (Cook, 1986) is used to evaluate the 
  local influence by means of likelihood displacement.
}

\value{
  \code{diagnosticsigt()} gives an influence diagnostics through a graphical 
  plot for the IGTD from a sample of observations.
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

## Produces influence diagnostics for the IGTD with normal kernel
diagnosticsigt(x, kernel = "normal")
}

\keyword{univar}

