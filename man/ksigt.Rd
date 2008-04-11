\name{ksigt}

\alias{ksigt}

\title{Test of Kolmogorov-Smirnov for the IGTD}

\description{
  The function \code{ksigt} gives the values for the Kolmogorov-Smirnov (KS) 
  test assuming an IGTD with parameters mu, lambda and an specific kernel. 
  In addition, optionally, this function allows one to obtain a comparative 
  graph between the empirical and theoretical cdfs for a given data set.
}

\usage{
  ksigt(x, kernel = "normal", graph = FALSE, 
        mainTitle = "Cumulative distribution function", xLabel = "data", 
        yLabel = "cdf")
}

%- maybe also 'usage' for other objects documented here.

\arguments{
  \item{x}{Vector of observations.}
  \item{kernel}{Kernel of the pdf of the associated symmetrical distribution 
                by means of which the IGTD is obtained. The kernels: 
                \code{"laplace"}, \code{"logistic"}, \code{"normal"} and 
                \code{"t"} are available.}
  \item{graph}{Logical; if TRUE (default), the cdf plot is provided.}
  \item{mainTitle}{Main title of the graph.}
  \item{xLabel}{A title for the x axis.}
  \item{yLabel}{A title for the y axis.}
}

\details{
  The Kolmogorov-Smirnov test is a goodness-of-fit technique based on the 
  maximum distance between the empirical and theoretical cdfs.
}

\value{
The function \code{ksigt()} carries out de Kolmogorov-Smirnov test for the IGTD.
}

\references{Sanhueza, A., Leiva, V., Balakrishnan, N. (2008). A new class of 
            inverse Gaussian type distributions. Metrika (in press).
}

\author{
Víctor Leiva <victor.leiva@uv.cl; victor.leiva@yahoo.com>, \cr
Hugo Hernández <hugo.hernandez.p@gmail.com> and \cr
Antonio Sanhueza <asanhueza@ufro.cl>.
}

\examples{
## Generates a sample from the IGTD with normal kernel 
x <- rigt(300, mu = 1.0, lambda = 1.0, kernel = "normal")

## Produces a KS test abd produces a graph for the IGTD with normal kernel
  ksigt(x, kernel = "normal", graph = TRUE,
        xLabel = "Data",
        yLabel = "Cumulative distribution function")
}

\keyword{univar}
\keyword{htest}
\keyword{math}

