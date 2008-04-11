\name{qqigt}

\alias{qqigt}

\title{Quantile versus quantile plot for the the IGTD}

\description{
  The function \code{qqigt()} produces a quantile-quantile (QQ) plot for the IGTD
  based on the MLE of their parameters. Also, a line going through the first and 
  the third quartile can be sketched. In addition, the coefficient of determination 
  of least squares for the fit line is given.
}

\usage{
qqigt(x, kernel = "normal", line = FALSE, xLabel = "Empirical quantiles", 
      yLabel = "Theoretical quantiles")
}

\arguments{
  \item{x}{Vector of observations.}
  \item{kernel}{Kernel of the pdf of the associated symmetrical distribution 
                by means of which the IGTD is obtained. The kernels: 
                \code{"laplace"}, \code{"logistic"}, \code{"normal"} and 
                \code{"t"} are available.}
  \item{line}{Logical; if TRUE (default), a line going by the first and third 
              quartile is sketched.}
  \item{xLabel}{A title for the x axis.}
  \item{yLabel}{A title for the x axis.}
}

\details{
  The function \code{qqigt()} carries out a QQ plot for the IGTD.
}

\value{
 The function \code{qqigt()} carries out an graphical plot
 useful as goodness-of-fit tool.
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

## Produces a QQ plot for the IGTD with normal kernel
qqigt(x, kernel = "normal", line = TRUE)
}

\keyword{univar}
\keyword{htest}


