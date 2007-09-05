\name{diagnosticsig}
\alias{diagnosticsig}

\title{Diagnostic plots for the inverse Gaussian type distribution}

\description{
The function \code{diagnosticsig()} produces an index plot of the total local influence.
}

\usage{
diagnosticsig(data, kernel = "normal", main = "", ylim = NULL)
}

\arguments{
  \item{data}{Vector of observations.}
  \item{kernel}{Kernel of the pdf of the associated symmetrical distribution by means of which the IGTD is obtained.
  The kernels: \code{"Laplace"}, \code{"logistic"}, \code{"normal"} and \code{"t"} are available.
  When the \code{t} kernel is considered, the value for the parameter nu must be incorporated.}
  \item{main}{An overall title for the plot.}
  \item{ylim}{Limit for the y axis.}
}

\details{
The local influence diagnostics method (Cook, 1986) is used to evaluate the local influence by means of likelihood displacement.
}

\value{
\code{diagnosticsig()} gives an influence diagnostics through a graphical plot for the IGTD from a sample of observations.
}

\references{
Cook, R. D. and Weisberg, S. (1982). Residuals and Influence in Regression. Chapman and Hall, London.

Cook, R. D. (1986). Assessment of local influence (with discussion).  Journal of the Royal Statistical Society Series B 48, 133-169.

Sanhueza, A., Leiva, V. and Balakrishnan, N. (2007). A new class of inverse Gaussian type distributions. Metrika (in press).
}

\author{
Víctor Leiva <victor.leiva@uv.cl>, Hugo Hernández <hugo.hernandez@msn.com>, and Antonio Sanhueza <asanhue@ufro.cl>.
}

\examples{
## Generates a sample from the IGTD
## Produces influence diagnostics for the IGTD with g="normal"
x<-rig(300,mu=1,lambda=1,kernel="normal")
diagnosticsig(x, kernel="normal")
}

\keyword{univar}
