\name{ll}

\alias{ll}

\title{Analytical first derivative with respect to lambda of the IGTD}

\description{
  This function computes the first analytical derivative of the loglikelihood 
  with respect to lambda of the IGTD.
}

\usage{
ll(theta, x, nu = 1.0, kernel = "normal")
}

\arguments{
  \item{theta}{Vector of parameters \code{mu} and \code{lambda}.}
  \item{x}{Vector of observations.}
  \item{nu}{Shape parameter corresponding to the degrees of freedom of the t 
            distribution. In the case of the Laplace, logistic, normal kernels,
            nu can be fixed at the value 1.0 since this parameter is not 
            involved in these kernels.}
  \item{kernel}{Kernel of the pdf of the associated symmetrical distribution 
                by means of which the IGTD is obtained. The kernels: 
                \code{"laplace"}, \code{"logistic"}, \code{"normal"} and 
                \code{"t"} are available.}
}

\value{
  \code{ll()} return the first analytical derivative of the loglikelihood 
  with respect to lambda of the IGTD.
}

\references{Sanhueza, A., Leiva, V., Balakrishnan, N. (2008). A new class of 
            inverse Gaussian type distributions. Metrika (in press).}

\author{
Víctor Leiva <victor.leiva@uv.cl; victor.leiva@yahoo.com>, \cr
Hugo Hernández <hugo.hernandez.p@gmail.com> and \cr
Antonio Sanhueza <asanhueza@ufro.cl>.
}

\keyword{math}


