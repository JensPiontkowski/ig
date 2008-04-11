\name{loglikigt}

\alias{loglikigt}

\title{Loglikelihood function associated with the IGTD}

\description{
  This function computes the numerical value of the loglikelihood function 
  associated with the IGTD.
}

\usage{
loglikigt(x, nu = 1.0, kernel = "normal")
}

\arguments{
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
Compute the loglikelihood function associated to the inverse Gaussian type distribution.
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

## Computes the valur for the likelihood for a sample x from the IGTD 
## with normal kernel
loglikigt(x, nu = 1.0, kernel = "normal")
}

\keyword{univar}
\keyword{htest}
\keyword{math}
