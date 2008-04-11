\name{sicigt}

\alias{sicigt}

\title{Schwartz information criterion for a sample from the IGTD}

\description{
  The function \code{sicig()} gives the Schwartz information criterion (SIC) 
  value assuming an IGTD with parameters mu, lambda and a specific kernel.
}

\usage{
sicigt(x, nu = 1.0, kernel = "normal")
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

\details{
  The SIC is a selection model criterion based on information loss. According 
  to this criterion, it is possible to choice a hypothetic model that better 
  describe the data set considering the smaller SIC value. The SIC is defined as 
  \eqn{SIC = -l(\theta)/n+ p \log(n)/(2n)}, where \eqn{l(\theta)} is the 
  log-likelihood function associated with the model, \eqn{n} is the sample size 
  and \eqn{p} is the number of involved parameters; for more details
  see Spieglhaiter, Best, Carlin and van der Linde (2002).
}

\value{
 \code{sicigt()} gives the value for the SIC of the IGTD.
}

\references{
  Spieglhaiter, D. J., Best, N. G., Carlin, B. P., van der Linde, A. (2002).
  Bayesian measures of complexity and fit. Journal of the Royal Statistical 
  Society Series B 64, 1-34.
  
  Sanhueza, A., Leiva, V., Balakrishnan, N. (2008). A new class of 
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

## Computes the SIC value of the IGTD with normal kernel from the data x
sicigt(x, 1.0, kernel = "normal")
}

\keyword{univar}
\keyword{htest}
\keyword{math}


