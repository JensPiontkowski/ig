\name{mleigt}

\alias{mleigt}

\title{MLE of the parameters of the IGTD}

\description{
  The function \code{mleigt} gives the maximum likelihood estimates (MLE) of the 
  parameters mu and lambda of the IGTD generated from the kernels: Laplace, logistic and
  normal (classical case) based on a sample of observations based on this distribution.
}

\usage{
mleigt(x, kernel = "normal")
}

\arguments{
  \item{x}{Vector of observations.}
  \item{kernel}{Kernel of the pdf of the associated symmetrical distribution 
                by means of which the IGTD is obtained. The kernels: 
                \code{"laplace"}, \code{"logistic"} and \code{"normal"} are 
                available.}
}

\details{
  The MLEs of the parameters mu and lambda of the classical IG distribution
  are obtained using the analytical expressions of these estimators. In the case
  of the IGTD generated from the kernels: Laplace, logistic and normal, the 
  MLEs of the parameters mu and lambda must be obtained using numerical procedure 
  already implmented in \code{R}.
}

\value{
 \code{mleigt()} computes MLEs for the parameters of the IGTD 
  generated from the kernels: Laplace, logistic and normal giving results 
  according to the following list:
  \item{muEstimate}{Returns the value of the MLE of mu.}
  \item{lambdaEstimate}{Returns the value of the MLE of lambda.}
  \item{loglikelihood}{Returns the value of the IGTD loglikelihood.}
}

\references{Sanhueza, A., Leiva, V., Balakrishnan, N. (2008). A new class of 
            inverse Gaussian type distributions. Metrika (in press).}

\author{
Víctor Leiva <victor.leiva@uv.cl; victor.leiva@yahoo.com>, \cr
Hugo Hernández <hugo.hernandez.p@gmail.com> and \cr
Antonio Sanhueza <asanhueza@ufro.cl>.
}

\examples{
## Generates a sample from the IGTD with t kernel
x <- rigt(300, mu = 1.0, lambda = 1.0, nu = 5, kernel = "normal")

## Computes the likelihood for a sample x from the IGTD with normal kernel
mleigt(x)
}

\keyword{univar}
\keyword{htest}
