\name{sicig}
\alias{sicig}

\title{Schwartz information criterium (SIC) for a sample of the IGTD}

\description{
The function \code{sicig()} gives the SIC value assuming an IGTD with parameters mu, lambda and a specific kernel.
}

\usage{
sicig(x, kernel = "normal", nu.fixed = 2)
}

\arguments{
  \item{x}{Vector of observations.}
  \item{kernel}{Kernel of the pdf of the associated symmetrical distribution by means of which the IGTD is obtained.
  The kernels: \code{"Laplace"}, \code{"logistic"}, \code{"normal"} and \code{"t"} are available.}
  \item{nu.fixed}{Additional parameter of the IGTD when the t kernel is used. This parameter corresponds
  to a shape parameter and it is also known as \code{"degree of freedom"}. For default \code{nu=1},
  in which case the Cauchy distribution is obtained. The Student-t distribution has always
  degrees of kurtosis greater than normal distribution. This aspect is transferred to the IGTD and
  produces robust parameter estimates for the IGTD.}
}

\details{
The SIC is a selection model criterion based on information loss. According to this criterion, it is possible
to choice a hypothetic model that better describe the data set considering the smaller SIC value.
The SIC is defined as \eqn{SIC = -l(\theta)/n+ p \log(n)/(2n)}, where \eqn{l(\theta)} is the log-likelihood function associated
with the model, n is the sample size, and p is the number of involved parameters; for more details
see Spieglhaiter, Best, Carlin and van der Linde (2002).}

\references{
Spieglhaiter, D. J., Best, N. G., Carlin, B. P., van der Linde, A. (2002). Bayesian measures of complexity and fit. Journal of the Royal Statistical Society Series B 64, 1-34.
}

\author{
Víctor Leiva <victor.leiva@uv.cl>, Hugo Hernández <hugo.hernandez@msn.com>, and Antonio Sanhueza <asanhue@ufro.cl>.
}

\keyword{univar}
