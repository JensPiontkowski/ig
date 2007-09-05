\name{Loglikig}
\alias{Loglikig}

\title{Loglikelihood function associated to the inverse Gaussian type distribution}

\description{Compute the loglikelihood function associated to the inverse Gaussian type distribution.}

\usage{
Loglikig(x, kernel = "normal", nu.fixed = 2)
}

\arguments{
  \item{x}{Vector of observations.}
  \item{kernel}{Kernel of the pdf of the associated symmetrical distribution by means of which the IGTD is obtained. The kernels: \code{"Laplace"}, \code{"logistic"}, \code{"normal"} and \code{"t"} are available.}
  \item{nu.fixed}{Additional parameter of the IGTD when the \code{t} kernel is used.}
}

\references{
Sanhueza, A., Leiva, V. and Balakrishnan, N. (2007). A new class of inverse Gaussian type distributions. Metrika (in press).
}

\author{
Víctor Leiva <victor.leiva@uv.cl>, Hugo Hernández <hugo.hernandez@msn.com>, and Antonio Sanhueza <asanhue@ufro.cl>.
}

\keyword{math}
\keyword{htest}
