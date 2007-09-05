\name{lll}
\alias{lll}

\title{Analitical second derivative with respect to lambda}

\description{Compute the second analitical derivative of the loglikelihood with respect to lambda.}

\usage{
lll(theta, x, kernel = "normal", nu = 1)
}

\arguments{
  \item{theta}{Vector of parameters \code{mu} and \code{lambda}.}
  \item{x}{Vector of observations.}
  \item{kernel}{Kernel of the pdf of the associated symmetrical distribution by means of which the IGTD is obtained.}
  \item{nu}{Additional parameter of the IGTD when the t kernel is used.}
}

\references{
Sanhueza, A., Leiva, V. and Balakrishnan, N. (2007). A new class of inverse Gaussian type distributions. Metrika (in press).
}

\author{
Víctor Leiva <victor.leiva@uv.cl>, Hugo Hernández <hugo.hernandez@msn.com>, and Antonio Sanhueza <asanhue@ufro.cl>.
}

\keyword{math}
