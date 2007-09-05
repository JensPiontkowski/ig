\name{wgp}
\alias{wgp}

\title{Derivative of the weights in the likelihood fucntion of the inverse Gaussian type distribution}

\description{Compute the derivative of the weights in the likelihood fucntion of the inverse Gaussian type distribution.}

\usage{
wgp(u, kernel, nu = 1)
}

\arguments{
  \item{u}{Vector of values.}
  \item{kernel}{Kernel of the pdf of the associated symmetrical distribution by means of which the IGTD is obtained.}
  \item{nu}{Additional parameter of the IGTD when the \code{t} kernel is used.}
}

\references{
Sanhueza, A., Leiva, V. and Balakrishnan, N. (2007). A new class of inverse Gaussian type distributions. Metrika (in press).
}

\author{
Víctor Leiva <victor.leiva@uv.cl>, Hugo Hernández <hugo.hernandez@msn.com>, and Antonio Sanhueza <asanhue@ufro.cl>.
}

\keyword{math}
\keyword{htest}
