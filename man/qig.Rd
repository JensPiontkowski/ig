\name{qig}
\alias{qig}

\title{Quantile function (qf) of the inverse Gaussian type distribution}

\description{
Quantile function for the IGDT with mean mu, scale parameter lambda and associated kernel \eqn{g}.
}

\usage{
qig(p, mu = 1, lambda = 1, kernel = "normal", parameter.nu = 1, lower.tail = TRUE, log.p = FALSE)
}

\arguments{
  \item{p}{Vector of quantiles.}
  \item{mu}{Mean.}
  \item{lambda}{Scale parameter.}
  \item{kernel}{Kernel of the pdf of the associated symmetrical distribution by means of which the IGTD is obtained. The kernels: \code{"Laplace"}, \code{"logistic"}, \code{"normal"} and \code{"t"} are available.}
  \item{parameter.nu}{Additional parameter of the IGTD when the t kernel is used.}
  \item{lower.tail}{Logical; if TRUE (default), probabilities are P[X <= x],\cr
  otherwise, P[X > x].}
  \item{log.p}{Logical; if TRUE, probabilities pr are given as log(pr).}
}

\details{
Unfortunately, it is not possible to find the qf of the IGTD in a closed analytical form, so these values must be obtained by numerical methods.
}

\value{
\code{qig()} gives the qf of an IGTD.
}

\references{
Sanhueza, A., Leiva, V. and Balakrishnan, N. (2007). A new class of inverse Gaussian type distributions. Metrika (in press).
}

\author{
Víctor Leiva <victor.leiva@uv.cl>, Hugo Hernández <hugo.hernandez@msn.com>, and Antonio Sanhueza <asanhue@ufro.cl>.
}

\examples{
## Compute the 50% percentile (median) for a vector of probabilities x
## of the IGT with mu=1, lambda=1 and kernel="normal"
x <- 0.5
q <- qig(0.5,mu=1.0,lambda=1.0,kernel="normal")
q
}

\keyword{distribution}
