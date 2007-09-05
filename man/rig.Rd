\name{rig}
\alias{rig}

\title{Random number generator from the IGTD}

\description{
Generate random numbers from the IGTD with mean mu, scale parameter lambda and associated kernel \eqn{g}.
}

\usage{
rig(n, mu = 1, lambda = 1, kernel = "normal", parameter.nu = 1)
}

\arguments{
  \item{n}{Number of observations.}
  \item{mu}{Mean.}
  \item{lambda}{Scale parameter.}
  \item{kernel}{Kernel of the pdf of the associated symmetrical distribution by means of which the IGTD is obtained.
The kernels: \code{"Laplace"}, \code{"logistic"}, \code{"normal"} and \code{"t"} are available.}
  \item{parameter.nu}{Additional parameter of the IGTD when the t kernel is used.}
}

\details{
Statistical inference tools may not exist in closed form for the
IGTD, which is not the case for the classical IGD.
Hence, simulation and
numerical studies are needed, which require a random number (r.n) generator.
Next, we present a r.n.
generator for the IGTD following a similar procedure to
the one given in Chhikara and Folks (1989, pp. 52-53) for the classical
inverse Gaussian distribution.
}

\value{
\code{rig()} gives a vector of n random numbers from the IGTD for mu, lambda and kernel specified.
}

\references{
Chhikara, R. S. and Folks, J. L. (1989). The Inverse Gaussian Distribution. Marcel Dekker, New York.
}

\author{
Víctor Leiva <victor.leiva@uv.cl>, Hugo Hernández <hugo.hernandez@msn.com>, and Antonio Sanhueza <asanhue@ufro.cl>.
}

\examples{
## Generate a sample x from the IGTD. At the end we have the histogram of x
x <- rig(1000,mu=1.0,lambda=1.0,kernel="normal")
hist(x, main="Histogram of a sample from IGTD")
}

\keyword{distribution}
