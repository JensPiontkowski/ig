\name{pig}
\alias{pig}

\title{Cumulative distribution function (cdf) of the inverse Gaussian type distribution}

\description{
Cumulative distribution function for the IGDT with mean mu, scale parameter lambda and associated kernel \eqn{g}.
}

\usage{
pig(q, mu = 1, lambda = 1, kernel = "normal", parameter.nu = 1, lower.tail = TRUE, log.p = FALSE)
}

\arguments{
  \item{q}{Vector of quantiles.}
  \item{mu}{Mean.}
  \item{lambda}{Scale parameter.}
  \item{kernel}{Kernel of the pdf of the associated symmetrical distribution by means of which the IGTD is obtained. The kernels: \code{"Laplace"}, \code{"logistic"}, \code{"normal"} and \code{"t"} are available.}
  \item{parameter.nu}{Additional parameter of the IGTD when the t kernel is used. }
  \item{lower.tail}{Logical; if TRUE (default), probabilities are P[X <= x],\cr
  otherwise, P[X > x].}
  \item{log.p}{Logical; if TRUE, probabilities p are given as log(p).}
}

\details{
The IGTD has cdf given by
\deqn{
F_T(t) = F_Z(a_t)+\int^{\infty}_{b_t} c \, g(u^2- \frac{4 \, \lambda}{\mu}) \mbox{d}u,
}
where \eqn{b_t=\sqrt{\lambda/\mu} [\sqrt{t/\mu} + \sqrt{\mu/t}]}, \eqn{\mu} is the mean, \eqn{\lambda} the scale parameter, \eqn{g} is the kernel of the pdf of the associated symmetrical distribution, c the normalization constant and \eqn{F_Z(\cdot)} denotes the cdf of the associated symmetrical distribution.
}

\value{
\code{pig()} gives the cdf of an IGTD.
}

\references{
Sanhueza, A., Leiva, V. and Balakrishnan, N. (2007). A new class of inverse Gaussian type distributions. Metrika (in press).
}

\author{
Víctor Leiva <victor.leiva@uv.cl>, Hugo Hernández <hugo.hernandez@msn.com>, and Antonio Sanhueza <asanhue@ufro.cl>.
}

\examples{
## Compute the cdf for a vector q with mu=1, lambda=1 and g="normal"
## At the end we have the graph of the cdf of the IGTD with g="normal".
x <- seq(0, 4,by=0.01)
px <- pig(x,mu=1.0,lambda=1.0,kernel="normal")
print(px)
plot(x, px, main = "cdf of the IGTD (g='normal')", ylab="F(x)")
}

\keyword{distribution}
