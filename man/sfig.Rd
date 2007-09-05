\name{sfig}
\alias{sfig}

\title{Survival function (sf) of the inverse Gaussian type distribution}

\description{
Survival function for the IGTD with mean parameter mu, scale parameter lambda and associated kernel \eqn{g}.
}

\usage{
sfig(t, mu = 1, lambda = 1, kernel = "normal", parameter.nu = 1)
}

\arguments{
  \item{t}{Vector of quantiles.}
  \item{mu}{Mean.}
  \item{lambda}{Scale parameter.}
  \item{kernel}{Kernel of the pdf of the associated symmetrical distribution by means of which the IGTD is obtained. The kernels: \code{"Laplace"}, \code{"logistic"}, \code{"normal"} and \code{"t"} are available.}
  \item{parameter.nu}{Additional parameter of the IGTD when the t kernel is used. This parameter corresponds to a shape parameter and it is also known as \code{"degree of freedom"}. For default \code{nu=1}, in which case the Cauchy distribution is obtained. The Student-t distribution has always degrees of kurtosis greater than normal distribution. This aspect is transferred to the IGTD and produces robust parameter estimates for the IGTD.}
}

\details{
The IGTD has sf given by
\deqn{
S_T(t)= F_Z(-a_t)-\int^{\infty}_{b_t} c \, g(u^2- \frac{4 \, \lambda}{\mu}) \ \mbox{d}u; \ t >0.
}
}

\value{
\code{sfig()} gives the sf of an IGTD.
}

\author{
Víctor Leiva <victor.leiva@uv.cl>, Hugo Hernández <hugo.hernandez@msn.com>, and Antonio Sanhueza <asanhue@ufro.cl>.
}

\examples{
## Compute the sf of the IGTD with g="normal" for a vector x with mu=1, lambda=1
## At the end we have the graph of the IGTD sf
x <- seq(0, 4,by=0.01)
sx <- sfig(x,mu=1.0,lambda=1.0,kernel="normal")
print(sx)
plot(x, sx, main = "Survival of the IGTD (classical case)", ylab="S(x)")
}

\keyword{univar}
\keyword{survival}
