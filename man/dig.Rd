\name{dig}
\alias{dig}

\title{Probability density function (pdf) of the inverse Gaussian type distribution}

\description{
Probability density function for the inverse Gaussian type distribution with mean parameter mu,
scale parameter lambda and associated kernel \eqn{g}. The IGTD is a generalization of the inverse Gaussian
type distribution; for details see Sanhueza, Leiva and Balakrishnan (2007).
The \eqn{g} function corresponds to the kernel of the pdf of the associated symmetrical distribution.
In the ig package, the IGTD can be obtained from the following kernels: Laplace, logistic, normal
(classical case) and Student-t. All these kernels are implemented in the \code{R} software.
The Laplace or double exponential distribution is obtained from the \pkg{normalp} package developed by Mineo (2005).
}

\usage{
dig(x, mu = 1, lambda = 1, kernel = "normal", parameter.nu = 1, log = FALSE)
}

\arguments{
  \item{x}{Vector of observations.}
  \item{mu}{Mean.}
  \item{lambda}{Scale parameter.}
  \item{kernel}{Kernel of the pdf of the associated symmetrical distribution by means of which the IGTD is obtained. The kernels: \code{"Laplace"}, \code{"logistic"}, \code{"normal"} and \code{"t"} are available.}
  \item{parameter.nu}{Additional parameter of the IGTD when the \code{t} kernel is used. This parameter corresponds to a shape parameter known as \code{"degree of freedom"}. For default \code{nu=1}, in which case the Cauchy distribution is obtained. The Student-t distribution has always degrees of kurtosis greater than normal distribution. This aspect is transferred to the IGTD and produces robust parameter estimates for the IGTD.}
  \item{log}{Logical; if TRUE, probabilities p are given as log(p).}
}

\details{
If mu, lambda or \eqn{g} are not specified, then they assume the default values 0, 1 and \code{"normal"}, respectively. The IGTD has pdf given by
\deqn{
f_T(t)=f_Z(a_{t}) \sqrt{\lambda}/\sqrt{t^{3}},
}
with \eqn{t > 0}, \eqn{\mu>0} and \eqn{\lambda>0}, where \eqn{f_Z(\cdot)= c\,g(\cdot)} is the pdf of the associated symmetrical about zero distribution and \eqn{a_{t} = a_{t}(\mu,\lambda) = \sqrt{\lambda/\mu} [\sqrt{t/\mu} - \sqrt{\mu/t}]}.
}

\value{
\code{dig()} gives the pdf of an IGTD.
}

\references{
Mineo, A. (2003). A new package for the general error distribution. R News 3, 13-16.

Sanhueza, A., Leiva, V. and Balakrishnan, N. (2007). A new class of inverse Gaussian type distributions. Metrika (in press).
}

\author{
Víctor Leiva <victor.leiva@uv.cl>, Hugo Hernández <hugo.hernandez@msn.com>, and Antonio Sanhueza <asanhue@ufro.cl>.
}

\examples{
## Compute the pdf of the IGTD with g="normal" for a vector x with mu=1, lambda=1
## At the end we have the graph of this pdf
x <- seq(0, 4,by=0.01)
fx <- dig(x,mu=1.0,lambda=1.0,kernel="normal")
print(fx)
plot(x, fx, main = "pdf of the IGTD (classical case)", ylab="f(x)")
}

\keyword{distribution}
