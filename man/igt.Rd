\name{igt}

\alias{digt}
\alias{pigt}
\alias{qigt}
\alias{rigt}

\title{The Inverse Gaussian Type Distribution (IGTD)}

\description{
Density, distribution function, quantile function and random generation for the 
inverse gaussian type distribution with mean parameter \eqn{mu}, scale parameter 
\eqn{lambda} and associated kernel.
}

\usage{
digt(x, mu = 1.0, lambda = 1.0, nu = 1.0, kernel = "normal", 
     log = FALSE)

pigt(q, mu = 1.0, lambda = 1.0, nu = 1.0, kernel = "normal", 
     lower.tail = TRUE, log.p = FALSE)
     
qigt(p, mu = 1.0, lambda = 1.0, nu = 1.0, kernel = "normal", 
     lower.tail = TRUE, log.p = FALSE)
     
rigt(n, mu = 1.0, lambda = 1.0, nu = 1.0, kernel = "normal")     
}

\arguments{
  \item{x, q}{Vector of observations (quantiles).}
  \item{p}{Vector of probabilities.}
  \item{mu}{Mean.}
  \item{lambda}{Scale parameter.}
  \item{nu}{Shape parameter corresponding to the degrees of freedom of the t 
            distribution. In the case of the Laplace, logistic, normal kernels,
            nu can be fixed at the value 1.0 since this parameter is not 
            involved in these kernels.}
  \item{n}{Number of observations.}
  \item{kernel}{Kernel of the pdf of the associated symmetrical distribution 
                by means of which the IGTD is obtained. The kernels: 
                \code{"laplace"}, \code{"logistic"}, \code{"normal"} and 
                \code{"t"} are available.}
  \item{log, log.p}{Logical; if TRUE, probabilities p are given as log(p).}
  \item{lower.tail}{Logical; if TRUE (default), probabilities are P[X <= x],\cr
  otherwise, P[X > x].}
}

\details{
Probability density function (pdf) for the IGTD with mean parameter mu, scale 
parameter lambda and associated kernel \eqn{g}. The IGTD is a 
generalization of the IGTD; for details see Sanhueza, Leiva and Balakrishnan
(2008). The \eqn{g} argument corresponds to the kernel of the pdf of the 
associated symmetrical distribution. In the ig package, the IGTD can be 
obtained from the following kernels: Laplace, logistic, normal (classical 
case) and Student-t. All these kernels are implemented in the \code{R} 
software. The Laplace or double exponential distribution can be obtained 
from the \pkg{normalp} package developed by Mineo (2005).

If mu, lambda or \eqn{g} are not specified, then they assume the default 
values 1.0, 1.0 and \code{"normal"}, respectively. 

The IGTD has pdf given by
\deqn{
f_T(t)=f_Z(a_{t}) \sqrt{\lambda}/\sqrt{t^{3}},
}
with \eqn{t > 0}, \eqn{\mu>0} and \eqn{\lambda>0}, where \eqn{f_Z(\cdot) = 
c\,g(\cdot)} is the pdf of the associated symmetrical about zero distribution
and \eqn{a_{t} = a_{t}(\mu,\lambda) = \sqrt{\lambda/\mu} [\sqrt{t/\mu} - 
\sqrt{\mu/t}]}.

It is not possible to find the quantile function of the IGTD in a closed analytical 
form, so these values must be obtained by numerical methods.

Statistical inference tools may not exist in closed form for the IGTD, which is 
not the case for the classical IGTD. Hence, simulation and numerical studies are 
needed, which require a random number (r.n) generator. Next, we present a r.n.
generator for the IGTD following a similar procedure to the one given in Chhikara 
and Folks (1989, pp. 52-53) for the classical inverse Gaussian distribution. This generator has been
implemented in the \pkg{ig} package. Thus, if \eqn{T \sim {\rm IGT}(\mu,
\lambda; g)}, the algorithm steps are:
\itemize{
\item Generate a r.n. \eqn{u} from \eqn{U \sim G\chi^2(g)} by using an appropriate generator.
\item Generate a r.n. \eqn{w} from \eqn{W \sim U(0,1)}.
\item Set values for \eqn{\mu} and \eqn{\lambda} and then compute a random number \eqn{t = t_1} or \eqn{t = t_2} from \eqn{T \sim IGT(\mu, \lambda; g)} according to the following criterion:
}

\enumerate{
\item If \eqn{w \leq p_0}, then \eqn{t_1=\mu+\frac{\mu^2\,u}{2\lambda}-\frac{\mu}{2\lambda}\sqrt{4\mu\lambda\,u+\mu^2\,u^2}}, where \eqn{p_0=\frac{\mu}{\mu+t_1}};
\item If \eqn{w > p_0}, then \eqn{t_{2}= \frac{\mu^2}{t_1}}. 
}
}
             
\value{
\code{digt()} gives the density, \code{pigt()} gives the distribution function, 
\code{qigt()} gives the quantile function, and \code{rigt()} generates random deviates.
}

\references{
Sanhueza, A., Leiva, V., Balakrishnan, N. (2008). A new class of 
inverse Gaussian type distributions. Metrika (in press).

Chhikara, R. S. and Folks, J. L. (1989). The Inverse Gaussian Distribution. 
Marcel Dekker, New York.}

\author{
Víctor Leiva <victor.leiva@uv.cl; victor.leiva@yahoo.com>, \cr
Hugo Hernández <hugo.hernandez.p@gmail.com> and \cr
Antonio Sanhueza <asanhueza@ufro.cl>.
}

\examples{
## Computes the pdf of the IGTD with g = "normal" for a vector x with mu = 1.0, 
## lambda = 1.0
x  <- seq(0.01, 4, by = 0.01)
fx <- digt(x, mu = 1.0, lambda = 1.0, nu = 1.0, kernel = "normal")
print(fx)

## At the end there is a graph of this pdf
plot(x, fx, main = "pdf of the IGTD (classical case)", ylab = "f(x)")

## Computes the cdf of the IGTD with g = "normal" for a vector x with mu = 1.0, 
## lambda = 1.0
Fx <- pigt(x, mu = 1.0, lambda = 1.0, nu = 1.0, kernel = "normal")
print(Fx)

## At the end there is a graph of this cdf
plot(x, Fx, main = "cdf of the IGTD (classical case)", ylab = "F(x)")

## Compute the 50 percentile (median) for a vector of probabilities x
## of the IGT with mu = 1.0, lambda = 1.0 and kernel = "normal"
q <- qigt(0.5, mu = 1.0, lambda = 1.0, kernel = "normal")
q

## Generates a sample x from the IGTD with normal kernel. 
## At the end we have the histogram of x
x <- rigt(1000, mu = 1.0, lambda = 1.0, nu = 1.0, kernel = "normal")
hist(x, main = "Histogram of a sample from IGTD")
}

\keyword{distribution}


