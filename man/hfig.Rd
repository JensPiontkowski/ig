\name{hfig}
\alias{hfig}

\title{function (hf) of the inverse Gaussian type distribution}

\description{
Hazard function or failure rate for the IGTD with mean parameter mu, scale parameter lambda and associated kernel \code{g}.
}

\usage{
hfig(t, mu = 1, lambda = 1, kernel = "normal", parameter.nu = 1)
}

\arguments{
  \item{t}{Vector of quantiles.}
  \item{mu}{Mean.}
  \item{lambda}{Scale parameter.}
  \item{kernel}{Kernel of the pdf of the associated symmetrical distribution by means of which the IGTD is obtained. The kernels: "Laplace", "logistic", "normal" and "t" are available.}
  \item{parameter.nu}{Additional parameter of the IGTD when the t kernel is used. This parameter corresponds
  to a shape parameter and it is also known as "degree of freedom". For default \code{nu=1},
  in which case the Cauchy distribution is obtained. The Student-t distribution has always degrees of kurtosis
  greater than normal distribution. This aspect is transferred to the IGTD and produces robust
  parameter estimates for the IGTD.}
}

\details{
The IGTD has hf given by
\deqn{
h_T(t)=\frac{f_Z(a_{t}) \, \frac{\sqrt{\lambda}}{\sqrt{t^{3}}}}{F_Z(-a_t)-\int^{\infty}_{b_t} c
\, g(u^2- \frac{4 \, \lambda}{\mu}) \ \mbox{\footnotesize d}u}.
}
}

\value{
\code{hfig()} gives the hf of an IGTD.
}

\author{
Víctor Leiva <victor.leiva@uv.cl>, Hugo Hernández <hugo.hernandez@msn.com>, and Antonio Sanhueza <asanhue@ufro.cl>.
}

\examples{
## Computes the hf of the IGTD with g="normal" for a vector x with mu=1, lambda=1 and g="normal"
## At the end we have the graph of the IGTD hf 
x <- seq(0, 4,by=0.01)
hx <- hfig(x,mu=1.0,lambda=1.0,kernel="normal")
print(hx)
plot(x, hx, main = "Hazard function of the IGTD (classical case)", ylab="h(x)")
}

\keyword{univar}
\keyword{survival}
