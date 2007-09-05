\name{dig.kotz}
\alias{dig.kotz}

\title{Probability density function (pdf) of the inverse Gaussian type distribution generated from the Kotz kernel}

\description{Compute the probabilility density function of inverse Gaussian distribution generated from the Kotz distribution with parameters \code{q}, \code{r} and \code{s}.}

\usage{
dig.kotz(t, mu, lambda, parameters = c(1, 1, 1), log = FALSE)
}

\arguments{
  \item{t}{Vector of observations.}
  \item{mu}{Mean.}
  \item{lambda}{Scale parameter.}
  \item{parameters}{Parameters of the Kotz distribution.}
  \item{log}{Logical; if TRUE, probabilities p are given as log(p).}
}

\details{
The IGTD has pdf given by
\deqn{
f_T(t)=f_Z(a_{t}) \sqrt{\lambda}/\sqrt{t^{3}},
}
with \eqn{t > 0}, \eqn{\mu>0} and \eqn{\lambda>0}, where \eqn{f_Z(\cdot)= c\,g(\cdot)} is the pdf of the Kotz distribution \eqn{a_{t} = a_{t}(\mu,\lambda) = \sqrt{\lambda/\mu} [\sqrt{t/\mu} - \sqrt{\mu/t}]}.
}

\value{
 \code{dig.kotz()} gives the pdf of an IGTD generated from the Kotz kernel.
}

\references{
Sanhueza, A., Leiva, V. and Balakrishnan, N. (2007). A new class of inverse Gaussian type distributions. Metrika (in press).
}

\author{
Víctor Leiva <victor.leiva@uv.cl>, Hugo Hernández <hugo.hernandez@msn.com>, and Antonio Sanhueza <asanhue@ufro.cl>.
}

\examples{
x<-seq(-3,3,by=0.01)
y<-dig.kotz(x,1.0,1.0,c(1,2,3))
plot(x,y,type="l",xlab="x",ylab="f(x)")
}

\keyword{distribution}
