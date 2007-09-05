\name{dig.PVII}
\alias{dig.PVII}

\title{Probability density function (pdf) of the inverse Gaussian type distribution generated from the Pearson type VII kernel}

\description{Compute the probabilility density function of inverse Gaussian distribution generated from the the Pearson type VII kernel with parameters \code{q} and \code{r}.}

\usage{
dig.PVII(t, mu, lambda, parameters = c(1, 1), log = FALSE)
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
with \eqn{t > 0}, \eqn{\mu>0} and \eqn{\lambda>0}, where \eqn{f_Z(\cdot)= c\,g(\cdot)} is the pdf of the Pearson VII distribution \eqn{a_{t} = a_{t}(\mu,\lambda) = \sqrt{\lambda/\mu} [\sqrt{t/\mu} - \sqrt{\mu/t}]}.
}

\value{
 \code{dig.PVII()} gives the pdf of an IGTD generated from the Pearson VII kernel.
}

\references{
Sanhueza, A., Leiva, V. and Balakrishnan, N. (2007). A new class of inverse Gaussian type distributions. Metrika (in press).
}

\author{
Víctor Leiva <victor.leiva@uv.cl>, Hugo Hernández <hugo.hernandez@msn.com>, and Antonio Sanhueza <asanhue@ufro.cl>.
}

\examples{
x<-seq(-3,3,by=0.01)
y<-dig.PVII(x,1.0,1.0,c(1,1))
plot(x,y,type="l",xlab="x",ylab="f(x)")
}

\keyword{distribution}
