\name{dLaplace}
\alias{dLaplace}

\title{Probability density function (pdf) of the Laplace distribution}

\description{Compute the probabilility density function of Laplace distribution.}

\usage{
dLaplace(x)
}

\arguments{
  \item{x}{Vector of observations.}
}

\details{The Laplace distribution has pdf given by
\deqn{
f_T(t)=(1/2) \exp(-|t|),
}
with \eqn{-\infty < t < + \infty}
}

\value{
 \code{dLaplace()} gives the pdf of an Laplace distribution.
}

\references{Johnson, N., Kotz, S. and Balakrishnan, N. (1994). Continuous univariate distributions, vol 1.}

\author{
Víctor Leiva <victor.leiva@uv.cl>, Hugo Hernández <hugo.hernandez@msn.com>, and Antonio Sanhueza <asanhue@ufro.cl>.
}

\examples{
x<-seq(-3,3,by=0.01)
y<-dLaplace(x)
plot(x,y,type="l",xlab="x",ylab="f(x)")
}

\keyword{distribution}
