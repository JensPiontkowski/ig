\name{dlaplace}

\alias{dlaplace}

\title{Density of the Laplace distribution}

\description{
  Probabilility density function of the Laplace distribution.
}

\usage{
dlaplace(x)
}

\arguments{
  \item{x}{Vector of observations.}
}

\details{The Laplace distribution has pdf given by
  \deqn{
        f_T(t)=(1/2) \exp(-|t|),
       }
  with \eqn{-\infty < t < + \infty}.
  }

\value{
 \code{dLaplace()} gives the pdf of an Laplace distribution.
}

\references{Mineo, A. (2003). A new package for the general error distribution. 
            R News 3, 13-16.
            
            Sanhueza, A., Leiva, V., Balakrishnan, N. (2008). A new class of 
            inverse Gaussian type distributions. Metrika (in press).
}

\author{
Víctor Leiva <victor.leiva@uv.cl; victor.leiva@yahoo.com>, \cr
Hugo Hernández <hugo.hernandez.p@gmail.com> and \cr
Antonio Sanhueza <asanhueza@ufro.cl>.
}

\examples{
# Produces a graphical plot for the Laplace distribution
x <- seq(-3, 3, by = 0.01)
y <- dlaplace(x)
plot(x, y, type = "l", xlab = "x", ylab = "f(x)")
}

\keyword{distribution}


