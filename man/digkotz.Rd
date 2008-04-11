\name{digkotz}

\alias{digkotz}

\title{Pdf of the IGTD generated from the Kotz kernel}

\description{
  This function computes the probabilility density function of inverse Gaussian 
  distribution generated from the Kotz distribution with parameters \code{q}, 
  \code{r} and \code{s}.
}

\usage{
  digkotz(x, mu = 1.0, lambda = 1.0, parameters = c(1.0, 1.0, 1.0), 
          log = FALSE)
}

\arguments{
  \item{x}{Vector of observations.}
  \item{mu}{Mean.}
  \item{lambda}{Scale parameter.}
  \item{parameters}{Parameters of the Kotz distribution, \code{c(q, r, s)}.}
  \item{log}{Logical; if TRUE, probabilities p are given as log(p).}
}
\details{
  The IGTD has pdf given by
  \deqn{
        f_T(t)=f_Z(a_{t}) \sqrt{\lambda}/\sqrt{t^{3}},
       }
  with \eqn{t > 0}, \eqn{\mu>0} and \eqn{\lambda>0}, where \eqn{f_Z(\cdot)= 
  c\,g(\cdot)} is the pdf of the Kotz distribution  \eqn{a_{t} = 
  a_{t}(\mu,\lambda) = \sqrt{\lambda/\mu} [\sqrt{t/\mu} - \sqrt{\mu/t}]}.
}

\value{
 \code{digkotz()} gives the pdf of an IGTD generated from the Kotz kernel.
}

\references{Sanhueza, A., Leiva, V., Balakrishnan, N. (2008). A new class of 
            inverse Gaussian type distributions. Metrika (in press).}

\author{
Víctor Leiva <victor.leiva@uv.cl; victor.leiva@yahoo.com>, \cr
Hugo Hernández <hugo.hernandez.p@gmail.com> and \cr
Antonio Sanhueza <asanhueza@ufro.cl>.
}

\examples{
# Produces a graphical plot for the IGD from the Kotz kernel
# with parameters q = 1, r = 2 and s = 3 
x <- seq(-3, 3, by = 0.01)
y <- digkotz(x, 1.0, 1.0, parameters = c(1, 2, 3))
plot(x, y, type = "l", xlab = "x", ylab = "f(x)")
}

\keyword{distribution}


