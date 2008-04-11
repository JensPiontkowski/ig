\name{digpvii}

\alias{digpvii}

\title{Density of the IGTD generated from the Pearson type VII kernel}

\description{
  Probabilility density function of the IGTD generated from 
  the the Pearson type VII kernel with parameters \code{q} and \code{r}.
}

\usage{
digpvii(x, mu = 1.0, lambda = 1.0, parameters = c(1.0, 1.0), log = FALSE)
}

\arguments{
  \item{x}{Vector of observations.}
  \item{mu}{Mean.}
  \item{lambda}{Scale parameter.}
  \item{parameters}{Parameters of the Kotz distribution, \code{c(q, r)}.}
  \item{log}{Logical; if TRUE, probabilities p are given as log(p).}
}

\details{
  The IGTD has pdf given by
  \deqn{
        f_T(t) = f_Z(a_{t}) \sqrt{\lambda}/\sqrt{t^{3}},
       }
  with \eqn{t > 0}, \eqn{\mu>0} and \eqn{\lambda>0}, where \eqn{f_Z(\cdot) = 
  c\,g(\cdot)} is the pdf of the Pearson VII distribution \eqn{a_{t} = 
  a_{t}(\mu,\lambda) = \sqrt{\lambda/\mu} [\sqrt{t/\mu} - \sqrt{\mu/t}]}.
}

\value{
   \code{digpvii()} gives the pdf of an IGTD generated from the Pearson VII kernel.
}

\references{Sanhueza, A., Leiva, V., Balakrishnan, N. (2008). A new class of 
            inverse Gaussian type distributions. Metrika (in press).}

\author{
Víctor Leiva <victor.leiva@uv.cl; victor.leiva@yahoo.com>, \cr
Hugo Hernández <hugo.hernandez.p@gmail.com> and \cr
Antonio Sanhueza <asanhueza@ufro.cl>.
}

\examples{
# Produces a graphical plot for the IGD from the Pearson VII type kernel
# with parameters q = 1 y r = 2
x <- seq(-3, 3, by = 0.01)
y <- digpvii(x, 1.0, 1.0, parameters = c(1, 2))
plot(x, y, type = "l", xlab = "x", ylab = "f(x)")
}

\keyword{distribution}


