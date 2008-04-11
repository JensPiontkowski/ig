\name{sfigt}

\alias{sfigt}

\title{Survival function of the IGTD}

\description{
  Survival function (sf) for the IGTD with mean parameter mu, scale parameter 
  lambda and associated kernel \eqn{g}.
}

\usage{
sfigt(x, mu = 1.0, lambda = 1.0, nu = 1.0, kernel = "normal")
}

\arguments{
  \item{x}{Vector of observations.}
  \item{mu}{Mean.}
  \item{lambda}{Scale parameter.}
  \item{nu}{Shape parameter corresponding to the degrees of freedom of the t 
            distribution. In the case of the Laplace, logistic, normal kernels,
            nu can be fixed at the value 1.0 since this parameter is not 
            involved in these kernels.}
  \item{kernel}{Kernel of the pdf of the associated symmetrical distribution 
                by means of which the IGTD is obtained. The kernels: 
                \code{"laplace"}, \code{"logistic"}, \code{"normal"} and 
                \code{"t"} are available.}
}

\details{
  The IGTD has sf given by
  \deqn{
        S_T(t) = F_Z(-a_t)-\int^{\infty}_{b_t} c \, g(u^2- \frac{4 \, \lambda}{\mu}) 
        \ \mbox{d}u; \ t >0.
        }
}

\value{\code{sfig()} gives the sf of an IGTD.}

\references{Sanhueza, A., Leiva, V., Balakrishnan, N. (2008). A new class of 
            inverse Gaussian type distributions. Metrika (in press).
}

\author{
Víctor Leiva <victor.leiva@uv.cl; victor.leiva@yahoo.com>, \cr
Hugo Hernández <hugo.hernandez.p@gmail.com> and \cr
Antonio Sanhueza <asanhueza@ufro.cl>.
}

\examples{
## Computes the sf of the IGTD with normal kernel for a vector x with mu = 1, 
## lambda = 1
x  <- seq(0.01, 4, by = 0.01)
sx <- sfigt(x, mu = 1.0, lambda = 1.0, nu = 1.0, kernel = "normal")
print(sx)

## At the end there is the graph of this pdf
plot(x, sx, main = "sf of the IGTD (classical case)", ylab = "s(x)")
}

\keyword{univar}
\keyword{survival}
