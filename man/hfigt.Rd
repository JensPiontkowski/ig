\name{hfigt}

\alias{hfigt}

\title{Hazard function of the IGTD}

\description{
  Hazard function (hf) or failure rate for the IGTD with mean parameter mu, 
  scale parameter lambda and associated kernel \code{g}.
}

\usage{
hfigt(x, mu = 1.0, lambda = 1.0, nu = 1.0, kernel = "normal")
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

\details{The IGTD has hf given by
  \deqn{
        h_T(t) = \frac{f_Z(a_{t}) \, \frac{\sqrt{\lambda}}{\sqrt{t^{3}}}}
        {F_Z(-a_t) - \int^{\infty}_{b_t} c \, g(u^2 - \frac{4 \, \lambda}{\mu})
        \mbox{\footnotesize d}u}.
       }
}

\value{
  \code{hfig()} gives the hf of an IGTD.
}

\references{Sanhueza, A., Leiva, V., Balakrishnan, N. (2008). A new class of 
            inverse Gaussian type distributions. Metrika (in press).
}

\author{
Víctor Leiva <victor.leiva@uv.cl; victor.leiva@yahoo.com>, \cr
Hugo Hernández <hugo.hernandez.p@gmail.com> and \cr
Antonio Sanhueza <asanhueza@ufro.cl>.
}

\examples{
## Computes the hf of the IGTD with normal kernel for a vector x with mu = 1.0, 
## lambda = 1.0
x  <- seq(0.01, 4, by = 0.01)
hx <- hfigt(x, mu = 1.0, lambda = 1.0, nu = 1.0, kernel = "normal")
print(hx)

## At the end there is the graph of this pdf
plot(x, hx, main = "hf of the IGTD (classical case)", ylab = "h(x)")
}

\keyword{univar}
\keyword{survival}

