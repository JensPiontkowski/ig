\name{wg}

\alias{wg}

%- Also NEED an '\alias' for EACH other topic documented here.

\title{Weights in the likelihood of the IGTD}

\description{
  This function computes the weights in the likelihood function of the 
  inverse Gaussian type distribution given by: \eqn{w=g'(u)/g(u)}, where 
  \eqn{g} is the kernel of the pdf of the symmetrical distribution.
}

\usage{
wg(u, nu = 1.0, kernel = "normal")
}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{u}{Vector of values.}
  \item{nu}{Shape parameter corresponding to the degrees of freedom of the t 
            distribution. In the case of the Laplace, logistic, normal kernels,
            nu can be fixed at the value 1.0 since this parameter is not 
            involved in these kernels.}
  \item{kernel}{Kernel of the pdf of the associated symmetrical distribution 
                by means of which the IGTD is obtained. The kernels: 
                \code{"laplace"}, \code{"logistic"}, \code{"normal"} and 
                \code{"t"} are available.}
}

\value{
  \code{wg()} return the weights in the likelihood function of the 
  inverse Gaussian type distribution.
}

\references{Sanhueza, A., Leiva, V., Balakrishnan, N. (2008). A new class of 
            inverse Gaussian type distributions. Metrika (in press).}

\author{
Víctor Leiva <victor.leiva@uv.cl; victor.leiva@yahoo.com>, \cr
Hugo Hernández <hugo.hernandez.p@gmail.com> and \cr
Antonio Sanhueza <asanhueza@ufro.cl>.
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.

\keyword{math}
\keyword{htest}
