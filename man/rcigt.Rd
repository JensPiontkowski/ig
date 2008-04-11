\name{rcigt}

\alias{rcigt}

\title{Relative change on the MLE of the IGTD}

\description{
  The function \code{rcigt()} computes the relative change (RC) on the MLE 
  when some observations are removed in order to evaluate the effect of their 
  potentially influence.
}

\usage{
rcigt(x, casesRemoved = NULL, kernel = "normal")
}

\arguments{
  \item{x}{Vector of observations.}
  \item{casesRemoved}{Index of the potentially influential case(s) that must 
                      be removed.}
  \item{kernel}{Kernel of the pdf of the associated symmetrical distribution 
                by means of which the IGTD is obtained. The kernels: 
                \code{"laplace"}, \code{"logistic"}, \code{"normal"} and 
                \code{"t"} are available.}
}

\details{
  This function computes the relative changes (RC), in percentage, of each 
  estimated parameter, defined by \eqn{\mbox{RC}_{\theta_j} = |(\hat \theta_j -
  \hat \theta_{j(\mbox{\tiny I})})/\hat \theta_j| \times 100\%}, where 
  \eqn{\hat \theta_{j(\mbox{\tiny I})}} denotes the MLE of \eqn{\theta_j} after 
  the set I of cases has been removed.
}

\value{
  \code{rcigt()} gives the RCs on the MLEs of the parameters of the IGTD from 
  a sample of observations without to consider some potentially influential case(s) 
  related to the MLE of the complete sample.
}

\references{Sanhueza, A., Leiva, V., Balakrishnan, N. (2008). A new class of 
            inverse Gaussian type distributions. Metrika (in press).}

\author{
Víctor Leiva <victor.leiva@uv.cl; victor.leiva@yahoo.com>, \cr
Hugo Hernández <hugo.hernandez.p@gmail.com> and \cr
Antonio Sanhueza <asanhueza@ufro.cl>.
}

\examples{
## Generates a sample from the IGTD with normal kernel
x <- rigt(300, mu = 1.0, lambda = 1.0, kernel = "normal")

## Computes the RC the MLE of the parameters of the IGTD with g="normal" when the
##  case 1 has been removed
rcigt(x, casesRemoved = c(1), kernel = "normal")
}

\keyword{univar}
\keyword{htest}
\keyword{math}
