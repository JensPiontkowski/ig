\name{mleig}
\alias{mleig}

\title{Maximum likelihood estimation (MLE) of the inverse Gaussian type distribution}

\description{
The function mleig estimates the parameters mu and lambda of the IGTD from a sample of observations.
Also, the parameter nu is estimated when the t kernel is considered. In addition, the value for the
log-likelihood function from a sample of the IGTD is given.}

\usage{
mleig(x, kernel = "normal")
}

\arguments{
  \item{x}{Vector of observations.}
  \item{kernel}{Kernel of the pdf of the associated symmetrical distribution by means of which the IGTD is obtained. The kernels: \code{"Laplace"}, \code{"logistic"}, \code{"normal"} and \code{"t"} are available.}
}

\details{
The used algorithm is based on a method proposed by Sanhueza, Leiva and Balakrishnan (2007),
which uses a type-EM algorithm. The iterative procedure considers the estimates for mu and lambda of the classical IG case as
initial values, which has a closed analytical form; see Chhikara and Folks (1989).
The implemented method is a robust estimation procedure because for the classical IG
case of the IGD, this gives equal weights for each case, independently of whether the value
is in the tails or in the center of the distribution. However, for the IGT-t case,
the method gives smaller weight to the extreme cases.
}

\value{
\code{mleig()} estimate the parameters of the IGTD from a sample of observations.
}

\references{
Chhikara, R. S., Folks, J. L. (1989). The Inverse Gaussian Distribution. Marcel Dekker, New York.

Sanhueza, A., Leiva, V., Balakrishnan, N. (2007). A new class of inverse Gaussian type distributions. Metrika (in press).
}

\author{
Víctor Leiva <victor.leiva@uv.cl>, Hugo Hernández <hugo.hernandez@msn.com>, and Antonio Sanhueza <asanhue@ufro.cl>.
}

\examples{
## Generates a sample from the IGTD
## Estimates the parameters of the IGTD with g="normal"
x<-rig(300,mu=1,lambda=1,kernel="normal")
mleig(x, kernel="normal")
}

\keyword{univar}
