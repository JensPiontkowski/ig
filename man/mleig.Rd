\name{mleig}

\alias{mleig}

\title{Maximum likelihood estimation of the IGTD}

\description{
  The function \code{mleig} gives the maximum likelihood estimate (MLE) of the 
  parameters mu and lambda of the IGD (classical case) from a sample of observations 
  based on this distribution.
}

\usage{
mleig(x)
}

\arguments{
  \item{x}{Vector of observations.}
}

\details{
  The MLEs of the parameters mu and lambda of the classical IG distribution
  are obtained using the analytical expressions of these estimators.
}

\value{
  \code{mleig()} computes MLEs for the parameters of the classical IGD 
  giving results according to the following list:
  \item{muEstimate}{Returns the value of the MLE of mu.}
  \item{lambdaEstimate}{Returns the value of the MLE of lambda.}
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

## Computes the likelihood for a sample x from the IGTD with normal kernel
mleig(x)
}

\keyword{univar}
\keyword{htest}
