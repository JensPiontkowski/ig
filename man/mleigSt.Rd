\name{mleigSt}

\alias{mleigSt}

\title{MLE of the parameters of the IGTD generated from the t kernel}

\description{
  The function \code{mleigSt} gives the maximum likelihood estimates (MLE) of the 
  parameters mu and lambda of the IGTD generated from the t kernel based on a sample of 
  observations based on this distribution.
}

\usage{
mleigSt(x)
}

\arguments{
  \item{x}{Vector of observations.}
}

\details{
  The MLEs of the parameters mu and lambda of the classical IG distribution
  are obtained using the analytical expressions of these estimators. In the case
  of the IGTD generated from the t kernel, the MLEs of the parameters mu and lambda 
  must be obtained using numerical procedure already implmented in \code{R}. 
  In this procedure, the parameter nu is obtained by using an optimal methodology
  based on the data.
}

\value{
 \code{mleigSt()} computes MLEs for the parameters of the IGTD 
  generated from the t kernel giving results according to the following list:
  \item{muEstimate}{Returns the value of the MLE of mu.}
  \item{lambdaEstimate}{Returns the value of the MLE of lambda.}
  \item{nuOptimal}{Returns the optimal value for nu.}
  \item{loglikelihood}{Returns the value of the IGTD loglikelihood.}
}

\references{Sanhueza, A., Leiva, V., Balakrishnan, N. (2008). A new class of 
            inverse Gaussian type distributions. Metrika (in press).}

\author{
Víctor Leiva <victor.leiva@uv.cl; victor.leiva@yahoo.com>, \cr
Hugo Hernández <hugo.hernandez.p@gmail.com> and \cr
Antonio Sanhueza <asanhueza@ufro.cl>.
}

\examples{
## Generates a sample from the IGTD with t kernel
x <- rigt(300, mu = 1.0, lambda = 1.0, nu = 5, kernel = "t")

## Computes the likelihood for a sample x from the IGTD with t kernel
mleigSt(x)
}

\keyword{univar}
\keyword{htest}
