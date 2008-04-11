\name{descriptiveSummary}

\alias{descriptiveSummary}

\title{Descriptive summary of the data}

\description{
  The function \code{descriptiveSummary()} gives a descriptive statistics of the
  data.
}

\usage{
  descriptiveSummary(x)
}

\arguments{
  \item{x}{Vector of observations.}
}

\details{
  The function \code{descriptiveSummary()} gives a descriptive statistics of the
  data containing: mean (\eqn{\overline{x}}), median, mode, standard deviation (\eqn{s}), coefficients of
  variation, skewness and kurtosis, range, minimum, maximum and the number
  de observations. This function uses the command \code{searchMode()} that allows
  to find the empirical mode of the data.

  Coefficient of variation (CV):
  \deqn{CV=\frac{s}{\overline{x}}\times 100 \quad (\rm{in} \ \%).} 

  Coefficient of skewness (CS):
  \deqn{CS=\frac{1}{n}\frac{\sum_{i=1}^{n}(x_i-\overline{x})^3}{s^3}.}

  Coefficient of kurtosis (CK):
  \deqn{CK=\frac{1}{n}\frac{\sum_{i=1}^{n}(x_i-\overline{x})^4}{s^4}-3.}
}

\value{
  The function \code{descriptiveSummary()} carries out a descriptive summary of the data
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

## Produces a descriptive statistics of the data x
descriptiveSummary(x)
}

\keyword{univar}
