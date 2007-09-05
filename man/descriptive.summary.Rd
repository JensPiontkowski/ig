\name{descriptive.summary}
\alias{descriptive.summary}

\title{Descriptive summary of the data}

\description{
The function \code{descriptive.summary()} gives a descriptive summary of the data.
}

\usage{
descriptive.summary(x)
}

\arguments{
  \item{x}{Vector of observations.}
}

\details{
The function \code{descriptive.summary()} gives a descriptive summary of the data containing: mean,
median, mode, standard deviation, coefficients of variation, skewness and kurtosis,
range,  minimum, maximum and the number de observations. This function uses the command
\code{basicStats()} of the R package named \pkg{Fbasics} and the command \code{search.mode()} that allows
to find the empirical mode of the data.
}

\value{
The function \code{descriptive.summary()} carries out a descriptive summary of the data
}

\author{
Víctor Leiva <victor.leiva@uv.cl>, Hugo Hernández <hugo.hernandez@msn.com>, and Antonio Sanhueza <asanhue@ufro.cl>.
}

\examples{
## Generates a sample from the IGTD
## Produces a descriptive summary of the data
x<-rig(300,mu=1,lambda=1,kernel="normal")
descriptive.summary (x)
}

\keyword{univar}
