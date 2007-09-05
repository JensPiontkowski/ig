\name{histig}
\alias{histig}

\title{Histogram, box-plot and estimated pdf of the IGTD}

\description{
The function \code{histig()} produces a histogram and a a box-plot for the data. Also, the estimated pdf
may be sketched on the histogram.
}

\usage{
histig(x, main = "Histogram and boxplot", xlab = "Data", ylab = "Frequency",
pdf = "FALSE", kernel.pdf = "normal", col = NULL,
boxplot = "TRUE", col.pdf = 1, col.boxplot = 4)
}

\arguments{
  \item{x}{Vector of observations.}
  \item{main}{A title of histogram.}
  \item{xlab}{A title for the x axis.}
  \item{ylab}{A title for the x axis.}
  \item{pdf}{Logical; if TRUE, the pdf is sketched on the histogram, otherwise not.}
  \item{kernel.pdf}{Kernel of the pdf of the associated symmetrical distribution by means of which the
  IGTD is obtained. The kernels: \code{"Laplace"}, \code{"logistic"}, \code{"normal"} and \code{"t"} are available.}
  \item{col}{Color inside the histogram.}
  \item{boxplot}{Logical; if TRUE (default), the boxplot is plotted, otherwise not.}
  \item{col.pdf}{Color of the estimated pdf curve.}
  \item{col.boxplot}{Color inside the boxplot.}
}

\details{
The function \code{histig()} simultaneously produces a box-plot and a histogram for the data.
The box-plot may be supressed by the instruction \code{boxplot=FALSE}. Also,
the estimated pdf may be sketched on the histogram adding the instruction \code{pdf=TRUE}.
}

\value{
The function \code{histig()} carries out an exploratory graphical analysis.
}

\author{
Víctor Leiva <victor.leiva@uv.cl>, Hugo Hernández <hugo.hernandez@msn.com>, and Antonio Sanhueza <asanhue@ufro.cl>.
}

\keyword{univar}
