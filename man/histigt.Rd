\name{histigt}

\alias{histigt}

\title{Histogram, box-plot and estimated pdf of the IGTD}

\description{
  The function \code{histigt()} produces a histogram and a box-plot of the data.
  Also, the estimated pdf may be sketched on the histogram.
}

\usage{
  histigt(x, kernel = "normal", boxPlot = "TRUE", densityLine = "FALSE", 
          mainTitle = "Histogram and boxplot", xLabel = "Data", 
          yLabel = "Frequency", yRange = NULL, colourHistogram = "blue", 
          colourDensity = "black", colourBoxPlot = "blue")
}

\arguments{
  \item{x}{Vector of observations.}
  \item{kernel}{Kernel of the pdf of the associated symmetrical distribution 
                by means of which the IGTD is obtained. The kernels: 
                \code{"laplace"}, \code{"logistic"}, \code{"normal"} and 
                \code{"t"} are available.}
  \item{boxPlot}{Logical; if TRUE (default), the boxplot is plotted, 
                 otherwise not.}
  \item{densityLine}{Logical; if TRUE, the pdf is sketched on the histogram, 
                     otherwise not.}
  \item{mainTitle}{Main title of the graph.}
  \item{xLabel}{A title for the x axis.}
  \item{yLabel}{A title for the x axis.}
  \item{yRange}{Limit for the y axis.}
  \item{colourHistogram}{Color inside the histogram.}
  \item{colourDensity}{Color of the estimated pdf curve.}
  \item{colourBoxPlot}{Color inside the boxplot.}
}

\details{
  The function \code{histigt()} simultaneously produces a box-plot and a 
  histogram for the data. The box-plot may be supressed by the instruction 
  \code{boxPlot = FALSE}. Also, the estimated pdf may be sketched on the 
  histogram adding the instruction \code{densityLine = TRUE}.
}

\value{
 The function \code{histigt()} carries out an exploratory graphical analysis
 and can be also useful as a simple goodness-of-fit tool.
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
## Generates a sample from the IGTD with normal kernel 
x <- rigt(300, mu = 1.0, lambda = 1.0, kernel = "normal")

## Produces a histogram for the IGTD with normal kernel from the data x
histigt(x, 
        mainTitle = "", 
        xLabel = "Data",
        kernel = "normal", 
        colourHistogram = 4, 
        colourBoxPlot = 4)
}

\keyword{univar}


