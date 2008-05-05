\name{histgbs}

\alias{histgbs}

\title{Histogram, box-plot and estimated pdf of the GBSD}

\description{
  The function {histgbs()} produces a histogram and a box-plot of the data.
  Also, the estimated pdf may be sketched on the histogram.
}

\usage{
  histgbs(x, kernel = "normal", boxPlot = "TRUE", densityLine = "FALSE", 
          mainTitle = "Histogram and boxplot", xLabel = "Data", 
          yLabel = "Frequency", yRange = NULL, colourHistogram = "blue", 
          colourDensity = "black", colourBoxPlot = "blue")
}

\arguments{
  \item{x}{Vector of observations.}
  \item{kernel}{Kernel of the pdf of the associated symmetrical distribution 
                by means of which the GBSD is obtained. The kernels: 
                laplace, logistic, normal and 
                t are available.}
  \item{boxPlot}{Logical; if TRUE (default), the boxplot is plotted, 
                 otherwise not.}
  \item{densityLine}{Logical; if TRUE, the pdf is sketched on the histogram, 
                     otherwise not.}
  \item{mainTitle}{Main title of the graph.}
  \item{xLabel}{A title for the \eqn{x} axis.}
  \item{yLabel}{A title for the \eqn{x} axis.}
  \item{yRange}{Limit for the \eqn{y} axis.}
  \item{colourHistogram}{Color inside the histogram.}
  \item{colourDensity}{Color of the estimated pdf curve.}
  \item{colourBoxPlot}{Color inside the boxplot.}
}

\details{
  The function {histgbs()} simultaneously produces a box-plot and a 
  histogram for the data. The box-plot may be supressed by the instruction 
  {boxPlot = FALSE}. Also, the estimated pdf may be sketched on the 
  histogram adding the instruction {densityLine = TRUE}.
}

\value{
 The function {histgbs()} carries out an exploratory graphical analysis
 and can be also useful as a simple goodness-of-fit tool.
}

\references{Diaz-Garcia, J.A., Leiva, V. (2005)  A new family of life
distributions based on elliptically contoured distributions. J. Stat. Plan. Infer.
128:445-457 (Erratum: J. Stat. Plan. Infer. 137:1512-1513).

Leiva, V., Barros, M., Paula, G.A., Sanhueza, A. (2008) Generalized 
Birnbaum-Saunders distributions applied to air pollutant concentration.
Environmetrics 19:235-249.

Sanhueza, A., Leiva, V., Balakrishnan, N. (2008) The generalized Birnbaum-Saunders
distribution and its theory, methodology and application. Comm. Stat. Theory and
Meth. 37:645-670.}

\author{
Barros, Michelli <michelli.karinne@gmail.com> \cr 
Leiva, Victor <victor.leiva@uv.cl, victor.leiva@yahoo.com> \cr
Paula, Gilberto A. <giapaula@ime.usp.br> 
}


\examples{
## Generates a sample from the GBSD with normal kernel 
x <- rgbs(300, alpha = 1.0, beta = 1.0, nu = 1.0, kernel = "normal")

## Produces a histogram for the GBSD with normal kernel from the data x
histgbs(x, 
        mainTitle = "", 
        xLabel = "Data",
        kernel = "normal", 
        colourHistogram = 4, 
        colourBoxPlot = 4)
}

\keyword{univar}