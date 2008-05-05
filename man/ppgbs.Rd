\name{ppgbs}

\alias{ppgbs}

\title{probability versus probability plot for the the GBSD}

\description{
  The function {ppgbs()} produces a probability-probability (pp) plot for the GBSD
  based on the MLE of their parameters. Also, a line going through the first and 
  the third quartile can be sketched. In addition, the coefficient of determination 
  of least squares for the fit line is given.
}

\usage{
ppgbs(x, kernel = "normal", line = FALSE, 
      xLabel = "Empirical distribution function", 
      yLabel = "Theorical distribution function")
}

\arguments{
  \item{x}{Vector of observations.}
  \item{kernel}{Kernel of the pdf of the associated symmetrical distribution 
                by means of which the GBSD is obtained. The kernels: 
                laplace, logistic, normal and 
                t are available.}
  \item{line}{Logical; if TRUE (default), a line going by the first and third 
              quartile is sketched.}
  \item{xLabel}{A title for the \code{x} axis.}
  \item{yLabel}{A title for the \code{x} axis.}
}

\details{
  The function {ppgbs()} carries out a pp plot for the GBSD.
}

\value{
 The function \code{ppgbs()} carries out an graphical plot
 useful as goodness-of-fit tool.
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

## Produces a pp plot for the GBSD with normal kernel
ppgbs(x, kernel = "normal", line = TRUE)
}

\keyword{univar}
\keyword{htest}