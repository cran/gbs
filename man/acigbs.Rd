\name{acigbs}

\alias{acigbs}

\title{Approximate confidence region for the parameters of the GBSD}

\description{
  The function acigbs() produces a plot of an approximate confidence 
  region and computes approximate confidence intervals (ACI) for the parameters
  \eqn{\alpha} and \eqn{\beta} of the GBSD from a sample of observations.
}

\usage{
  acigbs(x, kernel = "normal", confLevel = 95, 
         chart = c(NULL, NULL, NULL, NULL), 
         colourRegion = 1,
         colourEstimates = 2)
}

\arguments{
  \item{x}{Vector of observations.}
  \item{kernel}{Kernel of the pdf of the associated symmetrical distribution by 
                means of which the GBSD is obtained. The kernels: 
                laplace, logistic, normal and t are available.}
  \item{confLevel}{Confidence level of the region.}
  \item{chart}{Vector of limits of the graphs. It is a vector of the type:
               c(xmin, xmax, ymin, ymax).}
  \item{colourRegion}{Color of an approximate confidence region in the plot.}
  \item{colourEstimates}{Color of MLE estimators in the plot.}
}

\details{
  In order to construct a confidence region for \eqn{\alpha} and \eqn{\beta}
  of the GBSD, we use the asymptotic normality of the MLEs.
}

\value{ 
  acigbs() shows a plot of an approximate confidence region and computes 
  approximate confidence intervals (ACI) for the parameters of the GBSD 
  considering the established confidence level from a sample of observatons giving 
  results according to the following list:
  \item{alphaEstimate}{Return the value of the MLE of alpha.}
  \item{alphaAci}{Return 95\% ACI for alpha.}
  \item{betaEstimate}{Return the value of the MLE of beta.}
  \item{betaAci}{Return 95\% ACI for beta.}
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

## Estimates the parameters of the GBSD with normal kernel by means of a 
## 95\% approximate confidence region and produces a graphical confidence 
## region
acigbs(x, kernel = "normal", confLevel = 95, colourRegion = 1, colourEstimates = 1)
}

\keyword{univar}
\keyword{htest}
\keyword{math}