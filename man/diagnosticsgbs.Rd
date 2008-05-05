\name{diagnosticsgbs}

\alias{diagnosticsgbs}

\title{Influence diagnostics plots for the GBSD}

\description{
  The function \code{diagnosticsgbs()} produces an index plot of total 
  local influence.
}

\usage{
  diagnosticsgbs(x, kernel = "normal", mainTitle = "", yRange = NULL)
}

\arguments{
  \item{x}{Vector of observations.}
  \item{kernel}{Kernel of the pdf of the associated symmetrical distribution by 
                means of which the GBSD is obtained. The kernels: 
                \code{"laplace"}, \code{"logistic"}, \code{"normal"} and 
                \code{"t"} are available.}
  \item{mainTitle}{An overall title for the plot.}
  \item{yRange}{Limit for the \eqn{y} axis.}
}

\details{
  The Cook's local influence diagnostics method (1986) is used to evaluate the 
  local influence by means of likelihood displacement.
}

\value{
  \code{diagnosticsgbs()} gives an influence diagnostics through a graphical 
  plot for the GBSD from a sample of observations.
}

\references{Cook, R.D. (1986) Assessment of local influence (with discussion).
J. Royal Stat. Soc. B 48: 133-169.

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

## Produces influence diagnostics for the GBSD with normal kernel
diagnosticsgbs(x, kernel = "normal")
}

\keyword{univar}

