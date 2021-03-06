\name{ksgbs}

\alias{ksgbs}

\title{Test of Kolmogorov-Smirnov for the GBSD}

\description{
  The function {ksgbs} gives the values for the Kolmogorov-Smirnov (KS) 
  test assuming a GBSD with parameters \eqn{\alpha}, \eqn{\beta} and an specific kernel. 
  In addition, optionally, this function allows one to obtain a comparative 
  graph between the empirical and theoretical cdfs for a given data set.
}

\usage{
  ksgbs(x, kernel = "normal", graph = FALSE, 
        mainTitle = "Cumulative distribution function", xLabel = "data", 
        yLabel = "cdf")
}

%- maybe also 'usage' for other objects documented here.

\arguments{
  \item{x}{Vector of observations.}
  \item{kernel}{Kernel of the pdf of the associated symmetrical distribution 
                by means of which the GBSD is obtained. The kernels: 
                {"laplace"}, {"logistic"}, {"normal"} and 
                {"t"} are available.}
  \item{graph}{Logical; if TRUE (default), the cdf plot is provided.}
  \item{mainTitle}{Main title of the graph.}
  \item{xLabel}{A title for the \eqn{x} axis.}
  \item{yLabel}{A title for the \eqn{y} axis.}
}

\details{
  The Kolmogorov-Smirnov test is a goodness-of-fit technique based on the 
  maximum distance between the empirical and theoretical cdfs.
}

\value{
The function {ksgbs()} carries out de Kolmogorov-Smirnov test for the GBSD.
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
x <- rgbs(300, alpha = 1.0, beta = 1.0,  nu = 1.0, kernel = "normal")

## Produces a KS test abd produces a graph for the GBSD with normal kernel
  ksgbs(x, kernel = "normal", graph = TRUE, xLabel = "Data",
        yLabel = "Cumulative distribution function")
}

\keyword{univar}
\keyword{htest}
\keyword{math}

