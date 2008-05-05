\name{mlebs}

\alias{mlebs}

\title{Maximum likelihood estimation of the GBSD}

\description{
  The function {mlebs} gives the maximum likelihood estimate (MLE) of the 
  parameters \eqn{\alpha} and \eqn{\beta} of the BSD (classical case) from a sample of 
  observations based on this distribution.
}

\usage{
mlebs(x)
}

\arguments{
  \item{x}{Vector of observations.}
}

\details{
  The MLEs of the parameters \eqn{\alpha} and \eqn{\beta} of the classical IG distribution
  are obtained using the analytical expressions of these estimators.
}

\value{
  {mlebs()} computes MLEs for the parameters of the classical BSD 
  giving results according to the following list:
  \item{alphaEstimate}{Returns the value of the MLE of alpha.}
  \item{betaEstimate}{Returns the value of the MLE of beta.}
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

## Computes the likelihood for a sample x from the GBSD with normal kernel
mlebs(x)
}

\keyword{univar}
\keyword{htest}
