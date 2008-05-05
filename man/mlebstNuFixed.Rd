\name{mlebstNuFixed}

\alias{mlebstNuFixed}

\title{MLE of the parameters of the BS-Student with fixed nu}

\description{
  The function \code{mlebstNuFixed} gives the maximum likelihood estimates (MLE) of the 
  parameters \eqn{\alpha} and \eqn{\beta} of the GBSD generated from the t kernel 
  based on a sample of observations based on this distribution.
}

\usage{
mlebstNuFixed(x, nu = 1)
}

\arguments{
  \item{x}{Vector of observations.}
  \item{nu}{Shape parameter corresponding to the degrees of freedom of the t 
            distribution.}
}

\details{
  The MLEs of the parameters \eqn{\alpha} and \eqn{\beta} of the classical BS distribution
  are obtained using numerical procedure already implemented in \code{R}. 
  In this procedure, the parameter \code{nu} is previosly fixed. This methodoly can be useful 
  for simulation studies or to determine the optimal value of nu.
}

\value{
 \code{mlebstNuFixed()} computes MLEs for the parameters of the GBSD 
  generated from the t kernel giving results according to the following list:
  \item{alphaEstimate}{Returns the value of the MLE of alpha.}
  \item{betaEstimate}{Returns the value of the MLE of beta.}
  \item{nuFixed}{Returns the fixed value for nu.}
  \item{loglikelihood}{Returns the value of the GBSD loglikelihood.}
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
## Generates a sample from the GBSD with t kernel
x <- rgbs(300, alpha = 1.0, beta = 1.0, nu = 5, kernel = "t")

## Computes the likelihood for a sample x from the GBSD with t kernel
mlebstNuFixed(x, nu = 5)
}

\keyword{univar}
\keyword{htest}
