\name{mlegbsc}

\alias{mlegbsc}

\title{MLE of the parameters of the GBSD with censored data}

\description{
  The function \code{mlegbsc} gives the maximum likelihood estimates (MLE) of the 
  parameters \eqn{\alpha} and \eqn{\beta} of the GBSD generated from the kernels: Laplace, logistic and
  normal (classical case) based on a sample of type II censored observations based on this distribution.
}

\usage{
mlegbsc(x, status)
}

\arguments{
  \item{x}{Vector of observations.}
  \item{status}{Vector indicating if the observation is uncensored taking a value 
equal to one (1) or 
                censored taking a value equal to zero (0).}
}

\details{
  The MLEs of the parameters \eqn{\alpha} and \eqn{\beta} of the GBS distribution
  generated from the kernels: Laplace, logistic and normal with censored data, must be obtained 
  using numerical procedure already implemented in \code{R}.
}

\value{
 {mlegbsc()} computes MLEs for the parameters of the GBSD 
  generated from the kernels: Laplace, logistic and normal giving results 
  according to the following list:
  \item{alphaEstimate}{Returns the value of the MLE of alpha.}
  \item{betaEstimate}{Returns the value of the MLE of beta.}
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
x <- rgbs(300, alpha = 1.0, beta = 1.0, nu = 5, kernel = "normal")

## Computes the likelihood for a sample x from the GBSD with normal kernel
## with type II censored data

pc     <- 20
status <- c(rep(1, floor((length(x) - pc) * (length(x)) / 100)), 
            rep(0, (floor(pc * (length(x)) / 100))))
mlegbsc(x, status)
}

\keyword{univar}
\keyword{htest}