\name{rcgbs}

\alias{rcgbs}

\title{Relative change on the MLE of the GBSD}

\description{
  The function \code{rcgbs()} computes the relative change (RC) on the MLE 
  when some observations are removed in order to evaluate the effect of 
  their potential influence.
}

\usage{
rcgbs(x, casesRemoved = NULL, kernel = "normal")
}

\arguments{
  \item{x}{Vector of observations.}
  \item{casesRemoved}{Index of the potentially influential case(s) that must 
                      be removed.}
  \item{kernel}{Kernel of the pdf of the associated symmetrical distribution 
                by means of which the GBSD is obtained. The kernels: 
                laplace, logistic, normal and 
                t are available.}
}

\details{
  This function computes the relative changes (RC), in percentage, of each 
  estimated parameter, defined by \eqn{\mbox{RC}_{\theta_j} = |(\hat \theta_j -
  \hat \theta_{j(\mbox{\tiny I})})/\hat \theta_j| \times 100\%}, where 
  \eqn{\hat \theta_{j(\mbox{\tiny I})}} denotes the MLE of \eqn{\theta_j} after 
  the set I of cases has been removed.
}

\value{
  \code{rcgbs()} gives the RCs on the MLEs of the parameters of the GBSD from 
  a sample of observations without to consider some potentially influential case(s) 
  related to the MLE of the complete sample.
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

## Computes the RC the MLE of the parameters of the GBSD with g="normal" when the
##  case 1 has been removed
rcgbs(x, casesRemoved = c(1), kernel = "normal")
}

\keyword{univar}
\keyword{htest}
\keyword{math}
