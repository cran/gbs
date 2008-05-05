\name{loglikgbs}

\alias{loglikgbs}

\title{Loglikelihood function associated with the GBSD}

\description{
  This function computes the numerical value of the loglikelihood function 
  associated with the GBSD.
}

\usage{
loglikgbs(x, nu = 1.0, kernel = "normal")
}

\arguments{
  \item{x}{Vector of observations.}
  \item{nu}{Shape parameter corresponding to the degrees of freedom of the t 
            distribution. In the case of the Laplace, logistic, normal kernels,
            nu can be fixed at the value 1.0 since this parameter is not 
            involved in these kernels.}
  \item{kernel}{Kernel of the pdf of the associated symmetrical distribution 
                by means of which the GBSD is obtained. The kernels: 
                laplace, logistic, normal and 
                t are available.}
}

\value{
Compute the loglikelihood function associated to the GBS distribution.
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

## Computes the value for the likelihood for a sample x from the GBSD 
## with normal kernel
loglikgbs(x, nu = 1.0, kernel = "normal")
}

\keyword{univar}
\keyword{htest}
\keyword{math}