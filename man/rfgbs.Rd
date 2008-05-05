\name{rfgbs}

\alias{rfgbs}

\title{Reliability function of the GBSD}

\description{
  Reliability function (rf) for the GBSD with shape parameter \eqn{alpha}, 
  scale parameter \eqn{beta} and associated kernel \eqn{g}.
}

\usage{
rfgbs(x, alpha = 1.0, beta = 1.0, nu = 1.0, kernel = "normal")
}

\arguments{
  \item{x}{Vector of observations.}
  \item{alpha}{Shape parameter.}
  \item{beta}{Scale parameter.}
  \item{nu}{Shape parameter corresponding to the degrees of freedom of the t 
            distribution. In the case of the Laplace, logistic, normal kernels,
            nu can be fixed at the value 1.0 since this parameter is not 
            involved in these kernels.}
  \item{kernel}{Kernel of the pdf of the associated symmetrical distribution 
                by means of which the GBSD is obtained. The kernels: 
                laplace, logistic, normal and 
                t are available.}
}

\details{
  The GBSD has rf given by
  \deqn{
        S_T(t) = 1- F_T(t); \ t >0.
        }
}

\value{\code{rfgbs()} gives the rf of an GBSD.}

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
## Computes the rf of the GBSD with normal kernel for a vector x with alpha = 1, 
## beta = 1
x  <- seq(0.01, 4, by = 0.01)
rfx <- rfgbs(x, alpha = 1.0, beta = 1.0, nu = 1.0, kernel = "normal")
print(rfx)

## At the end there is the graph of this pdf
plot(x, rfx, main = "Rf of the GBSD (classical case)", ylab = "R(x)")
}

\keyword{univar}
\keyword{survival}
