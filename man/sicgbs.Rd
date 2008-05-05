\name{sicgbs}

\alias{sicgbs}

\title{Schwartz information criterion for a sample from the GBSD}

\description{
  The function \code{sicgbs()} gives the Schwartz information criterion (SIC) 
  value assuming a GBSD with parameters \eqn{\alpha}, \eqn{\beta}and a 
  specific kernel.
}

\usage{
sicgbs(x, nu = 1.0, kernel = "normal")
}

\arguments{
  \item{x}{Vector of observations.}
  \item{nu}{Shape parameter corresponding to the degrees of freedom of the t 
            distribution. In the case of the Laplace, logistic, normal kernels,
            nu can be fixed at the value 1.0 since this parameter is not 
            involved in these kernels.}
  \item{kernel}{Kernel of the pdf of the associated symmetrical distribution 
                by means of which the GBSD is obtained. The kernels: 
                {"laplace"}, {"logistic"}, {"normal"} and 
                {"t"} are available.}
}

\details{
  The SIC is a selection model criterion based on information loss. According 
  to this criterion, it is possible to choice a hypothetic model that better 
  describe the data set considering the smaller SIC value. The SIC is defined as 
  \eqn{SIC = -l(\theta)/n+ p \log(n)/(2n)}, where \eqn{l(\theta)} is the 
  log-likelihood function associated with the model, \eqn{n} is the sample size 
  and \eqn{p} is the number of involved parameters; for more details
  see Spieglhaiter et al.  (2002).
}

\value{
 \code{sicgbs()} gives the value for the SIC of the GBSD.
}

\references{
Diaz-Garcia, J.A., Leiva, V. (2005)  A new family of life
distributions based on elliptically contoured distributions. J. Stat. Plan. Infer.
128:445-457 (Erratum: J. Stat. Plan. Infer. 137:1512-1513).

Leiva, V., Barros, M., Paula, G.A., Sanhueza, A. (2008) Generalized 
Birnbaum-Saunders distributions applied to air pollutant concentration.
Environmetrics 19:235-249.

Sanhueza, A., Leiva, V., Balakrishnan, N. (2008) The generalized Birnbaum-Saunders
distribution and its theory, methodology and application. Comm. Stat. Theory and
Meth. 37:645-670.

  Spieglhaiter, D. J., Best, N. G., Carlin, B. P., van der Linde, A. (2002).
  Bayesian measures of complexity and fit. Journal of the Royal Statistical 
  Society Series B 64, 1-34.
}


\author{
Barros, Michelli <michelli.karinne@gmail.com> \cr 
Leiva, Victor <victor.leiva@uv.cl, victor.leiva@yahoo.com> \cr
Paula, Gilberto A. <giapaula@ime.usp.br> 
}


\examples{
## Generates a sample from the GBSD with normal kernel
x <- rgbs(300, alpha = 1.0, beta = 1.0,  nu = 1.0, kernel = "normal")

## Computes the SIC value of the GBSD with normal kernel from the data x
sicgbs(x, nu = 1.0, kernel = "normal")
}

\keyword{univar}
\keyword{htest}
\keyword{math}