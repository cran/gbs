\name{wgp}

\alias{wgp}

\title{Derivatives of the weights in the likelihood of the GBSD}

\description{
  This function computes the derivatives of the weights in the likelihood 
  function of the GBS distribution.
}

\usage{
wgp(u, nu = 1.0, kernel = "normal")
}

\arguments{
  \item{u}{Vector of values.}
  \item{nu}{Shape parameter corresponding to the degrees of freedom of the t 
            distribution. In the case of the Laplace, logistic, normal kernels,
            nu can be fixed at the value 1.0 since this parameter is not 
            involved in these kernels.}
  \item{kernel}{Kernel of the pdf of the associated symmetrical distribution 
                by means of which the GBSD is obtained. The kernels: 
                Laplace, logistic, normal and 
                t are available.}
}

\value{
  \code{wgp()} return the derivatives of the weights in the likelihood 
  function of the GBS distribution.
}

\references{
Sanhueza, A., Leiva, V., Balakrishnan, N. (2008) The generalized Birnbaum-Saunders
distribution and its theory, methodology and application. Comm. Stat. Theory and
Meth. 37:645-670.}

\author{
Barros, Michelli <michelli.karinne@gmail.com> \cr 
Leiva, Victor <victor.leiva@uv.cl, victor.leiva@yahoo.com> \cr
Paula, Gilberto A. <giapaula@ime.usp.br> 
}

\keyword{math}
\keyword{htest}