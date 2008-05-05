\name{gbs}

\alias{dgbs}
\alias{pgbs}
\alias{qgbs}
\alias{rgbs}

\title{The generalized Birnbaum-Saunders distribution (GBSD)}

\description{
Density, distribution function, quantile function and random generation for the 
generalized Birnbaum-Saunders distribution with mean parameter \eqn{\alpha}, scale parameter 
\eqn{\beta} and associated kernel.
}

\usage{
dgbs(x, alpha = 1.0, beta = 1.0, nu = 1.0, kernel = "normal", 
     log = FALSE)

pgbs(q, alpha = 1.0, beta = 1.0, nu = 1.0, kernel = "normal", 
     lower.tail = TRUE, log.p = FALSE)
     
qgbs(p, alpha = 1.0, beta = 1.0, nu = 1.0, kernel = "normal", 
     lower.tail = TRUE, log.p = FALSE)
     
rgbs(n, alpha = 1.0, beta = 1.0, nu = 1.0, kernel = "normal")     
}

\arguments{
  \item{x, q}{Vector of observations or quantiles.}
  \item{p}{Vector of probabilities.}
  \item{alpha}{Shape parameter.}
  \item{beta}{Scale parameter.}
  \item{nu}{Shape parameter corresponding to the degrees of freedom of the t 
            distribution. In the case of the Laplace, logistic, normal kernels,
            nu can be fixed at the value 1.0 since this parameter is not 
            involved in these kernels.}
  \item{n}{Number of observations.}
  \item{kernel}{Kernel of the pdf of the associated symmetrical distribution 
                by means of which the GBSD is obtained. The kernels: 
                laplace, logistic, normal and 
                t are available.}
  \item{log, log.p}{Logical; if TRUE, probabilities p are given as log\eqn{(p)}.}
  \item{lower.tail}{Logical; if TRUE (default), probabilities are \eqn{P(X \leq x)},
  otherwise, \eqn{P(X > x)}.}
}

\details{
Probability density function for the GBSD with shape parameter \eqn{\alpha}, scale 
parameter \eqn{\beta} and associated kernel \eqn{g}. The GBSD is a 
generalization of the BSD; for details see Sanhueza et al. 
(2008). The argument \eqn{g} corresponds to the kernel of the pdf of the 
associated symmetrical distribution. In the \pkg{gbs} package, the GBSD can be 
obtained from the following kernels: Laplace, logistic, normal (classical 
case) and Student-\eqn{t}. All these kernels are implemented in the \code{R} 
software. The Laplace or double exponential distribution can be obtained 
from the \pkg{normalp} package developed by Mineo (2005).

If \eqn{\alpha}, \eqn{\beta} or \eqn{g} are not specified, then they assume the default 
values 1.0, 1.0 and \code{"normal"}, respectively. 

The GBSD has pdf given by
\deqn{
f_T(t)= f_Z(a_t) \, A_t, \quad t > 0, \alpha>0, \beta>0,
}
where \eqn{f_Z(\cdot) =  c\,g(\cdot)} is the pdf of the associated symmetrical about zero distribution,
\eqn{a_{t} = a_{t}(\alpha,\beta) = [1/\alpha] [\sqrt{t/\beta} - 
\sqrt{\beta/t}]} and \eqn{A_{t}} is the derivative of \eqn{a_{t}}.

It is not possible to find the quantile function of the GBSD in a closed analytical 
form, so these values must be obtained by numerical methods.

Statistical inference tools may not exist in closed form for the GBSD, which is 
not the case for the classical GBSD. Hence, simulation and numerical studies are 
needed, which require a random number (r.n) generator. The gbs package has implemented
an r.n. generator according to Sanhueza et al. (2008).}
             
\value{
\code{dgbs()} gives the density, \code{pgbs()} gives the distribution function, 
\code{qgbs()} gives the quantile function and \code{rgbs()} generates random numbers
from the GBSD.
}

\references{Diaz-Garcia, J.A., Leiva, V. (2005)  A new family of life
distributions based on elliptically contoured distributions. J. Stat. Plan. Infer.
128:445-457 (Erratum: J. Stat. Plan. Infer. 137:1512-1513).

Leiva, V., Barros, M., Paula, G.A., Sanhueza, A. (2008) Generalized 
Birnbaum-Saunders distributions applied to air pollutant concentration.
Environmetrics 19:235-249.

Mineo, A. (2003). A new package for the general error distribution. 
R News 3:13-16.

Sanhueza, A., Leiva, V., Balakrishnan, N. (2008) The generalized Birnbaum-Saunders
distribution and its theory, methodology and application. Comm. Stat. Theory and
Meth. 37:645-670.}

\author{
Barros, Michelli <michelli.karinne@gmail.com> \cr 
Leiva, Victor <victor.leiva@uv.cl, victor.leiva@yahoo.com> \cr
Paula, Gilberto A. <giapaula@ime.usp.br> 
}

\examples{
## Computes the pdf of the GBSD with g = "normal" for a vector x with alpha = 1.0, 
## beta = 1.0
x  <- seq(0.01, 4, by = 0.01)
fx <- dgbs(x, alpha = 1.0, beta = 1.0, nu = 1.0, kernel = "normal")
print(fx)

## At the end there is a graph of this pdf
plot(x, fx, main = "pdf of the GBSD (classical case)", ylab = "f(x)")

## Computes the cdf of the GBSD with g = "normal" for a vector x with alpha = 1.0, 
## beta = 1.0
Fx <- pgbs(x, alpha = 1.0, beta = 1.0, nu = 1.0, kernel = "normal")
print(Fx)

## At the end there is a graph of this cdf
plot(x, Fx, main = "cdf of the GBSD (classical case)", ylab = "F(x)")

## Compute the 50 percentile (median) for a vector of probabilities x
## of the gbs with alpha = 1.0, beta = 1.0 and kernel = "normal"
q <- qgbs(0.5, alpha = 1.0, beta = 1.0, nu = 1.0, kernel = "normal")
q

## Generates a sample x from the GBSD with normal kernel. 
## At the end we have the histogram of x
x <- rgbs(1000, alpha = 1.0, beta = 1.0, nu = 1.0, kernel = "normal")
hist(x, main = "Histogram of a sample from GBSD")
}

\keyword{distribution}