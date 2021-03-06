\name{dgbskotz}

\alias{dgbskotz}

\title{Pdf of the GBSD generated from the Kotz kernel}

\description{
  This function computes the probabilility density function of the GBSD
  generated from a Kotz kernel with parameters \eqn{q}, \eqn{r} and \eqn{s}.
}

\usage{
  dgbskotz(x, alpha = 1.0, beta = 1.0, parameters = c(1.0, 1.0, 1.0), 
           log = FALSE)
}

\arguments{
  \item{x}{Vector of observations.}
  \item{alpha}{Shape parameter.}
  \item{beta}{Scale parameter.}
  \item{parameters}{Parameters of the Kotz distribution denoted by
        \eqn{q}, \eqn{r} and \eqn{s}.}
  \item{log}{Logical; if TRUE, probabilities \eqn{p} are given as \eqn{\mbox{log}(p)}.}
}

\details{
For details about the pdf of the GBSD with Kotz kernel see Sanhueza et al. (2008).
}

\value{
 {dgbskotz()} gives the pdf of an GBSD generated from the Kotz kernel.
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
# Produces a graphical plot for the GBSD from the Kotz kernel
# with parameters q = 1, r = 2 and s = 3 
x <- seq(-3, 3, by = 0.01)
y <- dgbskotz(x, 1.0, 1.0, parameters = c(1, 2, 3))
plot(x, y, type = "l", xlab = "x", ylab = "f(x)")
}

\keyword{distribution}