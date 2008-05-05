
\name{dlaplace}

\alias{dlaplace}

\title{Density of the Laplace distribution}

\description{
  Probabilility density function of the Laplace distribution.
}

\usage{
dlaplace(x)
}

\arguments{
  \item{x}{Vector of observations.}
}

\details{The Laplace distribution has pdf given by
  \deqn{
        f_X(x)=(1/2) \exp(-|x|), \quad -\infty < x < + \infty
       }  }

\value{
 \code{dlaplace()} gives the pdf of an Laplace distribution.
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
# Produces a graphical plot for the Laplace distribution
x <- seq(-3, 3, by = 0.01)
y <- dlaplace(x)
plot(x, y, type = "l", xlab = "x", ylab = "f(x)")
}

\keyword{distribution}