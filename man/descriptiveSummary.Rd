\name{descriptiveSummary}

\alias{descriptiveSummary}

\title{Descriptive summary of the data}

\description{
  The function \code{descriptiveSummary()} gives a descriptive statistics of the
  data.
}

\usage{
  descriptiveSummary(x)
}

\arguments{
  \item{x}{Vector of observations.}
}

\details{
  The function \code{descriptiveSummary()} gives a descriptive statistics of the
  data containing: mean (\eqn{\overline{x}}), median, mode, standard deviation (\eqn{s}), 
  coefficients of variation, skewness and kurtosis, range, minimum, maximum and the number
  de observations. This function uses the command \code{searchMode()} that allows
  to find the empirical mode of the data.

  Sample coefficient of variation (CV):
  \deqn{\mbox{CV}=\frac{S}{\overline{X}}\times 100 \quad (\rm{in} \ \%).} 

  Sample coefficient of skewness (CS):
  \deqn{\mbox{CS}=\frac{1}{n}\frac{\sum_{i=1}^{n}(X_i-\overline{X})^3}{S^3}.}

  Sample coefficient of kurtosis (CK):
  \deqn{\mbox{CK}=\frac{1}{n}\frac{\sum_{i=1}^{n}(X_i-\overline{X})^4}{S^4}-3.}
}

\value{
  The function \code{descriptiveSummary()} carries out a descriptive summary of the data.
}


\author{
Barros, Michelli <michelli.karinne@gmail.com> \cr 
Leiva, Victor <victor.leiva@uv.cl, victor.leiva@yahoo.com> \cr
Paula, Gilberto A. <giapaula@ime.usp.br> 
}

\examples{
## Generates a sample from the GBSD with normal kernel
x <- rgbs(300, alpha = 1.0, beta = 1.0, nu = 1.0, kernel = "normal")

## Produces a descriptive statistics of the data x
descriptiveSummary(x)
}

\keyword{univar}
