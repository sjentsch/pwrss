\name{cors.to.q}
\alias{cors.to.q}
\alias{q.to.cors}
\alias{cor.to.z}

\title{Conversion from Correlation Difference to Cohen's q}

\description{

  Helper function to convert correlation difference to Cohen's q (and vice versa). \code{cor.to.z()} function applies Fisher's z transformation.

}

\usage{
  cor.to.z(rho, verbose = 1)

  cors.to.q(rho1, rho2, verbose = 1)

  q.to.cors(q, rho1 = NULL, rho2 = NULL, verbose = 1)

}

\arguments{
  \item{rho}{correlation.}
  \item{rho1}{first correlation.}
  \item{rho2}{second correlation.}
  \item{q}{Cohen's q effect size.}
  \item{verbose}{\code{1} by default (returns test, hypotheses, and results), if \code{2} a more detailed output is given (plus key parameters and defintions), if \code{0} no output is printed on the console.}
}

\value{
  \item{rho1}{first correlation.}
  \item{rho2}{second correlation.}
  \item{delta}{correlation difference: rho1 - rho2.}
  \item{q}{Cohen's q effect size.}
}


\examples{

q.to.cors(q = 0.10, rho1 = 0.50)
q.to.cors(q = 0.30, rho1 = 0.50)
q.to.cors(q = 0.50, rho1 = 0.50)

cors.to.q(rho2 = 0.5712027, rho1 = 0.50)
cors.to.q(rho2 = 0.6907068, rho1 = 0.50)
cors.to.q(rho2 = 0.7815365, rho1 = 0.50)

}

