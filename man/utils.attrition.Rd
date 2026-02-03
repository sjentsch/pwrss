\name{inflate.sample}
\alias{inflate.sample}

\title{Inflate Sample Size for Attrition}

\description{

  Helper function to inflate sample size for attrition.

}

\usage{
  inflate.sample(n, rate = 0.05,
               ceiling = TRUE,
               verbose = 1)
}

\arguments{
  \item{n}{sample size.}
  \item{rate}{attrition rate.}
  \item{ceiling}{rounds-up the inflated sample size.}
  \item{verbose}{\code{1} by default (returns test, hypotheses, and results), if \code{2} a more detailed output is given (plus key parameters and defintions), if \code{0} no output is printed on the console.}
}

\value{inflated sample size.}

\examples{

inflate.sample(n = 100, rate = 0.05)

}

