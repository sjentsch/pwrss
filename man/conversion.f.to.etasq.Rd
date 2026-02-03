\name{f.to.etasq}
\alias{f.to.etasq}
\alias{etasq.to.f}

\title{Conversion from Cohen's f to Eta-squared}

\description{
Helper function to convert between Cohen's f and Eta-squared.
}

\usage{
f.to.etasq(f, verbose = 1)

etasq.to.f(eta.squared, verbose = 1)
}

\arguments{
\item{f}{Cohen's f.}
\item{eta.squared}{(Partial) Eta-squared.}
\item{verbose}{\code{1} by default (returns test, hypotheses, and results), if \code{2} a more detailed output is given (plus key parameters and defintions), if \code{0} no output is printed on the console.}
}

\value{
\item{f}{Cohen's f.}
\item{eta.squared}{(Partial) Eta-squared.}
}

\examples{
f.to.etasq(f = 0.10) # small
f.to.etasq(f = 0.25) # medium
f.to.etasq(f = 0.40) # large
}

\references{
  Cohen, J. (1988). Statistical power analysis for the behavioral sciences (2nd ed.). Lawrence Erlbaum Associates.
}
