\name{f.to.rsq}
\alias{f.to.rsq}
\alias{rsq.to.f}

\title{Conversion from Cohen's f to R-squared}

\description{
Helper function to convert between Cohen's f and R-squared.
}

\usage{
  rsq.to.f(r.squared.full, r.squared.reduced = 0, verbose = 1)

  f.to.rsq(f, r.squared.full = NULL, verbose = 1)
}

\arguments{
  \item{f}{Cohen's f.}
  \item{r.squared.full}{R-squared for the full model.}
  \item{r.squared.reduced}{R-squared for the reduced model.}
  \item{verbose}{\code{1} by default (returns test, hypotheses, and results), if \code{2} a more detailed output is given (plus key parameters and defintions), if \code{0} no output is printed on the console.}
}

\value{
  \item{f}{Cohen's f.}
  \item{f.squared}{Cohen's f-squared.}
  \item{r.squared.full}{R-squared for the full model.}
  \item{r.squared.reduced}{R-squared for the reduced model.}
}

\examples{
  f.to.rsq(f = 0.10) # small
  f.to.rsq(f = 0.25) # medium
  f.to.rsq(f = 0.40) # large
}

\references{
Cohen, J. (1988). Statistical power analysis for the behavioral sciences (2nd ed.). Lawrence Erlbaum Associates.

Selya, A. S., Rose, J. S., Dierker, L. C., Hedeker, D., & Mermelstein, R. J. (2012). A practical guide to calculating Cohen's f2, a measure of local effect size, from PROC MIXED. Frontiers in Psychology, 3, 111. \doi{10.3389/fpsyg.2012.00111}
}
