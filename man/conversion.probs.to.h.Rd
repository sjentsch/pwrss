\name{probs.to.h}
\alias{probs.to.h}

\title{Conversion from Probability Difference to Cohen's h}

\description{

  Helper function to convert probability difference to Cohen's h (and vice versa).

}

\usage{

probs.to.h(prob1, prob2 = 0.50, verbose = 1)

}

\arguments{
  \item{prob1}{probability of success in the first group, or under the alternative hypothesis in the one-sample case).}
  \item{prob2}{probability of success in the second group, or under the null hypothesis in the one-sample case).}
  \item{h}{Cohen's h effect size.}
  \item{verbose}{\code{1} by default (returns test, hypotheses, and results), if \code{2} a more detailed output is given (plus key parameters and defintions), if \code{0} no output is printed on the console.}
}

\value{
  \item{p1}{probability of success in the first group, or under the alternative hypothesis in the one-sample case).}
  \item{p2}{probability of success in the second group, or under the null hypothesis in the one-sample case).}
  \item{h}{Cohen's h effect size.}
}


\examples{

probs.to.h(prob1 = 0.56, prob2 = 0.50)

}

