#' Power Analysis for the Generic Binomial Test
#'
#' Calculates power for the generic binomial test with (optional) Type 1 and
#' Type 2 error plots.
#'
#'
#' @param size        number of trials (zero or more).
#' @param prob        probability of success on each trial under alternative.
#' @param null.prob   probability of success on each trial under null.
#' @param alpha       type 1 error rate, defined as the probability of
#'                    incorrectly rejecting a true null hypothesis, denoted as
#'                    \eqn{\alpha}.
#' @param alternative direction or type of the hypothesis test: "two.sided",
#'                    "one.sided", or "two.one.sided". For non-inferiority or
#'                    superiority tests, add or subtract the margin from the
#'                    null hypothesis value and use alternative = "one.sided".
#' @param plot        logical; \code{FALSE} switches off Type 1 and Type 2
#'                    error plot. \code{TRUE} by default.
#' @param verbose     \code{1} by default (returns test, hypotheses, and
#'                    results), if \code{2} a more detailed output is given
#'                    (plus key parameters and defintions), if \code{0} no
#'                    output is printed on the console.
#' @param pretty      logical; whether the output should show Unicode
#'                    characters (if encoding allows for it). \code{FALSE} by
#'                    default.
#'
#' @return
#'   \item{size}{number of trials (zero or more).}
#'   \item{prob}{probability of success on each trial under alternative.}
#'   \item{null.prob}{probability of success on each trial under null.}
#'   \item{binom.alpha}{critical value(s).}
#'   \item{power}{statistical power \eqn{(1-\beta)}.}
#'
#' @examples
#' # one-sided
#' power.binom.test(size = 200, prob = 0.6, null.prob = 0.5,
#'                  alpha = 0.05, alternative = "one.sided")
#'
#' # two-sided
#' power.binom.test(size = 200, prob = 0.4, null.prob = 0.5,
#'                  alpha = 0.05, alternative = "two.sided")
#'
#' # equivalence
#' power.binom.test(size = 200, prob = 0.5, null.prob = c(0.4, 0.6),
#'                  alpha = 0.05, alternative = "two.one.sided")
#'
#' @export power.binom.test
power.binom.test <- function(size,
                             prob,
                             null.prob = 0.5,
                             alpha = 0.05,
                             alternative = c("two.sided", "one.sided", "two.one.sided"),
                             plot = TRUE,
                             verbose = 1,
                             pretty = FALSE) {

  check.proportion(prob, alpha)
  check.logical(plot)

  alternative <- tolower(match.arg(alternative))

  if (any(!is.numeric(size)) || any(size < 0) || any(!(abs(size - round(size)) < .Machine$double.eps ^ 0.5)))
    stop("Incorrect value for `size`.", call. = FALSE)

  if (alternative == "two.one.sided") {
    if (isFALSE(all(is.numeric(null.prob))) || any(null.prob < 0) || any(null.prob > 1))
      stop("Incorrect value for `null.prob`.", call. = FALSE)
    if (length(null.prob) != 2)
      stop("Provide null margins in the form of null.prob = c(lower, upper)", call. = FALSE)
  } else {
    if (isFALSE(all(is.numeric(null.prob))) || length(null.prob) != 1 || any(null.prob < 0) || any(null.prob > 1))
      stop("Incorrect value for `null.prob`.", call. = FALSE)
  }

  if (alternative == "two.one.sided") {

    if (length(null.prob) != 2) stop("Null specification is not consistent with equivalence testing.", call. = FALSE)
    if (null.prob[1] > null.prob[2]) stop("Lower margin is greater than the upper margin?", call. = FALSE)

    if (prob > min(null.prob) && prob < max(null.prob)) {
      # equivalence
      q.low <- stats::qbinom(alpha, size, prob = null.prob[1], lower.tail = FALSE)
      q.high <- stats::qbinom(alpha, size, prob = null.prob[2], lower.tail = TRUE)

      prob.low <- stats::pbinom(q.low, size, null.prob[1], lower.tail = FALSE)
      prob.high <- stats::pbinom(q.high, size, null.prob[2], lower.tail = TRUE)

      q.low[prob.low > alpha] <- q.low[prob.low > alpha] + 1
      q.high[prob.high > alpha] <- q.high[prob.high > alpha] - 1

      prob.low <- stats::pbinom(q.low, size, null.prob[1], lower.tail = FALSE)
      prob.high <- stats::pbinom(q.high, size, null.prob[2], lower.tail = TRUE)
      approx.alpha <-  (prob.low + prob.high) / 2 ########## average ###########

      binom.alpha <- c(q.low, q.high)

      power <- stats::pbinom(q.low, size, prob, lower.tail = FALSE) +
        stats::pbinom(q.high, size, prob, lower.tail = TRUE) - 1

      if (power < 0) power <- 0

    } else {
      # minimal effect
      q.low <- stats::qbinom(alpha / 2, size, prob = null.prob[1], lower.tail = TRUE)
      q.high <- stats::qbinom(alpha / 2, size, prob = null.prob[2], lower.tail = FALSE)

      prob.low <- stats::pbinom(q.low, size, null.prob[1], lower.tail = TRUE)
      prob.high <- stats::pbinom(q.high, size, null.prob[2], lower.tail = FALSE)

      q.low[prob.low > alpha / 2] <- q.low[prob.low > alpha / 2] - 1
      q.high[prob.high > alpha / 2] <- q.high[prob.high > alpha / 2] + 1

      prob.low <- stats::pbinom(q.low, size, null.prob[1], lower.tail = TRUE)
      prob.high <- stats::pbinom(q.high, size, null.prob[2], lower.tail = FALSE)
      approx.alpha <- prob.low + prob.high

      binom.alpha <- c(q.low, q.high)

      power <- stats::pbinom(q.low, size, prob, lower.tail = TRUE) +
        stats::pbinom(q.high, size, prob, lower.tail = FALSE)

    }

  } else if (alternative == "two.sided") {

    q.low <- stats::qbinom(alpha / 2, size, null.prob, lower.tail = TRUE)
    q.high <- stats::qbinom(alpha / 2, size, null.prob, lower.tail = FALSE)

    prob.low <- stats::pbinom(q.low, size, null.prob, lower.tail = TRUE)
    prob.high <- stats::pbinom(q.high, size, null.prob, lower.tail = FALSE)

    q.low[prob.low > alpha / 2] <- q.low[prob.low > alpha / 2] - 1
    q.high[prob.high > alpha / 2] <- q.high[prob.high > alpha / 2] + 1

    prob.low <- stats::pbinom(q.low, size, null.prob, lower.tail = TRUE)
    prob.high <- stats::pbinom(q.high, size, null.prob, lower.tail = FALSE)
    approx.alpha <-  prob.low + prob.high

    binom.alpha <- c(q.low, q.high)

    power <- stats::pbinom(q.low, size, prob, lower.tail = TRUE) +
      stats::pbinom(q.high, size, prob, lower.tail = FALSE)

  } else if (alternative == "one.sided") {

    if (prob < null.prob) {
      # less
      q <- stats::qbinom(alpha, size, null.prob, lower.tail = TRUE)
      prob.alpha <- stats::pbinom(q, size, null.prob, lower.tail = TRUE)
      q[prob.alpha > alpha] <- q[prob.alpha > alpha] - 1
      approx.alpha <- stats::pbinom(q, size, null.prob, lower.tail = TRUE)

      binom.alpha <- q

      power <- stats::pbinom(q, size, prob, lower.tail = TRUE)

    } else {
      # greater
      q <- stats::qbinom(alpha, size, null.prob, lower.tail = FALSE)
      prob.alpha <- stats::pbinom(q, size, null.prob, lower.tail = FALSE)
      q[prob.alpha > alpha] <- q[prob.alpha > alpha] + 1
      approx.alpha <- stats::pbinom(q, size, null.prob, lower.tail = FALSE)

      binom.alpha <- q

      power <- stats::pbinom(q, size, prob, lower.tail = FALSE)

    }

  } else {

    stop("Incorrect `alternative` specification.", call. = FALSE)

  }


  if (plot) {

    if (length(size) > 1 || length(prob) > 1 || length(null.prob) > 2 || length(alpha) > 1)
      stop("Plotting is not available for multiple values", call. = FALSE)

    suppressWarnings({
      .plot.binom.t1t2(size = size, prob = prob, null.prob = null.prob,
                       alpha = approx.alpha, alternative = alternative)
    }) # supressWarnings

  }

  if (ensure_verbose(verbose) > 0) {

    print.obj <- list(test = "Generic Binomial Test",
                      requested = "power",
                      size = size,
                      alpha = approx.alpha,
                      alt = alternative,
                      prob.alternative = prob,
                      prob.null = null.prob,
                      binom.alpha = binom.alpha,
                      power = power)

    if (pretty) {
      .print.pwrss.binom(print.obj, verbose = verbose)
    } else {
      .print.ascii.pwrss.binom(print.obj, verbose = verbose)
    }

  } # end of verbose

  return(invisible(list(size = size,
                        alpha = approx.alpha,
                        alternative = alternative,
                        prob = prob,
                        null.prob = null.prob,
                        binom.alpha = binom.alpha,
                        power = power)))

} # power.binom.test()

power.binom <- power.binom.test
