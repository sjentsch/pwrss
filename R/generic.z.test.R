#' Statistical Power for the Generic z-Test
#'
#' Calculates power for the generic z-Test with (optional) Type 1 and Type 2
#' error plots.
#'
#'
#' @param mean mean of the alternative.
#' @param sd standard deviation of the alternative. Do not change this value
#' except when some sort of variance correction is applied (e.g. as in logistic
#' and Poisson regressions).
#' @param null.mean mean of the null. When alternative = "two.one.sided", the
#' function expects two values in the form c(lower, upper). If a single value
#' is provided, it is interpreted as the absolute bound and automatically
#' expanded to c(-value, +value).
#' @param null.sd standard deviation of the null. Do not change this value
#' except when some sort of correction is applied.
#' @param alpha type 1 error rate, defined as the probability of incorrectly
#' rejecting a true null hypothesis, denoted as \eqn{\alpha}.
#' @param alternative character; direction or type of the hypothesis test:
#' "one.sided", "two.sided", or "two.one.sided". "two.one.sided" is used for
#' equivalence and minimal effect testing.
#' @param plot logical; \code{FALSE} switches off Type 1 and Type 2 error plot.
#' \code{TRUE} by default.
#' @param verbose \code{1} by default (returns test, hypotheses, and results),
#' if \code{2} a more detailed output is given (plus key parameters and
#' defintions), if \code{0} no output is printed on the console.
#' @param ... legacy inputs will be mapped to their corresponding arguments
#' (silent). e.g. \code{ncp}
#' @param pretty logical; whether the output should show Unicode characters (if
#' encoding allows for it). \code{FALSE} by default.
#'
#' @return \item{mean}{mean of the alternative distribution.}
#' \item{sd}{standard deviation of the alternative distribution.}
#' \item{null.mean}{mean of the null distribution.} \item{null.sd}{standard
#' deviation of the null distribution.} \item{z.alpha}{critical value(s).}
#' \item{power}{statistical power \eqn{(1-\beta)}.}
#'
#' @examples
#' # two-sided
#' # power defined as the probability of observing z-statistics
#' # greater than the positive critical t value OR
#' # less than the negative critical t value
#' power.z.test(mean = 1.96, alpha = 0.05,
#'              alternative = "two.sided")
#'
#' # one-sided
#' # power is defined as the probability of observing z-statistics
#' # greater than the critical t value
#' power.z.test(mean = 1.96, alpha = 0.05,
#'              alternative = "one.sided")
#'
#' # equivalence
#' # power is defined as the probability of observing a test statistic
#' # greater than the upper critical value (for the lower bound) AND
#' # less than the lower critical value (for the upper bound)
#' power.z.test(mean = 0, null.mean = c(-2, 2), alpha = 0.05,
#'              alternative = "two.one.sided")
#'
#' # minimal effect testing
#' # power is defined as the probability of observing a test statistic
#' # greater than the upper critical value (for the upper bound) OR
#' # less than the lower critical value (for the lower bound).
#' power.z.test(mean = 2, null.mean = c(-1, 1), alpha = 0.05,
#'              alternative = "two.one.sided")
#'
#' @export power.z.test
power.z.test <- function(mean = NULL, sd = 1, null.mean = 0, null.sd = 1,
                         alpha = 0.05, alternative = c("two.sided", "one.sided", "two.one.sided"),
                         plot = TRUE, verbose = 1, pretty = FALSE, ...) {

  old.args <- list(...) # just arguments in ...
  names.old.args <- names(old.args)

  if ("ncp" %in% names.old.args) {
    if (is.null(mean)) {
      check.numeric(old.args$ncp)
      mean <- old.args$ncp
    } else {
      stop("Both the new argument `mean` and the deprecated argument `ncp` were provided. Please specify only one.", call. = FALSE)
    }
  }

  check.proportion(alpha)
  check.positive(sd)

  alternative <- tolower(match.arg(alternative))

  # calculate statistical power
  if (alternative == "two.sided") {

    ifelse(is.numeric(mean) && length(mean) == 1,
           valid.mean <- TRUE,
           valid.mean <- FALSE)

    ifelse(is.numeric(null.mean) && length(null.mean) == 1,
           valid.null.mean <- TRUE,
           valid.null.mean <- FALSE)

    if (isFALSE(valid.mean) || isFALSE(valid.null.mean))
      stop("`mean` or `null.mean` must be numeric and of length one for the two-sided test.", call. = FALSE)

    # if (mean < null.mean) stop("`mean` must be equal or greater than `null.mean` for the two-sided test.", .call = FALSE)

    z.alpha.upper <- stats::qnorm(alpha / 2, mean = null.mean, sd = null.sd, lower.tail = FALSE)
    z.alpha.lower <- stats::qnorm(alpha / 2, mean = null.mean, sd = null.sd, lower.tail = TRUE)
    z.alpha <- c(z.alpha.lower, z.alpha.upper)

    power <-  stats::pnorm(z.alpha.lower, mean = mean, sd = sd, lower.tail = TRUE) +
      stats::pnorm(z.alpha.upper, mean = mean, sd = sd, lower.tail = FALSE)


  } else if (alternative == "one.sided") {

    ifelse(is.numeric(mean) && length(mean) == 1,
           valid.mean <- TRUE,
           valid.mean <- FALSE)

    ifelse(is.numeric(null.mean) && length(null.mean) == 1,
           valid.null.mean <- TRUE,
           valid.null.mean <- FALSE)

    if (isFALSE(valid.mean) || isFALSE(valid.null.mean))
      stop("`mean` or `null.mean` must be numeric and of length one for the one-sided test.", call. = FALSE)
    # if (any(mean < null.mean) && alternative == "greater")
    #   stop("`alternative` = 'greater' but `mean` < `null.mean`.", call. = FALSE)
    # if (any(mean > null.mean) && alternative == "less")
    #   stop("`alternative` = 'less' but `mean` > `null.mean.", call. = FALSE)

    ifelse(mean > null.mean,
           lower.tail <- FALSE,
           lower.tail <- TRUE)
    z.alpha <- stats::qnorm(alpha, mean = null.mean, sd = null.sd, lower.tail = lower.tail) # if mean > null.mean
    power <- stats::pnorm(z.alpha, mean = mean, sd = sd, lower.tail = lower.tail) # if mean > null.mean

  } else if (alternative == "two.one.sided") {

    ifelse(is.numeric(mean) && length(mean) == 1,
           valid.mean <- TRUE,
           valid.mean <- FALSE)

    ifelse(is.numeric(null.mean) && length(null.mean) %in% c(1, 2),
           valid.null.mean <- TRUE,
           valid.null.mean <- FALSE)

    if (isFALSE(valid.mean))
      stop("`mean` must be numeric and of length one for equivalence tests.", call. = FALSE)
    if (isFALSE(valid.null.mean))
      stop(paste("`null.mean` must be numeric and of length one (absolute value) or length two (with lower and upper",
                 "bounds) for the equivalence test."), call. = FALSE)

    if (length(null.mean) == 1) null.mean <- c(min(c(-null.mean, null.mean)), max(-null.mean, null.mean))

    # equivalence test
    if (mean > min(null.mean) && mean < max(null.mean)) {

      z.alpha.upper <- stats::qnorm(alpha, mean = min(null.mean), sd = null.sd, lower.tail = FALSE)
      z.alpha.lower <- stats::qnorm(alpha, mean = max(null.mean), sd = null.sd, lower.tail = TRUE)
      z.alpha <- c(z.alpha.upper, z.alpha.lower)

      power <- stats::pnorm(z.alpha.lower, mean = mean, sd = sd, lower.tail = TRUE) +
        stats::pnorm(z.alpha.upper, mean = mean, sd = sd, lower.tail = FALSE) - 1

      power[power < 0] <- 0

    }

    # minimum effect test
    if (mean < min(null.mean) || mean > max(null.mean)) {

      z.alpha.lower <- stats::qnorm(alpha / 2, mean = min(null.mean), sd = null.sd, lower.tail = TRUE)
      z.alpha.upper <- stats::qnorm(alpha / 2, mean = max(null.mean), sd = null.sd, lower.tail = FALSE)
      z.alpha <- c(z.alpha.lower, z.alpha.upper)

      power <- stats::pnorm(z.alpha.lower, mean = mean, sd = sd, lower.tail = TRUE) +
        stats::pnorm(z.alpha.upper, mean = mean, sd = sd, lower.tail = FALSE)

    }

  } else {

    stop("Not a valid hypothesis type.", call. = FALSE)

  }

  if (plot) {
    # if (sd != 1 || null.sd != 1)
    #   stop("Plotting is not available when the standard deviation of the standard normal distribution deviates from one.", call. = FALSE)

    try(silent = TRUE,
        suppressWarnings({
          .plot.t.t1t2(ncp = mean, null.ncp = null.mean, df = Inf, alpha = alpha, alternative = alternative)
        }) # supressWarnings
    ) # try

  }

  if (ensure_verbose(verbose) > 0) {

    print.obj <- list(test = "Generic Z-Test",
                      requested = "power",
                      alt = alternative,
                      mean.alternative = mean,
                      sd.alternative = sd,
                      mean.null = null.mean,
                      sd.null = null.sd,
                      alpha = alpha,
                      z.alpha = z.alpha,
                      power = power)

    if (pretty) {
      .print.pwrss.z(print.obj, verbose = verbose)
    } else {
      .print.ascii.pwrss.z(print.obj, verbose = verbose)
    }

  } # verbose


  return(invisible(list(alternative = alternative,
                        mean = mean,
                        sd = sd,
                        null.mean = null.mean,
                        null.sd = null.sd,
                        alpha = alpha,
                        z.alpha = z.alpha,
                        power = power)))

} # end of power.z.test()

power.z <- power.z.test
