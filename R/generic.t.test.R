#' Statistical Power for the Generic t-Test
#'
#' Calculates power for the generic t-Test with (optional) Type 1 and Type 2
#' error plots.
#'
#'
#' @aliases power.t
#'
#' @param             ncp non-centrality parameter for the alternative.
#' @param null.ncp    non-centrality parameter for the null. When alternative =
#'                    "two.one.sided", the function expects two values in the
#'                    form `c(lower, upper)`. If a single value is provided, it
#'                    is interpreted as the absolute bound and automatically
#'                    expanded to `c(-value, +value)`.
#' @param df          degrees of freedom.
#' @param alpha       type 1 error rate, defined as the probability of
#'                    incorrectly rejecting a true null hypothesis, denoted as
#'                    \eqn{\alpha}.
#' @param alternative character; the direction or type of the hypothesis test:
#'                    "two.sided", "one.sided", or "two.one.sided".
#'                    "two.one.sided" is used for equivalence and minimal
#'                    effect testing.
#' @param plot        logical; \code{FALSE} switches off Type 1 and Type 2
#'                    error plot. \code{TRUE} by default.
#' @param verbose     \code{1} by default (returns test, hypotheses, and
#'                    results), if \code{0} no output is printed on the
#'                    console.
#' @param pretty      logical; whether the output should show Unicode
#'                    characters (if encoding allows for it). \code{FALSE} by
#'                    default.
#'
#' @return
#'   \item{df}{degrees of freedom.}
#'   \item{ncp}{non-centrality parameter under alternative.}
#'   \item{ncp.null}{non-centrality parameter under null.}
#'   \item{t.alpha}{critical value(s).}
#'   \item{power}{statistical power \eqn{(1-\beta)}.}
#'
#' @examples
#' # two-sided
#' # power defined as the probability of observing test statistics greater
#' # than the positive critical value OR less than the negative critical value
#' power.t.test(ncp = 1.96, df = 100, alpha = 0.05, alternative = "two.sided")
#'
#' # one-sided
#' # power is defined as the probability of observing a test statistic greater
#' # than the critical value
#' power.t.test(ncp = 1.96, df = 100, alpha = 0.05, alternative = "one.sided")
#'
#' # equivalence
#' # power is defined as the probability of observing a test statistic greater
#' # than the upper critical value (for the lower bound) AND less than the
#' # lower critical value (for the upper bound)
#' power.t.test(ncp = 0, null.ncp = c(-2, 2), df = 100, alpha = 0.05,
#'              alternative = "two.one.sided")
#'
#' # minimal effect testing
#' # power is defined as the probability of observing a test statistic greater
#' # than the upper critical value (for the upper bound) OR less than the lower
#' # critical value (for the lower bound).
#' power.t.test(ncp = 2, null.ncp = c(-1, 1), df = 100, alpha = 0.05,
#'              alternative = "two.one.sided")
#'
#' @export power.t.test
power.t.test <- function(ncp, null.ncp = 0,
                         df, alpha = 0.05,
                         alternative = c("two.sided", "one.sided", "two.one.sided"),
                         plot = TRUE, verbose = 1, pretty = FALSE) {

  check.positive(df)
  check.proportion(alpha)

  alternative <- tolower(match.arg(alternative))

  # calculate statistical power
  if (alternative == "two.sided") {

    ifelse(is.numeric(ncp) && length(ncp) == 1,
           valid.ncp <- TRUE,
           valid.ncp <- FALSE)

    ifelse(is.numeric(null.ncp) && length(null.ncp) == 1,
           valid.null.ncp <- TRUE,
           valid.null.ncp <- FALSE)

    if (isFALSE(valid.ncp) || isFALSE(valid.null.ncp))
      stop("`ncp` or `null.ncp` must be numeric and of length one for the two-sided test.", call. = FALSE)
    # if (ncp < null.ncp) stop("`ncp` must be equal or greater than `null.ncp` for the two-sided test.", .call = FALSE)

    t.alpha.upper <- stats::qt(alpha / 2, df = df, ncp = null.ncp, lower.tail = FALSE)
    t.alpha.lower <- stats::qt(alpha / 2, df = df, ncp = null.ncp, lower.tail = TRUE)
    t.alpha <- c(t.alpha.lower, t.alpha.upper)
    power <-  stats::pt(t.alpha.lower, df = df, ncp = ncp, lower.tail = TRUE) +
      stats::pt(t.alpha.upper, df = df, ncp = ncp, lower.tail = FALSE)

  } else if (alternative == "one.sided") {

    ifelse(is.numeric(ncp) || length(ncp) == 1,
           valid.ncp <- TRUE,
           valid.ncp <- FALSE)

    ifelse(is.numeric(null.ncp) || length(null.ncp) == 1,
           valid.null.ncp <- TRUE,
           valid.null.ncp <- FALSE)

    if (isFALSE(valid.ncp) || isFALSE(valid.null.ncp))
      stop("`ncp` or `null.ncp` must be numeric and of length one for the one-sided test.", call. = FALSE)
    # if (any(ncp < null.ncp) && alternative == "greater") stop("`alternative` = 'greater' but `ncp` < `null.ncp`.", call. = FALSE)
    # if (any(ncp > null.ncp) && alternative == "less") stop("`alternative` = 'less' but `ncp` > `null.ncp`.", call. = FALSE)

    ifelse(ncp > null.ncp,
           lower.tail <- FALSE,
           lower.tail <- TRUE)
    t.alpha <- stats::qt(alpha, df = df, ncp = null.ncp, lower.tail = lower.tail) # if ncp > null.ncp
    power <- stats::pt(t.alpha, df = df, ncp = ncp, lower.tail = lower.tail) # if ncp > null.ncp

  } else if (alternative == "two.one.sided") {

    ifelse(is.numeric(ncp) && length(ncp) == 1,
           valid.ncp <- TRUE,
           valid.ncp <- FALSE)

    ifelse(is.numeric(null.ncp) && length(null.ncp) %in% c(1, 2),
           valid.null.ncp <- TRUE,
           valid.null.ncp <- FALSE)

    if (isFALSE(valid.ncp)) stop("`ncp` must be numeric and of length one for equivalence tests.", call. = FALSE)
    if (isFALSE(valid.null.ncp))
      stop(paste("`null.ncp` must be numeric and of length one (absolute value) or length two (with lower and upper",
                 "bounds) for the equivalence test."), call. = FALSE)

    if (length(null.ncp) == 1) null.ncp <- c(min(c(-null.ncp, null.ncp)), max(-null.ncp, null.ncp))

    # equivalence test
    if (ncp > min(null.ncp) && ncp < max(null.ncp)) {

      t.alpha.upper <- stats::qt(alpha, df = df, ncp = min(null.ncp), lower.tail = FALSE)
      t.alpha.lower <- stats::qt(alpha, df = df, ncp = max(null.ncp), lower.tail = TRUE)
      t.alpha <- c(t.alpha.upper, t.alpha.lower)

      power <- stats::pt(t.alpha.lower, df = df, ncp = ncp, lower.tail = TRUE) +
        stats::pt(t.alpha.upper, df = df, ncp = ncp, lower.tail = FALSE) - 1

      power[power < 0] <- 0

    }

    # minimum effect test
    if (ncp < min(null.ncp) || ncp > max(null.ncp)) {

      t.alpha.lower <- stats::qt(alpha / 2, df = df, ncp = min(null.ncp), lower.tail = TRUE)
      t.alpha.upper <- stats::qt(alpha / 2, df = df, ncp = max(null.ncp), lower.tail = FALSE)
      t.alpha <- c(t.alpha.lower, t.alpha.upper)

      power <- stats::pt(t.alpha.lower, df = df, ncp = ncp, lower.tail = TRUE) +
        stats::pt(t.alpha.upper, df = df, ncp = ncp, lower.tail = FALSE)

    }

  } else {

    stop("Not a valid hypothesis type.", call. = FALSE)

  }

  if (plot) {

    suppressWarnings({
      .plot.t.t1t2(ncp = ncp, null.ncp = null.ncp, df = df, alpha = alpha, alternative = alternative)
    }) # supressWarnings

  }

  if (ensure_verbose(verbose) > 0) {

    print.obj <- list(test = "Generic T-Test",
                      requested = "power",
                      alt = alternative,
                      ncp.alternative = ncp,
                      ncp.null = null.ncp,
                      t.alpha = t.alpha,
                      df = df, alpha = alpha,
                      power = power)

    if (pretty) {
      .print.pwrss.t(print.obj, verbose = verbose)
    } else {
      .print.ascii.pwrss.t(print.obj, verbose = verbose)
    }

  } # verbose


  return(invisible(list(alternative = alternative, ncp = ncp, null.ncp = null.ncp,
                        df = df, alpha = alpha, t.alpha = t.alpha, power = power)))

} # end of power.t.test()

power.t <- power.t.test
