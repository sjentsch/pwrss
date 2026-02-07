#' Statistical Power for the Generic Chi-Square Test
#'
#' Calculates power for the generic chi-square test with (optional) Type 1 and
#' Type 2 error plots.
#'
#'
#' @param ncp      non-centrality parameter for the alternative.
#' @param null.ncp non-centrality parameter for the null.
#' @param df       integer; degrees of freedom. For example, for the test of
#'                 independence df = (nrow - 1)*(ncol - 1).
#' @param alpha    type 1 error rate, defined as the probability of incorrectly
#'                 rejecting a true null hypothesis, denoted as \eqn{\alpha}.
#' @param plot     logical; \code{FALSE} switches off Type 1 and Type 2 error
#'                 plot. \code{TRUE} by default.
#' @param verbose  \code{1} by default (returns test, hypotheses, and results),
#'                 if \code{2} a more detailed output is given (plus key
#'                 parameters and defintions), if \code{0} no output is printed
#'                 on the console.
#' @param pretty   logical; whether the output should show Unicode characters
#'                 (if encoding allows for it). \code{FALSE} by default.
#'
#' @return
#'   \item{power}{statistical power \eqn{(1-\beta)}.}
#'
#' @examples
#' # power is defined as the probability of observing Chi-square-statistics
#' # greater than the critical  value
#' power.chisq.test(ncp = 20, df = 100, alpha = 0.05)
#'
#' @export power.chisq.test
power.chisq.test <- function(ncp, null.ncp = 0, df, alpha = 0.05,
                             plot = TRUE, verbose = 1, pretty = FALSE) {

  check.positive(df)
  check.proportion(alpha)

  ifelse(is.numeric(ncp) || length(ncp) == 1 || ncp >= 0,
         valid.ncp <- TRUE,
         valid.ncp <- FALSE)

  ifelse(is.numeric(null.ncp) || length(null.ncp) == 1 || all(null.ncp >= 0),
         valid.null.ncp <- TRUE,
         valid.null.ncp <- FALSE)

  if (isFALSE(valid.ncp) || isFALSE(valid.null.ncp))
    stop("`ncp` or `null.ncp` must be numeric, positive, and of length one.", call. = FALSE)
  if (ncp < null.ncp)
    stop("`ncp` should be greater than or equal to `null.ncp`.", call. = FALSE)

  chisq.alpha <- stats::qchisq(alpha, df = df, ncp = null.ncp, lower.tail = FALSE)

  power <- stats::pchisq(chisq.alpha, df = df, ncp = ncp, lower.tail = FALSE)

  if (plot) {

    .plot.chisq.t1t2(ncp = ncp, null.ncp = null.ncp, df = df, alpha = alpha)

  }

  if (ensure_verbose(verbose) > 0) {

    print.obj <- list(test = "Generic Chi-square Test",
                      requested = "power",
                      power = power, ncp.alternative = ncp, ncp.null = null.ncp,
                      alpha = alpha, chisq.alpha = chisq.alpha, df = df)

    if (pretty) {
      .print.pwrss.chisq(print.obj, verbose = verbose)
    } else {
      .print.ascii.pwrss.chisq(print.obj, verbose = verbose)
    }

  } # end of verbose

  return(invisible(list(power = power, ncp = ncp, null.ncp = null.ncp,
                        alpha = alpha, df = df, chisq.crit = chisq.alpha)))

} # end of power.chisq.test()

power.chisq <- power.chisq.test
