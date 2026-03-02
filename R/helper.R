# ensure valid values for verbose: logical values are converted to 0/1, numerical values are kept as long as they
# have the length 1, are integer and have the value 0, 1 or 2, in any other case, 1 (the default) is returned
ensure.verbose <- function(verbose = NULL) {
  if (is.logical(verbose)) {
    as.integer(verbose)
  } else if (is.numeric(verbose)) {
    ifelse(length(verbose) == 1 && verbose %% 1 == 0 && verbose %in% c(-1, 0, 1, 2), verbose, 1)
  } else {
    1
  }
}

# check the input parameters n, power and es, and return which calculation is requested
get.requested <- function(es = NULL, n = NULL, power = NULL) {

  if (is.null(es) || !is.na(es)) {
    parms <- sprintf("two of the parameters `%s`, `%s`, or `power`", deparse(substitute(es), n = 1), deparse(substitute(n), n = 1))
  } else {
    parms <- sprintf("one of the parameters `%s` or `power`", deparse(substitute(n), n = 1))
  }

  if (sum(check.not_null(n, power, es)) != 2)
    stop(sprintf("Exactly %s must be given, one has to be NULL.", parms), call. = FALSE)

  invisible(c("es", "n", "power")[check.null(es, n, power)]) # return what is requested / to be calculated

} # get.requested

isInt <- function(x) is.numeric(x) && !any(abs(x - round(x)) > .Machine$double.eps ^ 2 / 3)

# lenInt <- function(n) ifelse(n <= 1, 1, ceiling(log10(abs(n))) + as.integer(n %% 10 == 0))

check.snap4plot <- function(snpFle = "", pltFnc = NULL, pltPrm = list(), pltWdt = 800, pltHgh = 800) {
  if (nchar(Sys.getenv("GITHUB_ACTIONS")) == 0) { # ensures that the code only runs on a local machine, not as GitHub action
    tmpFle <- tempfile(fileext = ".png")
    addPrm <- list(power = 0.80, alpha = 0.05, verbose = 0)[c("power", "alpha", "verbose") %in% names(formals(pltFnc))]
    testthat::announce_snapshot_file(name = snpFle)
    grDevices::png(tmpFle, width = pltWdt, height = pltHgh)
    if (any(c("plot", "plot.main") %in% names(formals(pltFnc)))) {
      do.call(pltFnc, c(pltPrm, addPrm))
    } else {
      plot(do.call(pltFnc, c(pltPrm, addPrm)))
    }
    grDevices::dev.off()
    testthat::expect_snapshot_file(path = tmpFle, name = snpFle, variant = Sys.info()[["sysname"]])
    unlink(tmpFle)
  }
}
