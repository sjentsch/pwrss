if(getRversion() >= "2.15.1")
  utils::globalVariables(c("crtSnp"))


clean.parms <- function(func.parms = list()) {
  func.parms[names(func.parms)[!grepl("^power$|^n$|^n2$|^n.paired$|^n.total$|^n.vector$", names(func.parms))]]
}

# ensure valid values for verbose: logical values are converted to 0/1, numerical values are kept as long as they
# have the length 1, are integer and have the value 0, 1 or 2, in any other case, 1 (the default) is returned
ensure_verbose <- function(verbose = NULL) {
  if (is.logical(verbose)) {
    as.integer(verbose)
  } else if (is.numeric(verbose)) {
    ifelse(length(verbose) == 1 && verbose %% 1 == 0 && verbose %in% c(-1, 0, 1, 2), verbose, 1)
  } else {
    1
  }
}

isInt <- function(x) is.numeric(x) && !any(abs(x - round(x)) > .Machine$double.eps ^ 2 / 3)

# lenInt <- function(n) ifelse(n <= 1, 1, ceiling(log10(abs(n))) + as.integer(n %% 10 == 0))

check.snap4plot <- function(snpFle = "", pltFnc = NULL, pltPrm = list(), pltWdt = 800, pltHgh = 800) {
  if (ifelse(exists("crtSnp"), crtSnp, FALSE) || file.exists(file.path("_snaps", Sys.info()[["sysname"]], "plots", snpFle))) {
    tmpFle <- tempfile(fileext = ".png")
    addPrm <- list(power = 0.80, alpha = 0.05, verbose = 0)[c("power", "alpha", "verbose") %in% names(formals(pltFnc))]
    grDevices::png(tmpFle, width = pltWdt, height = pltHgh)
    if (any(c("plot", "plot.main") %in% names(formals(pltFnc)))) {
      do.call(pltFnc, c(pltPrm, addPrm))
    } else {
      plot(do.call(pltFnc, c(pltPrm, addPrm)))
    }
    grDevices::dev.off()
    testthat::expect_snapshot_file(tmpFle, snpFle, variant = Sys.info()[["sysname"]])
    unlink(tmpFle)
  }
}
