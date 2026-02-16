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

save_png <- function(code, width = 800, height = 800) {
  path <- tempfile(fileext = ".png")
  png(path, width = width, height = height)
  on.exit(dev.off())
  code

  path
}
