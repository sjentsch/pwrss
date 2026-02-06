# internal check functions
check.proportion <- function(...) {

  args <- list(...)
  arg.names <- vapply(substitute(list(...))[-1], function(fn) paste0("`", deparse(fn, nlines = 1), "`"), character(1))

  check <- vapply(args, function(x) length(x) == 1 && is.numeric(x) && x >= 0 && x <= 1, logical(1))

  if (any(!check))
    stop(format_errmsg(arg.names[!check], as.character(match.call()[[1]])), call. = FALSE)

} # check.probs()

check.correlation <- function(...) {

  args <- list(...)
  arg.names <- vapply(substitute(list(...))[-1], function(fn) paste0("`", deparse(fn, nlines = 1), "`"), character(1))

  check <- vapply(args, function(x) length(x) == 1 && is.numeric(x) && x >= -1 && x <= 1, logical(1))

  if (any(!check))
    stop(format_errmsg(arg.names[!check], as.character(match.call()[[1]])), call. = FALSE)

} # check.correlations()

check.logical <- function(...) {

  args <- list(...)
  arg.names <- vapply(substitute(list(...))[-1], function(fn) paste0("`", deparse(fn, nlines = 1), "`"), character(1))

  check <- vapply(args, function(x) length(x) == 1 && is.logical(x) && !is.na(x), logical(1))

  if (any(!check))
    stop(format_errmsg(arg.names[!check], as.character(match.call()[[1]])), call. = FALSE)

} # check.logical()


check.sample.size <- function(...) {

  args <- list(...)
  arg.names <- vapply(substitute(list(...))[-1], function(fn) paste0("`", deparse(fn, nlines = 1), "`"), character(1))

  check <- vapply(args,
                  function(x) length(x) == 1 && is.numeric(x) && is.finite(x) && x > 1 && abs(x - round(x)) < .Machine$double.eps ^ 0.5,
                  logical(1))

  if (any(!check))
    stop(format_errmsg(arg.names[!check], as.character(match.call()[[1]])), call. = FALSE)

} # check.sample.size

check.factor.level <- check.sample.size # check.factor.level

check.nonnegative <- function(...) {

  args <- list(...)
  arg.names <- vapply(substitute(list(...))[-1], function(fn) paste0("`", deparse(fn, nlines = 1), "`"), character(1))

  check <- vapply(args, function(x) length(x) == 1 && is.numeric(x) && is.finite(x) && x >= 0, logical(1))

  if (any(!check))
    stop(format_errmsg(arg.names[!check], as.character(match.call()[[1]])), call. = FALSE)

} # check.nonnegative

check.positive <- function(...) {

  args <- list(...)
  arg.names <- vapply(substitute(list(...))[-1], function(fn) paste0("`", deparse(fn, nlines = 1), "`"), character(1))

  check <- vapply(args, function(x) length(x) == 1 && is.numeric(x) && is.finite(x) && x > 0, logical(1))

  if (any(!check))
    stop(format_errmsg(arg.names[!check], as.character(match.call()[[1]])), call. = FALSE)

} # check.positive

check.numeric <- function(...) {

  args <- list(...)
  arg.names <- vapply(substitute(list(...))[-1], function(fn) paste0("`", deparse(fn, nlines = 1), "`"), character(1))

  check <- vapply(args, function(x) length(x) == 1 && is.numeric(x) && is.finite(x), logical(1))

  if (any(!check))
    stop(format_errmsg(arg.names[!check], as.character(match.call()[[1]])), call. = FALSE)

} # check.positive

check.vector <- function(x, fnc, min.length = 2) {

  var.name <- deparse(substitute(x), nlines = 1)
  if (!is.vector(x) || length(x) < min.length)
    stop(sprintf("`%s` neeeds to be a vector with a length of at least %d.", var.name, min.length), call. = FALSE)

  fnc.name <- deparse(substitute(fnc), nlines = 1)
  err.msg <- sprintf("All elements of `%s` need to be valid %s values (%s)", var.name, fnc2type(fnc.name), valid.cond(fnc.name))
  tryCatch(invisible(sapply(x, fnc)),
           error = function(e) stop(err.msg, call. = FALSE))
  
  invisible(NULL)
} # check.vector

check.same.lengths <- function(...) {

  args <- list(...)
  arg.names <- vapply(substitute(list(...))[-1], function(fn) paste0("`", deparse(fn, nlines = 1), "`"), character(1))
  if (is.null(args[[1]]))
    stop(sprintf("To use `check.same.lengths`, %s can not be NULL.", arg.names[[1]]), call. = FALSE)
  
  check <- rep(TRUE, length(args))
  check[c(FALSE, !unlist(lapply(args, is.null))[-1])] <-
    unlist(lapply(args, length)[c(FALSE, !unlist(lapply(args, is.null))[-1])]) == length(args[[1]])

  if (any(!check))
    stop(
      sprintf("%s should have the same length as %s.",
              paste(arg.names[!check], collapse = ifelse(sum(!check) == 2, " and ", ", ")),
              arg.names[1]),
      call. = FALSE
    )

} # check.same.lengths

check.correlation.matrix <- function(x) {

  var.name <- deparse(substitute(x), nlines = 1)

  is.square <- nrow(x) == ncol(x)
  if (!is.square)
    stop(sprintf("Correlation matrix `%s` is not square", var.name), call. = FALSE)

  is.symmetric <- isSymmetric.matrix(x)
  if (!is.symmetric)
    stop(sprintf("Correlation matrix `%s` is not symmetric", var.name), call. = FALSE)

  # check that all values (only the upper triangular, as the matrix is symmetric) are valid correlations,
  # either all values are valid or an error is thrown
  errmsg <- sprintf("The values in the correlation matrix (`%s`) must be numeric, >= -1 and <= 1", var.name)
  tryCatch(invisible(sapply(x[upper.tri(x)], check.correlation)), error = function(e) stop(errmsg, call. = FALSE))

  correct.diagonal <- all(diag(x) == 1)
  if (!correct.diagonal)
    stop(sprintf("All values in the main diagonal of the correlation matrix (`%s`) must be 1", var.name), call. = FALSE)

  is.positive.definite <- all(eigen(x, symmetric = TRUE)$values > 0)
  if (!is.positive.definite)
    stop(sprintf("Correlation matrix `%s` is not positive definite", var.name), call. = FALSE)

  is.invertible <- det(x) > 0
  if (!is.invertible)
    stop(sprintf("Correlation matrix `%s` is not invertible", var.name), call. = FALSE)

  is.well.conditioned <- kappa(x) < 1000
  if (!is.well.conditioned)
    stop(sprintf("Correlation matrix `%s` is not well-conditioned", var.name), call. = FALSE)

} # check.correlation.matrix()

check.n_power <- function(n = NULL, power = NULL) {

  n.name <- deparse(substitute(n), nlines = 1)

  if (is.null(n) && is.null(power))
    stop(sprintf("`%s` and `power` cannot be NULL at the same time.", n.name), call. = FALSE)

  if (!is.null(n) && !is.null(power))
    stop(sprintf("Exactly / only one of the parameters `%s` or `power` should be NULL.", n.name), call. = FALSE)

  invisible(ifelse(is.null(power), "power", "n")) # return what is requested / to be calculated

} # check.power_n

# helper function(s) ---------------------------------------------------------------------------------------------------
format_errmsg <- function(names = c(), fnc.name = NULL) {
  sprintf("Argument%s %s %s not have %svalid %s value%s (must be length 1, %s)",
          ifelse(length(names) > 1, "s", ""),
          paste(names, collapse = ifelse(length(names) > 2, ", ", " and ")),
          ifelse(length(names) > 1, "do", "does"),
          ifelse(length(names) > 1, "", "a "),
          fnc2type(fnc.name),
          ifelse(length(names) > 1, "s", ""),
          valid.cond(fnc.name))
}

fnc2type <- function(fnc.name = "") {
  gsub("factor.level", "factor level", gsub("sample.size", "sample size", gsub("nonnegative", "non-negative", gsub("check.", "", fnc.name))))
}

valid.cond <- function(fnc.name = "") {
  if (fnc.name == "check.proportion") {
    "numeric, >= 0, and <= 1"
  } else if (fnc.name == "check.correlation") {
    "numeric, >= -1, and <= 1"
  } else if (fnc.name == "check.logical") {
    "TRUE or FALSE"
  } else if (fnc.name %in% c("check.sample.size", "check.factor.level")) {
    "integer-like, > 1, and finite"
  } else if (fnc.name == "check.nonnegative") {
    "numeric, >= 0, and finite"
  } else if (fnc.name == "check.positive") {
    "numeric, > 0, and finite"
  } else if (fnc.name == "check.numeric") {
    "numeric, and finite"
  } else {
    stop(sprintf("% is not a valid check-function", fnc.name), call. = FALSE)
  }
}

# check.logical(correct, paired)
# check.proportions(p1, p2, alpha, r2, eta2)
# check.sample.size(n2, n2)
# check.nonnegative(sd1, sd2, f2)
# check.positive(k.covariates)
# check.numeric(mu1, mu2, beta0, beta1)
