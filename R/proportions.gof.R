####################################################
# Chi-square test for independence/goodness-of-fit #
####################################################

power.chisq.gof <- function(w, null.w = 0, df,
                            n = NULL, power = NULL, alpha = 0.05,
                            ceiling = TRUE, verbose = 1, pretty = FALSE) {

  func.parms <- clean.parms(as.list(environment()))
  verbose <- .ensure_verbose(verbose)

  check.positive(w, df)
  check.proportion(alpha)
  check.logical(ceiling)
  if (!is.null(power)) check.proportion(power)
  if (!is.null(n)) check.sample.size(n)

  if (w < null.w)
    stop("`w` should be greater than or equal to `null.w`.", call. = FALSE)

  ifelse(is.null(power),
         requested <- "power",
         requested <- "n")

  pwr.chisq <- function(w, null.w, df, n, alpha) {

    lambda <- n * w ^ 2
    null.lambda <- n * null.w ^ 2

    chisq.alpha <- qchisq(alpha, df = df, ncp = null.lambda, lower.tail = FALSE)
    power <- pchisq(chisq.alpha, df = df, ncp = lambda, lower.tail = FALSE)

    list(power = power,
         lambda = lambda,
         null.lambda = null.lambda,
         chisq.alpha = chisq.alpha)

  } # pwr.chisq


  ss.chisq <- function(w, null.w, df, power, alpha) {

    n <- try(silent = TRUE,
             suppressWarnings({
               uniroot(function(n) {
                 power - pwr.chisq(w = w, null.w = null.w,
                                   df = df, n = n, alpha = alpha)$power
               }, interval = c(2, 1e10))$root
             }) # supressWarnings

    ) # try

    if (inherits(n, "try-error") || n == 1e10)
      stop("Design is not feasible.", call. = FALSE)

    n

  } # ss.chisq


  if (is.null(power)) {

    pwr.obj <- pwr.chisq(w = w, null.w = null.w,
                       df = df, n = n, alpha = alpha)

    power <- pwr.obj$power
    ncp.alternative <- pwr.obj$lambda
    ncp.null <- pwr.obj$null.lambda
    chisq.alpha <- pwr.obj$chisq.alpha

  } else if (is.null(n)) {

    n <- ss.chisq(w = w, null.w = null.w,
                  df = df, power = power, alpha = alpha)

    if (ceiling) {
      n <- ceiling(n)
    }

    pwr.obj <- pwr.chisq(w = w, null.w = null.w,
                         df = df, n = n, alpha = alpha)

    power <- pwr.obj$power
    ncp.alternative <- pwr.obj$lambda
    ncp.null <- pwr.obj$null.lambda
    chisq.alpha <- pwr.obj$chisq.alpha

  }

  test <- "Chi-Square Test for Goodness-of-Fit or Independence"

  if (verbose > 0) {

    print.obj <- list(requested = requested,
                      test = test,
                      n = n,
                      df = df,
                      ncp.alternative = ncp.alternative,
                      ncp.null = ncp.null,
                      chisq.alpha = chisq.alpha,
                      alpha = alpha,
                      power = power)

    if (pretty) {
      .print.pwrss.gof(print.obj, verbose = verbose)
    } else {
      .print.ascii.pwrss.gof(print.obj, verbose = verbose)
    }

  }

  invisible(structure(list(parms = func.parms,
                           test = test,
                           df = df,
                           ncp = ncp.alternative,
                           null.ncp = ncp.null,
                           chisq.alpha = chisq.alpha,
                           power = power,
                           n = n),
                      class = c("pwrss", "chisq", "gof")))

} # end of power.chisq.gof()



pwrss.chisq.gofit <- function(p1 = c(0.50, 0.50),
                              p0 = probs.to.w(p1, verbose = 0)$null.prob.matrix,
                              w = probs.to.w(p1, p0, verbose = 0)$w,
                              df = probs.to.w(p1, p0, verbose = 0)$df,
                              n = NULL, power = NULL,
                              alpha = 0.05, verbose = TRUE) {

  verbose <- .ensure_verbose(verbose)
  user.parms.names <- names(as.list(match.call()))

  check.proportion(alpha)
  check.positive(w)
  check.sample.size(df)
  if (!is.null(power)) check.proportion(power)
  if (!is.null(n)) check.sample.size(n)

  if ("p1" %in% user.parms.names && "w" %in% user.parms.names)
    warning("Ignoring any specifications to `p1`, or `p0`.", call. = FALSE)
  if ("w" %in% user.parms.names && !("df" %in% user.parms.names))
    stop("Specify `df`.", call. = FALSE)
  if (is.vector(p1)) {
    if (length(p1) != length(p0))
      stop("Length of `p1` and `p0` should match.", call. = FALSE)
    if (sum(p1) != 1 || sum(p0) != 1)
      stop("Cell probabilities should sum to 1.", call. = FALSE)
  } else if (is.matrix(p1)) {
    if (any(dim(p1) != dim(p0)))
      stop("Dimensions of `p1` and `p0` differ.", call. = FALSE)
  } else {
    stop("Incorrect value for `p1`.", call. = FALSE)
  }

  gof.obj <- power.chisq.gof(w = w, null.w = 0, df = df,
                             n = n, power = power, alpha = alpha,
                             ceiling = TRUE, verbose = verbose)

  # cat("This function will be removed in the future. \n Please use power.chisq.gof() function. \n")

  return(invisible(gof.obj))

} # end of pwrss.chisq.gof()
