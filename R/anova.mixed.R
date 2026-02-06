power.f.mixed.anova <- function(eta.squared,
                                null.eta.squared = 0,
                                factor.levels = c(2, 2),
                                factor.type = c("between", "within"),
                                rho.within = 0.50,
                                epsilon = 1,
                                n.total = NULL,
                                power = NULL, alpha = 0.05,
                                effect = c("between", "within", "interaction"),
                                ceiling = TRUE, verbose = 1, pretty = FALSE) {

  effect <- tolower(match.arg(effect))
  func.parms <- clean.parms(as.list(environment()))

  check.nonnegative(eta.squared, null.eta.squared)
  if (!is.null(n.total)) check.sample.size(n.total)
  if (!is.null(power)) check.proportion(power)
  check.proportion(alpha)
  check.logical(ceiling, pretty)
  verbose <- ensure_verbose(verbose)
  requested <- check.n_power(n.total, power)

  for (i in 1:2) {
    factor.type.check <- factor.type[i]
    if (!is.character(factor.type.check) || !(factor.type.check %in% c("between", "within")))
      stop(paste("The `factor.type` argument must be specified as either c('between', 'within') or c('within', 'between'),",
                 "indicating the order in which the corresponding values in `factor.levels` are interpreted - specifically,",
                 "which factor is treated as between-subjects and which as within-subjects."), call. = FALSE)
  }

  if (length(factor.levels) != 2 || length(factor.type) != 2)
    stop("Excatly two factors are allowed in this procedure.", call. = FALSE)
  if (all(factor.type == "within") || all(factor.type == "between"))
    stop("Two `within` or two `between` factors are not allowed in this procedure.", call. = FALSE)

  n.levels.between <- factor.levels[which(tolower(factor.type) == "between")]
  n.levels.within <- factor.levels[which(tolower(factor.type) == "within")]
  if (n.levels.within > 1 && epsilon <  1 / (n.levels.within - 1))
    stop("Incorrect value for the non-sphericity correction factor (epsilon).", call. = FALSE)

  f.squared <- eta.squared / (1 - eta.squared)
  null.f.squared <- null.eta.squared / (1 - null.eta.squared)

  if (!is.na(rho.within)) {

    if (effect == "between") {
      f.squared <- f.squared  * (n.levels.within / (1 + (n.levels.within - 1) * rho.within))
      null.f.squared <- null.f.squared  * (n.levels.within / (1 + (n.levels.within - 1) * rho.within))
    } else {
      f.squared <- f.squared  * (n.levels.within / (1 - rho.within))
      null.f.squared <- null.f.squared  * (n.levels.within / (1 - rho.within))
    }

  } else {

    warning("Assuming that `eta.squared` and `null.eta.squared` are already adjusted for within-subject correlation.", call. = FALSE)

  }

  ss.mixed <- function(f.squared, null.f.squared, n.levels.between, n.levels.within, epsilon, alpha, power, effect) {

    n.min <- n.levels.between + 1

    n.total <- try(silent = TRUE,
                   stats::uniroot(function(n.total) {
                     if (effect == "between") {
                       df1 <- n.levels.between - 1
                       df2 <- n.total - n.levels.between
                     } else if (effect == "within") {
                       df1 <- (n.levels.within - 1) * epsilon
                       df2 <- (n.total - n.levels.between) * (n.levels.within - 1) * epsilon
                     } else if (effect == "interaction") {
                       df1 <- (n.levels.between - 1) * (n.levels.within - 1) * epsilon
                       df2 <- (n.total - n.levels.between) * (n.levels.within - 1) * epsilon
                     } else {
                       stop("Unknown effect", call. = FALSE)
                     }
                     u <- df1
                     v <- df2
                     lambda <- f.squared * n.total * epsilon
                     null.lambda <- null.f.squared * n.total * epsilon
                     f.alpha <- stats::qf(alpha, df1 = u, df2 = v, ncp = null.lambda, lower.tail = FALSE)

                     power - stats::pf(f.alpha, df1 = u, df2 = v, ncp = lambda, lower.tail = FALSE)

                   }, interval = c(n.min, 1e10))$root
                   ) # try

    if (inherits(n.total, "try-error") || n.total == 1e+10) stop("Design is not feasible.", call. = FALSE)

    n.total

  }

  pwr.mixed <- function(f.squared, null.f.squared, n.total,
                        n.levels.between, n.levels.within, epsilon, alpha, effect) {

    if (effect == "between") {
      df1 <- n.levels.between - 1
      df2 <- n.total - n.levels.between
    } else if (effect == "within") {
      df1 <- (n.levels.within - 1) * epsilon
      df2 <- (n.total - n.levels.between) * df1
    } else if (effect == "interaction") {
      df1 <- (n.levels.between - 1) * (n.levels.within - 1) * epsilon
      df2 <- (n.total - n.levels.between) * (n.levels.within - 1) * epsilon
    } else {
      stop("Unknown effect", call. = FALSE)
    }

    u <- df1
    v <- df2
    if (u < 1 || v < 1) stop("Design is not feasible", call. = FALSE)
    lambda <- f.squared * n.total * epsilon
    null.lambda <- null.f.squared * n.total * epsilon
    f.alpha <- stats::qf(alpha, df1 = u, df2 = v, ncp = null.lambda, lower.tail = FALSE)
    power <- stats::pf(f.alpha, df1 = u, df2 = v, ncp = lambda, lower.tail = FALSE)

    list(power = power, lambda = lambda, null.lambda = null.lambda,
         df1 = u, df2 = v, f.alpha = f.alpha)

  } # pwr.mixed()

  if (requested == "n") {

    n.total <- ss.mixed(f.squared = f.squared, null.f.squared = null.f.squared,
                        n.levels.between = n.levels.between, n.levels.within = n.levels.within,
                        epsilon = epsilon, alpha = alpha, power = power, effect = effect)

    if (ceiling) n.total <- ceiling(n.total / n.levels.between) * n.levels.between

  }

  # calculate power (if requested == "power") or update it (if requested == "n")
  pwr.obj <- pwr.mixed(f.squared = f.squared, null.f.squared = null.f.squared,
                       n.total = n.total, n.levels.between = n.levels.between, n.levels.within = n.levels.within,
                       epsilon = epsilon, alpha = alpha, effect = effect)

  power <- ncp <- pwr.obj$power
  df1 <-  pwr.obj$df1
  df2 <-  pwr.obj$df2
  ncp <-  pwr.obj$lambda
  null.ncp <-  pwr.obj$null.lambda
  f.alpha <- pwr.obj$f.alpha

  if (effect == "between")     effect_bw <- paste0(c("B", "W"), "(", c(n.levels.between, n.levels.within), ")", collapse = "|")
  if (effect == "within")      effect_bw <- paste0(c("W", "B"), "(", c(n.levels.within, n.levels.between), ")", collapse = "|")
  if (effect == "interaction") effect_bw <- paste0(c("B", "W"), "(", c(n.levels.between, n.levels.within), ")", collapse = ":")

  if (verbose > 0) {

    if (n.levels.between == 1) test <- "Repeated Measures Analysis of Variance (F-Test)"
    if (n.levels.within == 1) test <- "Analysis of Variance (F-Test)"
    if (n.levels.within > 1 && n.levels.between > 1) test <- "Mixed-Effects Analysis of Variance (F-Test)"

    print.obj <- list(test = test, effect = effect_bw, n.total = n.total,
                      requested = requested, factor.levels = factor.levels,
                      power = power, ncp = ncp, null.ncp = null.ncp,
                      alpha = alpha, f.alpha = f.alpha, df1 = df1, df2 = df2)

    if (pretty) {
      .print.pwrss.ancova(print.obj, verbose = verbose)
    } else {
      .print.ascii.pwrss.ancova(print.obj, verbose = verbose)
    }

  } # verbose

  invisible(structure(list(parms = func.parms,
                           effect = effect_bw,
                           test = "F",
                           df1 = df1,
                           df2 = df2,
                           ncp = ncp,
                           null.ncp = null.ncp,
                           f.alpha = f.alpha,
                           power = power,
                           n.total = n.total),
                      class = c("pwrss", "f", "anova_mixed")))

} # power.f.anova.mixed

pwrss.f.mixed.anova <- power.f.mixed.anova



pwrss.f.rmanova <- function(eta2 = 0.10, f2 = eta2 / (1 - eta2),
                             corr.rm = 0.50, n.levels = 2, n.rm = 2,
                             epsilon = 1, alpha = 0.05,
                             type = c("between", "within", "interaction"),
                             n = NULL, power = NULL, verbose = TRUE) {

  type <- tolower(match.arg(type))
  verbose <- ensure_verbose(verbose)
  
  arg.names <- names(as.list(match.call()))
  f2_eta2 <- as.list(match.call())[c("f2", "eta2")]
  if ("repmeasures.r" %in% arg.names)
    stop("`repmeasures.r` argument is obsolete, use `corr.rm` instead", call. = FALSE)
  if ("n.measurements" %in% arg.names)
    stop("`n.measurements` argument is obsolete, use `n.rm` instead", call. = FALSE)
  if (all(utils::hasName(f2_eta2, c("f2", "eta2")))) {
    stop("Effect size conflict for the alternative. Specify only either `eta2` or `f2`.", call. = FALSE)
  } else if (utils::hasName(f2_eta2, "f2")) {
    eta2 <- f2 / (1 + f2)
  }

  mixed.anova.obj <- power.f.mixed.anova(eta.squared = eta2, n.total = n,
                                         factor.levels = c(n.levels, n.rm),
                                         factor.type = c("between", "within"),
                                         rho.within = corr.rm, epsilon = epsilon,
                                         power = power, alpha = alpha,
                                         effect = type, ceiling = TRUE, verbose = verbose)

  # cat("This function will be removed in the future. \n Please use power.f.mixed.anova() function. \n")

  return(invisible(mixed.anova.obj))

} # pwrss.f.rmanova()
