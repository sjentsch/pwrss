# f.squared <- eta.squared / (1 - eta.squared)
# eta.squared <- f.squared / (1 + f.squared)
power.f.ancova <- function(eta.squared,
                           null.eta.squared = 0,
                           factor.levels = 2,
                           k.covariates = 0,
                           n.total = NULL,
                           power = NULL,
                           alpha = 0.05,
                           ceiling = TRUE,
                           verbose = 1,
                           pretty = FALSE) {

  func.parms <- clean.parms(as.list(environment()))

  check.nonnegative(eta.squared, null.eta.squared, k.covariates)
  check.vector(factor.levels, check.sample.size, 1)
  if (!is.null(n.total)) check.sample.size(n.total)
  if (!is.null(power)) check.proportion(power)
  check.proportion(alpha)
  check.logical(ceiling, pretty)
  verbose <- .ensure_verbose(verbose)

  requested <- check.n_power(n.total, power)

  f.squared <- eta.squared / (1 - eta.squared)
  null.f.squared <- null.eta.squared / (1 - null.eta.squared)

  ss <- function(df1, n.groups, k.covariates, f.squared, null.f.squared, alpha, power) {

    n.total <- try(silent = TRUE,
        suppressWarnings({
          uniroot(function(n.total) {
            u <- df1
            v <- n.total - n.groups - k.covariates
            lambda <- f.squared * n.total
            null.lambda <- null.f.squared * n.total
            f.alpha <- qf(alpha, df1 = u, df2 = v, ncp = null.lambda, lower.tail = FALSE)
            power - pf(f.alpha, df1 = u, df2 = v, ncp = lambda, lower.tail = FALSE)
          }, interval = c(n.groups + k.covariates + 2, 1e10))$root
        }) # supressWarnings
    ) # try

    if (inherits(n.total, "try-error") || n.total == 1e10) stop("Design is not feasible.", call. = FALSE)

    n.total

  } # ss

  pwr <- function(df1, n.total, n.groups, k.covariates, f.squared, null.f.squared, alpha) {
    u <- df1
    v <- n.total - n.groups - k.covariates
    lambda <- f.squared * n.total
    null.lambda <- null.f.squared * n.total
    f.alpha <- qf(alpha, df1 = u, df2 = v, ncp = null.lambda, lower.tail = FALSE)
    power <- pf(f.alpha, df1 = u, df2 = v, ncp = lambda, lower.tail = FALSE)
    list(power = power, u = u, v = v, lambda = lambda,
         null.lambda = null.lambda, f.alpha = f.alpha)
  } # pwr

  n.way <- length(factor.levels)
  n.groups <- prod(factor.levels)

  if (n.way == 1) {

    effect <- paste0(c("A"), "(", factor.levels, ")")
    df1 <- factor.levels[1] - 1

  } else if (n.way == 2) {

    effect <- paste0(c("A", "B"), "(", factor.levels, ")", collapse = ":")
    df1 <- prod(factor.levels - 1)

  } else if (n.way == 3) {

    effect <- paste0(c("A", "B", "C"), "(", factor.levels, ")", collapse = ":")
    df1 <- prod(factor.levels - 1)

  } else {

    stop("More than three-way ANOVA or ANCOVA is not allowed at the moment.", call. = FALSE)

  } # n.way

  if (requested == "n") {

    n.total <- ss(df1 = df1, n.groups = n.groups, k.covariates = k.covariates,
                  f.squared = f.squared, null.f.squared = null.f.squared,
                  alpha = alpha, power = power)

    if (ceiling) {

      n.total <- ceiling(n.total / n.groups) * n.groups

    }

    pwr.obj <- pwr(df1 = df1, n.total = n.total, n.groups = n.groups, k.covariates = k.covariates,
                   f.squared = f.squared, null.f.squared = null.f.squared, alpha = alpha)

    df2 <- n.total - n.groups - k.covariates
    power <- ncp <- pwr.obj$power
    ncp <-  pwr.obj$lambda
    null.ncp <-  pwr.obj$null.lambda
    f.alpha <- pwr.obj$f.alpha

  } else if (requested == "power") {

    pwr.obj <- pwr(df1 = df1, n.total = n.total, n.groups = n.groups, k.covariates = k.covariates,
                 f.squared = f.squared, null.f.squared = null.f.squared, alpha = alpha)
    df2 <- n.total - n.groups - k.covariates
    power <- ncp <- pwr.obj$power
    ncp <-  pwr.obj$lambda
    null.ncp <-  pwr.obj$null.lambda
    f.alpha <- pwr.obj$f.alpha

  }

  if (verbose > 0) {

    test <- paste(switch(n.way,
                         `1` = "One",
                         `2` = "Two",
                         `3` = "Three"),
                  ifelse(k.covariates > 0,
                         "-way Analysis of Covariance (F-Test)",
                         "-way Analysis of Variance (F-Test)"),
                  sep = "")

    print.obj <- list(test = test, effect = effect, n.total = n.total, n.way = n.way,
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
                           test = "F",
                           effect = effect,
                           df1 = df1,
                           df2 = df2,
                           ncp = ncp,
                           null.ncp = null.ncp,
                           f.alpha = f.alpha,
                           power = power,
                           n.total = n.total),
                      class = c("pwrss", "f", "ancova")))

} # end of power.f.ancova()

pwrss.f.ancova <- function(eta2 = 0.01, f2 = eta2 / (1 - eta2),
                           n.way = length(n.levels),
                           n.levels = 2, n.covariates = 0, alpha = 0.05,
                           n = NULL, power = NULL, verbose = TRUE) {

  verbose <- .ensure_verbose(verbose)
  f2_eta2 <- as.list(match.call())[c("f2", "eta2")]

  if (all(hasName(f2_eta2, c("f2", "eta2")))) {
    stop("Effect size conflict for the alternative. Specify only either `eta2` or `f2`.", call. = FALSE)
  } else if (hasName(f2_eta2, "f2")) {
    eta2 <- f2 / (1 + f2)
  }
  # eta2 doesn't need conversion, and falls back to the default if neither f2 nor eta2 is given explicitly

  ancova.obj <- power.f.ancova(eta.squared = eta2,
                               null.eta.squared = 0,
                               factor.levels = n.levels,
                               k.covariates = n.covariates,
                               n.total = n,
                               power = power,
                               alpha = alpha,
                               ceiling = TRUE,
                               verbose = verbose)

  # cat("This function will be removed in the future. \n Please use power.f.ancova() function. \n")

  return(invisible(ancova.obj))

} # pwrss.f.ancova


power.f.ancova.keppel <- function(mu.vector,
                                  sd.vector,
                                  n.vector = NULL,
                                  p.vector = NULL,
                                  factor.levels = NULL,
                                  r.squared = 0,
                                  k.covariates = 0,
                                  power = NULL,
                                  alpha = 0.05,
                                  ceiling = TRUE,
                                  verbose = 1,
                                  pretty = FALSE) {

  func.parms <- clean.parms(as.list(environment()))

  # value and consistency checks
  if (!is.vector(mu.vector) || !is.numeric(mu.vector))
    stop("Provide a vector of means (`mu.vector`) with its length equal to number of groups.", call. = FALSE)
  check.vector(mu.vector, check.numeric)
  check.vector(sd.vector, check.positive)
  if (!is.null(n.vector)) check.vector(n.vector, check.sample.size)
  if (!is.null(p.vector)) check.vector(p.vector, check.proportion)
  check.same.lengths(mu.vector, sd.vector, n.vector, p.vector)
  if (is.null(factor.levels)) factor.levels <- length(mu.vector)
  if (length(factor.levels) > 1)
    stop("Factorial designs are not allowed in Keppel's approach.", call. = FALSE)
  check.factor.level(factor.levels)
  if (length(mu.vector) != factor.levels)
    stop("Length of the vector of means (`mu.vector`) does not match number of levels.", call. = FALSE)
  if (r.squared > 1 || r.squared < 0 || !is.numeric(r.squared) || length(r.squared) != 1)
    stop("R-squared (explanatory power of covariates) takes a value between 0 and 1.", call. = FALSE)
  check.nonnegative(k.covariates)
  if (r.squared > 0 && k.covariates < 1)
    stop("Explanatory power of covariates is expected to be non-zero when number of covariates is non-zero.", call. = FALSE)
  if (!is.null(power)) check.proportion(power)
  check.proportion(alpha)
  check.logical(ceiling, pretty)
  verbose <- .ensure_verbose(verbose)

  requested <- check.n_power(n.vector, power)

  ncp.keppel <- function(mu.vector, sd.vector, n.vector, k.covariates, r.squared, factor.levels) {

    n.total <- sum(n.vector)
    mu_bar <- sum(n.vector * mu.vector) / n.total

    sigma2_pooled <- sum((n.vector - 1) * sd.vector ^ 2) / (n.total - length(mu.vector))

    sigma2_between <- sum(n.vector * (mu.vector - mu_bar) ^ 2) / n.total
    sigma2_error <- sigma2_pooled * (1 - r.squared)

    f.squared <- sigma2_between / sigma2_error
    eta.squared <- sigma2_between / (sigma2_between + sigma2_error)

    u <- prod(factor.levels - 1)
    v <- n.total - length(mu.vector) - k.covariates
    lambda <- f.squared * n.total

    list(f = sqrt(f.squared), eta.squared = eta.squared,
         df1 = u, df2 = v, lambda = lambda)

  }

  pwr.keppel <- function(mu.vector, sd.vector, n.vector, k.covariates, r.squared, alpha, factor.levels) {

    ncp.obj <- ncp.keppel(mu.vector = mu.vector, sd.vector = sd.vector,
                          n.vector = n.vector, k.covariates = k.covariates,
                          r.squared = r.squared, factor.levels = factor.levels)
    df1 <- ncp.obj$df1
    df2 <- ncp.obj$df2
    lambda <- ncp.obj$lambda
    f.alpha <- qf(alpha, df1 = df1, df2 = df2, lower.tail = FALSE)
    power <- pf(f.alpha, df1 = df1, df2 = df2, ncp = lambda, lower.tail = FALSE)
    list(power = power, df1 = df1, df2 = df2, lambda = lambda, f.alpha = f.alpha)
  }

  ss.keppel <- function(mu.vector, sd.vector, p.vector, k.covariates,
                        r.squared, alpha, power, factor.levels) {
    n.total <- uniroot(function(n.total) {
      n.vector <- n.total * p.vector
      power - pwr.keppel(mu.vector = mu.vector, sd.vector = sd.vector,
                         n.vector = n.vector, k.covariates = k.covariates,
                         r.squared = r.squared, alpha = alpha,
                         factor.levels = factor.levels)$power
    }, interval = c(length(mu.vector) + k.covariates + 2, 1e10))$root

    n.total
  }

  if (requested == "power") {

    pwr.obj <- pwr.keppel(mu.vector = mu.vector, sd.vector = sd.vector,
                          n.vector = n.vector, k.covariates = k.covariates,
                          r.squared = r.squared, alpha = alpha,
                          factor.levels = factor.levels)
    power <- pwr.obj$power
    df1 <- pwr.obj$df1
    df2 <- pwr.obj$df2
    ncp <- pwr.obj$lambda
    f.alpha <- pwr.obj$f.alpha
    n.total <- sum(n.vector)
    p.vector <- n.vector / sum(n.vector)

  } else if (requested == "n") {

    if (is.null(p.vector)) stop("`p.vector` cannot be NULL when sample size is requested", call. = FALSE)
    if (round(sum(p.vector), 5) != 1) stop("The elements of the `p.vector` should sum to 1", call. = FALSE)
    n.total <- ss.keppel(mu.vector = mu.vector, sd.vector = sd.vector,
                         p.vector = p.vector, k.covariates = k.covariates,
                         r.squared = r.squared, alpha = alpha, power =  power,
                         factor.levels = factor.levels)
    n.vector <- n.total * p.vector

    if (ceiling) {

      n.vector <- ceiling(n.vector)

    }

    pwr.obj <- pwr.keppel(mu.vector = mu.vector, sd.vector = sd.vector,
                          n.vector = n.vector, k.covariates = k.covariates,
                          r.squared = r.squared, alpha = alpha,
                          factor.levels = factor.levels)
    power <- pwr.obj$power
    df1 <- pwr.obj$df1
    df2 <- pwr.obj$df2
    ncp <- pwr.obj$lambda
    f.alpha <- pwr.obj$f.alpha
    n.total <- sum(n.vector)
    p.vector <- n.vector / sum(n.vector)
  }

  ncp.obj <- ncp.keppel(mu.vector = mu.vector, sd.vector = sd.vector,
                        n.vector = n.vector, k.covariates = k.covariates,
                        r.squared = r.squared, factor.levels = factor.levels)

  alpha.hc <- 0.05
  sd.vector.squared <- sd.vector ^ 2
  var.ratio <- max(sd.vector.squared) / min(sd.vector.squared)
  n.max <- n.vector[which(sd.vector.squared == max(sd.vector.squared))][1]
  n.min <- n.vector[which(sd.vector.squared == min(sd.vector.squared))][1]
  f.alpha.lower <- qf(alpha.hc, df1 = n.max - 1, df2 = n.min - 1, lower.tail = TRUE)
  f.alpha.upper <- qf(1 - alpha.hc, df1 = n.max - 1, df2 = n.min - 1, lower.tail = TRUE)
  if (var.ratio <= f.alpha.lower || var.ratio >= f.alpha.upper)
    warning("Interpretation of results may no longer be valid when variances differ beyond sampling error.", call. = FALSE)
  effect <- paste0(c("A"), "(", factor.levels, ")")
  n.way <- length(factor.levels)

  if (verbose > 0) {

    test <- ifelse(k.covariates > 0, "One-way Analysis of Covariance (F-Test)", "One-way Analysis of Variance (F-Test)")
    print.obj <- list(test = test, effect = effect, n.total = n.total, n.way = n.way,
                      requested = requested, factor.levels = factor.levels,
                      power = power, ncp = ncp, null.ncp = 0,
                      alpha = alpha, f.alpha = f.alpha, df1 = df1, df2 = df2)

    if (pretty) {
      .print.pwrss.ancova(print.obj, verbose = verbose)
    } else {
      .print.ascii.pwrss.ancova(print.obj, verbose = verbose)
    }

  } #verbose

  invisible(structure(list(parms = func.parms,
                           test = "F",
                           effect = effect,
                           eta.squared = ncp.obj$eta.squared,
                           f = ncp.obj$f,
                           df1 = df1,
                           df2 = df2,
                           ncp = ncp,
                           null.ncp = 0,
                           f.alpha = f.alpha,
                           power = power,
                           n.vector = n.vector,
                           n.total = n.total),
                      class = c("pwrss", "f", "ancova", "keppel")))

} # power.f.ancova.keppel

pwrss.f.ancova.keppel <- power.f.ancova.keppel


# default base functions contr.treatment() and contr.sum()
# provides coding for the design matrix (dummy, effect, etc.)
# some further manipulations are needed to get contrasts
# that is, add intercept and take inverse
# https://rpubs.com/timflutre/tuto_contrasts
# https://stats.oarc.ucla.edu/spss/faq/coding-systems-for-categorical-variables-in-regression-analysis/
factorial.contrasts <- function(factor.levels = c(3, 2),
                                coding.scheme = rep("deviation", length(factor.levels)),
                                base = factor.levels, # only used with dummy or treatment coding
                                intercept = FALSE,
                                verbose = 1) {

  if (length(coding.scheme) > length(factor.levels)) {

    coding.scheme <- coding.scheme[seq_along(factor.levels)]
    if (verbose >= 0)
      message(sprintf("Provide as many coding schemes as number of factors. Using the first %d.", length(factor.levels)))

  } # coding.scheme

  if (length(factor.levels) > 1) {

    if (length(coding.scheme) == 1 && verbose >= 0) {

      coding.scheme <- rep(coding.scheme, length(factor.levels))
      if (verbose >= 0)
        message("Assuming the same coding scheme applies to the other factor(s)")

    }

    if (length(base) == 1) {

      base <- rep(base, length(factor.levels))

    } else if (length(base) > length(factor.levels)) {

      base <- base[seq_along(factor.levels)]

    } # base

  } # factor.levels


  temp.contrast.list <- rep(list(NULL), length(factor.levels))

  for (i in seq_along(factor.levels)) {

    if (coding.scheme[i] %in% c("dummy", "treatment")) {

      temp.contrast.list[[i]] <- contr.treatment(n = factor.levels[i], base = base[i])

    } else if (coding.scheme[i] %in% c("deviation", "effect", "sum")) {

      temp.contrast.list[[i]] <- contr.sum(n = factor.levels[i])

    } else if (coding.scheme[i] == "helmert") {

      temp.contrast.list[[i]] <- contr.helmert(n = factor.levels[i])

    } else if (coding.scheme[i] %in% c("poly", "polynomial")) {

      temp.contrast.list[[i]] <- contr.poly(n = factor.levels[i])

    } else {

      stop(sprintf("Contrast type \"%s\" not supported at the moment.", coding.scheme[i]), call. = FALSE)

    }

  } # for loop



  if (length(factor.levels) == 1) {

    factor.data <- data.frame(A = gl(factor.levels[1], 1))

    contrasts.list <- list(A = temp.contrast.list[[1]])

    model.mat <- model.matrix(~ A, factor.data, contrasts.arg = contrasts.list)

    contrast.mat <- solve(model.mat)

    col.names <- expand.grid(
      A = paste0("A", seq_len(factor.levels[1]))
    )

    means.cell.names <- apply(col.names, 1, paste, collapse = ":")
    colnames(contrast.mat) <- means.cell.names


  } else if (length(factor.levels) == 2) {

    factor.data <- data.frame(A = gl(factor.levels[1], factor.levels[2]),
                              B = gl(factor.levels[2], 1, prod(factor.levels)))

    contrasts.list <- list(A = temp.contrast.list[[1]],
                           B = temp.contrast.list[[2]])

    model.mat <- model.matrix(~ A + B + A:B, factor.data,
                                 contrasts.arg = contrasts.list)

    contrast.mat <- solve(model.mat)

    col.names <- expand.grid(
      B = paste0("B", seq_len(factor.levels[2])),
      A = paste0("A", seq_len(factor.levels[1]))
    )
    col.names <- col.names[, c("A", "B")]
    means.cell.names <- apply(col.names, 1, paste, collapse = ":")

    colnames(contrast.mat) <- means.cell.names


  } else if (length(factor.levels) == 3) {

    factor.data <- data.frame(A = gl(factor.levels[1], prod(factor.levels[2:3])),
                              B = gl(factor.levels[2], factor.levels[3], prod(factor.levels)),
                              C = gl(factor.levels[3], 1, prod(factor.levels)))


    contrasts.list <- list(A = temp.contrast.list[[1]],
                           B = temp.contrast.list[[2]],
                           C = temp.contrast.list[[3]])

    model.mat <- model.matrix(~ A + B + C + A:B + A:C + B:C + A:B:C, factor.data,
                                 contrasts.arg = contrasts.list)

    contrast.mat <- solve(model.mat)

    col.names <- expand.grid(
      C = paste0("C", seq_len(factor.levels[3])),
      B = paste0("B", seq_len(factor.levels[2])),
      A = paste0("A", seq_len(factor.levels[1]))
    )
    col.names <- col.names[, c("A", "B", "C")]
    means.cell.names <- apply(col.names, 1, paste, collapse = ":")
    colnames(contrast.mat) <- means.cell.names

  } else {

    stop("This version supports only up to three-way interactions.", call. = FALSE)

  }

  if (length(factor.levels) > 1 && verbose >= 0)
    message("Elements of `mu.vector`, `sd.vector`, `n.vector` or `p.vector` should follow this specific order:\n",
            paste(means.cell.names, " "), "\n")

  if (isFALSE(intercept)) contrast.mat <- contrast.mat[-1, ]

  if (verbose > 0) {
    print(round(contrast.mat, 3))
  }

  invisible(list(factor.levels = factor.levels,
                 factor.data = factor.data,
                 model.matrix = model.mat,
                 contrast.matrix = contrast.mat))

} # factorial.contrasts()


power.f.ancova.shieh <- function(mu.vector,
                                 sd.vector,
                                 n.vector = NULL,
                                 p.vector = NULL,
                                 factor.levels = NULL,
                                 r.squared = 0,
                                 k.covariates = 1,
                                 contrast.matrix = NULL,
                                 power = NULL,
                                 alpha = 0.05,
                                 ceiling = TRUE,
                                 verbose = 1,
                                 pretty = FALSE) {

  func.parms <- clean.parms(as.list(environment()))

  # value and consistency checks
  if (!is.vector(mu.vector) || !is.numeric(mu.vector))
    stop("Provide a vector of means (`mu.vector`) with its length equal to the number of groups.", call. = FALSE)
  check.vector(mu.vector, check.numeric)
  check.vector(sd.vector, check.positive)
  if (!is.null(n.vector)) check.vector(n.vector, check.sample.size)
  if (!is.null(p.vector)) check.vector(p.vector, check.proportion)
  check.same.lengths(mu.vector, sd.vector, n.vector, p.vector)
  if (is.null(factor.levels)) factor.levels <- length(mu.vector)
  check.vector(factor.levels, check.factor.level, 1)
  if (length(mu.vector) != prod(factor.levels))
    stop("Provide a vector of means (`mu.vector`) with its length equal to the the product of `factor.levels`.", call. = FALSE)
  if (r.squared > 1 || r.squared < 0 || !is.numeric(r.squared) || length(r.squared) != 1)
    stop("R-squared (explanatory power of covariates) takes a value between 0 and 1.", call. = FALSE)
  check.positive(k.covariates)
  if (!is.null(power)) check.proportion(power)
  if (alpha > 1 || alpha < 0 || !is.numeric(alpha) || length(alpha) != 1)
    stop("Type 1 error rate (alpha) takes a value between 0 and 1.", call. = FALSE)
  check.logical(ceiling, pretty)
  verbose <- .ensure_verbose(verbose)

  requested <- check.n_power(n.vector, power)

  if (is.null(contrast.matrix)) {

    contrast.matrix <- factorial.contrasts(factor.levels = factor.levels,
                                           intercept = FALSE,
                                           verbose = min(verbose, 0))$contrast.matrix
    if (is.vector(contrast.matrix)) contrast.matrix <- t(as.matrix(contrast.matrix))
    contrast.matrix <- tail(contrast.matrix, n = prod(factor.levels - 1))

  } else {

    if (is.vector(contrast.matrix))
      contrast.matrix <- t(as.matrix(contrast.matrix))
    if (!is.matrix(contrast.matrix))
      stop("Contrast coefficients are not provided in the form of a matrix.", call. = FALSE)
    if (dim(contrast.matrix)[2] != length(mu.vector))
      stop("The number of columns in the contrast matrix should match number of groups.", call. = FALSE)
    if (dim(contrast.matrix)[1] > length(mu.vector) - 1)
      stop("The number of rows in the contrast matrix should be less than or equal to number of groups minus one.", call. = FALSE)
  }

  pwr.shieh <- function(mu.vector, sd.vector, n.vector, k.covariates, r.squared, alpha, contrast.matrix, calculate.lambda = FALSE) {

    n.total <- sum(n.vector)
    sigma2_pooled <- sum((n.vector - 1) * sd.vector ^ 2) / (n.total - length(mu.vector))
    sigma2_error <- sigma2_pooled * (1 - r.squared)

    Q.mat <- diag(n.total / n.vector)
    mean.mat <- matrix(mu.vector, length(mu.vector), 1)
    gamma2 <- (t(contrast.matrix %*% mean.mat) %*% solve(contrast.matrix %*% Q.mat %*%
               t(contrast.matrix)) %*% (contrast.matrix %*% mean.mat)) / sigma2_error
    gamma2 <- as.numeric(gamma2)

    u <- nrow(contrast.matrix)
    v <- n.total - length(mu.vector) - k.covariates

    f.alpha <- qf(1 - alpha, df1 = u, df2 = v)

    if (k.covariates > 1) {

      shape1 <- (v + 1) / 2
      shape2 <- k.covariates / 2
      mean.beta <- shape1 / (shape1 + shape2)
      sd.beta <- sqrt((shape1 * shape2) / ((shape1 + shape2) ^ 2 * (shape1 + shape2 + 1)))
      lower.beta <- max(0, mean.beta - 10 * sd.beta)

      integrand <- function(x) { dbeta(x, shape1 = (v + 1) / 2, shape2 = k.covariates / 2) *
                                 pf(f.alpha, u, v, n.total * gamma2 * x, lower.tail = FALSE) }
      power <- integrate(integrand, lower = lower.beta, upper = 1)$value

      lambda <- ifelse(calculate.lambda, n.total * gamma2 * (v + 1) / (v + 1 + k.covariates), NA)

    } else if (k.covariates == 1) {

      integrand <- function(x) dt(x, (v + 1)) * pf(f.alpha, u, v, n.total * gamma2 / (1 + (k.covariates / (v + 1)) * x ^ 2), lower.tail = FALSE)
      power <- integrate(integrand, lower = -10, upper = 10)$value

      lambda <- ifelse(calculate.lambda,
                       integrate(function(x) dt(x, v) * n.total * gamma2 / (1 + (k.covariates / v) * x ^ 2), lower = -Inf, upper = Inf)$value,
                       NA)

    }

    list(power = power, df1 = u, df2 = v, lambda = lambda, f.alpha = f.alpha)

  }

  ss.shieh <- function(mu.vector, sd.vector, p.vector, k.covariates, r.squared, alpha, power, contrast.matrix) {

    uniroot(function(n.total) {
      n.vector <- n.total * p.vector
      power -  pwr.shieh(mu.vector = mu.vector, sd.vector = sd.vector,
                   n.vector = n.vector, k.covariates = k.covariates,
                   r.squared =  r.squared, alpha = alpha, contrast.matrix = contrast.matrix)$power
    }, interval = c(length(mu.vector) + k.covariates + 1, 1e10))$root

  }


  if (requested == "power") {

    pwr.obj <- pwr.shieh(mu.vector = mu.vector, sd.vector = sd.vector,
                         n.vector = n.vector, k.covariates = k.covariates,
                         r.squared =  r.squared, alpha = alpha, contrast.matrix = contrast.matrix,
                         calculate.lambda = TRUE)
    power <- pwr.obj$power
    df1 <- pwr.obj$df1
    df2 <- pwr.obj$df2
    ncp <- pwr.obj$lambda
    f.alpha <- pwr.obj$f.alpha
    n.total <- sum(n.vector)
    p.vector <- n.vector / sum(n.vector)

  } else if (requested == "n") {

    if (is.null(p.vector)) stop("`p.vector` cannot be NULL when sample size is requested.", call. = FALSE)
    if (round(sum(p.vector), 5) != 1) stop("The elements of the `p.vector` should sum to 1.", call. = FALSE)
    n.total <- ss.shieh(mu.vector = mu.vector, sd.vector = sd.vector,
                        p.vector = p.vector, k.covariates =  k.covariates,
                        r.squared = r.squared, alpha = alpha,
                        power = power, contrast.matrix =  contrast.matrix)
    n.vector <- n.total * p.vector

    if (ceiling) {

      n.vector <- ceiling(n.vector)
      n.total <- sum(n.vector)

    }

    pwr.obj <- pwr.shieh(mu.vector = mu.vector, sd.vector = sd.vector,
                         n.vector = n.vector, k.covariates = k.covariates,
                         r.squared =  r.squared, alpha = alpha, contrast.matrix = contrast.matrix,
                         calculate.lambda = TRUE)
    power <- pwr.obj$power
    df1 <- pwr.obj$df1
    df2 <- pwr.obj$df2
    ncp <- pwr.obj$lambda
    f.alpha <- pwr.obj$f.alpha

  }

  f.squared <- ncp / n.total
  eta.squared <- f.squared / (1 + f.squared)

  alpha <- 0.05
  sd.vector.squared <- sd.vector ^ 2
  var.ratio <- max(sd.vector.squared) / min(sd.vector.squared)
  n.max <- n.vector[which(sd.vector.squared == max(sd.vector.squared))][1]
  n.min <- n.vector[which(sd.vector.squared == min(sd.vector.squared))][1]
  f.alpha.lower <- qf(alpha, df1 = n.max - 1, df2 = n.min - 1, lower.tail = TRUE)
  f.alpha.upper <- qf(1 - alpha, df1 = n.max - 1, df2 = n.min - 1, lower.tail = TRUE)
  if (var.ratio <= f.alpha.lower || var.ratio >= f.alpha.upper)
    warning("Interpretation of results may no longer be valid when variances differ beyond sampling error.", call. = FALSE)


  n.way <- length(factor.levels)

  if (n.way == 1) {
    effect <- paste0(c("A"), "(", factor.levels, ")")
  } else if (n.way == 2) {
    effect <- paste0(c("A", "B"), "(", factor.levels, ")", collapse = ":")
  } else if (n.way == 3) {
    effect <- paste0(c("A", "B", "C"), "(", factor.levels, ")", collapse = ":")
  } else {
    stop("More than three-way ANCOVA is not allowed at the moment.", call. = FALSE)
  } # n.way

  if (verbose > 0) {

    test <- paste(switch(n.way,
                         `1` = "One",
                         `2` = "Two",
                         `3` = "Three"),
                  ifelse(k.covariates > 0,
                         "-way Analysis of Covariance (F-Test)",
                         "-way Analysis of Variance (F-Test)"),
                  sep = "")

    print.obj <- list(test = test, effect = effect, n.total = n.total,
                      requested = requested, factor.levels = factor.levels,
                      power = power, ncp = ncp, null.ncp = 0,
                      alpha = alpha, f.alpha = f.alpha, df1 = df1, df2 = df2)

    if (pretty) {
      .print.pwrss.ancova(print.obj, verbose = verbose)
    } else {
      .print.ascii.pwrss.ancova(print.obj, verbose = verbose)
    }

  } # verbose

  invisible(structure(list(parms = func.parms,
                           test = "F",
                           effect = effect,
                           eta.squared = eta.squared,
                           f = sqrt(f.squared),
                           df1 = df1,
                           df2 = df2,
                           ncp = ncp,
                           null.ncp = 0,
                           f.alpha = f.alpha,
                           power = power,
                           n.vector = n.vector,
                           n.total = n.total),
                      class = c("pwrss", "f", "ancova", "shieh")))


} # end of power.f.ancova.shieh()

pwrss.f.ancova.shieh <- power.f.ancova.shieh


power.t.contrast <- function(mu.vector,
                             sd.vector,
                             contrast.vector,
                             n.vector = NULL,
                             p.vector = NULL,
                             r.squared = 0,
                             k.covariates = 1,
                             power = NULL,
                             alpha = 0.05,
                             tukey.kramer = FALSE,
                             ceiling = TRUE,
                             verbose = 1,
                             pretty = FALSE) {

  func.parms <- clean.parms(as.list(environment()))

  # value checks
  check.vector(mu.vector, check.numeric)
  check.vector(sd.vector, check.positive)
  if (!is.null(n.vector)) check.vector(n.vector, check.sample.size)
  if (!is.null(p.vector)) check.vector(p.vector, check.proportion)
  check.same.lengths(mu.vector, sd.vector, n.vector, p.vector)
  if (r.squared > 1 || r.squared < 0 || !is.numeric(r.squared) || length(r.squared) != 1)
    stop("R-squared (explanatory power of covariates) takes a value between 0 and 1.", call. = FALSE)
  check.nonnegative(k.covariates)
  if (!is.null(power)) check.proportion(power)
  check.proportion(alpha)
  check.logical(tukey.kramer, ceiling, pretty)
  verbose <- .ensure_verbose(verbose)

  if (is.vector(contrast.vector))
    contrast.matrix <- t(as.matrix(contrast.vector))
  else if (is.matrix(contrast.vector))
    contrast.matrix <- contrast.vector
  else
    stop("`contrast.vector` must be either a vector or a matrix.", call. = FALSE)

  if (dim(contrast.matrix)[2] != length(mu.vector))
    stop("The number of columns in the contrast matrix should match number of groups.", call. = FALSE)
  if (dim(contrast.matrix)[1] > 1)
    stop("The number of rows in the contrast matrix should be one.", call. = FALSE)

  requested <- check.n_power(n.vector, power)

  pwr.contrast <- function(mu.vector, sd.vector, n.vector, k.covariates,
                           r.squared, alpha, contrast.matrix, tukey.kramer,
                           calculate.lambda = FALSE) {

    n.total <- sum(n.vector)
    sigma2_pooled <- sum((n.vector - 1) * sd.vector ^ 2) / (n.total - length(mu.vector))
    sigma2_error <- sigma2_pooled * (1 - r.squared)

    psi <- sum(contrast.matrix * mu.vector)
    v <- n.total - length(mu.vector) - k.covariates

    if (tukey.kramer == 1) {

      t.alpha <- qtukey(1 - alpha, length(mu.vector), v) / sqrt(2)

      } else {

      t.alpha <- qt(1 - alpha / 2, v)

    }

    a <- sum(contrast.matrix ^ 2 / n.vector)
    # delta <- psi / sqrt(a * sigma2_error)
    d <- psi / sqrt(sigma2_error)

    if (k.covariates == 1) {

      integrand <- function(x) {

        dt(x, v + 1) * (pt(-t.alpha, v, psi / sqrt(sigma2_error * a * (1 + x ^ 2 / (v + 1)))) +
                          pt(t.alpha, v, psi / sqrt(sigma2_error * a * (1 + x ^ 2 / (v + 1))), lower.tail = FALSE))

      }

      power <- integrate(integrand, lower = -10, upper = 10)$value

      lambda <- numeric(1)
      ifelse(calculate.lambda,
             lambda <-  integrate(function(x) dt(x, v + 1) * (psi / sqrt(sigma2_error * a * (1 + x ^ 2 / (v + 1)))), -10, 10)$value,
             lambda <- NA)

    } else if (k.covariates > 1) {

      shape1 <- (v + 1) / 2
      shape2 <- k.covariates / 2
      mean.beta <- shape1 / (shape1 + shape2)
      sd.beta <- sqrt((shape1 * shape2) / ((shape1 + shape2) ^ 2 * (shape1 + shape2 + 1)))
      lower.beta <- max(0, mean.beta - 10 * sd.beta)

      integrand <- function(x) {
        dbeta(x, (v + 1) / 2, k.covariates / 2) * (pt(-t.alpha, v, sqrt(x) * psi / sqrt(sigma2_error * a)) +
                                                   pt(t.alpha, v, sqrt(x) * psi / sqrt(sigma2_error * a), lower.tail = FALSE))
      }
      power <- integrate(integrand, lower = lower.beta, upper = 1)$value

      lambda <-  ifelse(calculate.lambda,
                        integrate(function(x) { dbeta(x, (v + 1) / 2, k.covariates / 2) *
                                                (sqrt(x) * psi / sqrt(sigma2_error * a)) }, lower.beta, 1)$value,
                        NA)

    } else {

      stop("The number of covariates should be 1 or greater in the analysis of covariance.", call. = FALSE)

    }

    list(power = power, df = v, lambda = lambda, t.alpha = t.alpha, psi = psi, d = d)

  } # pwr.contrast()

  ss.contrast <- function(mu.vector, sd.vector, p.vector, power, k.covariates,
                          r.squared, alpha, contrast.matrix, tukey.kramer) {

    psi <- sum(contrast.matrix * mu.vector)

    if (psi == 0) {

      n.total <- as.integer(.Machine$integer.max)
      warning("Using infinity (maximum integer number as defined in R) for `n.total` because `psi` = 0.", call. = FALSE)

    } else {

      n.total <- uniroot(function(n.total) {
        n.vector <- n.total * p.vector
        power - pwr.contrast(mu.vector = mu.vector, sd.vector = sd.vector,
                             n.vector = n.vector, k.covariates = k.covariates,
                             r.squared = r.squared, alpha = alpha, contrast.matrix = contrast.matrix,
                             tukey.kramer = tukey.kramer, calculate.lambda = FALSE)$power
      }, interval = c(length(mu.vector) + k.covariates + 1, 1e10))$root

    }

    n.total

  } # ss.contrast


  if (requested == "power") {

    pwr.obj <- pwr.contrast(mu.vector = mu.vector, sd.vector = sd.vector, n.vector = n.vector,
                            k.covariates = k.covariates, r.squared = r.squared, alpha = alpha,
                            contrast.matrix = contrast.vector, tukey.kramer = tukey.kramer, calculate.lambda = TRUE)
    power <- pwr.obj$power
    df <- pwr.obj$df
    ncp <- pwr.obj$lambda
    t.alpha <- pwr.obj$t.alpha
    psi <- pwr.obj$psi
    d <- pwr.obj$d
    n.total <- sum(n.vector)
    p.vector <- n.vector / sum(n.vector)

  } else if (requested == "n") {

    if (is.null(p.vector)) stop("The `p.vector` cannot be NULL when the sample size is requested.", call. = FALSE)
    if (round(sum(p.vector), 5) != 1) stop("The elements of the `p.vector` should sum to 1.", call. = FALSE)
    n.total <- ss.contrast(mu.vector = mu.vector, sd.vector = sd.vector, p.vector = p.vector, power = power,
                           k.covariates = k.covariates, r.squared = r.squared, alpha = alpha,
                           contrast.matrix = contrast.vector, tukey.kramer = tukey.kramer)
    n.vector <- n.total * p.vector

    if (ceiling) {

      n.vector <- ceiling(n.vector)
      n.total <- sum(n.vector)

    }

    pwr.obj <- pwr.contrast(mu.vector = mu.vector, sd.vector = sd.vector,
                            n.vector = n.vector, k.covariates = k.covariates,
                            r.squared = r.squared, alpha = alpha, contrast.matrix = contrast.matrix,
                            tukey.kramer = tukey.kramer, calculate.lambda = TRUE)
    power <- pwr.obj$power
    df <- pwr.obj$df
    ncp <- pwr.obj$lambda
    t.alpha <- pwr.obj$t.alpha
    psi <- pwr.obj$psi
    d <- pwr.obj$d

  }

  # u <- 1 # nrow(contrast.matrix)
  # v <- n.total - length(mu.vector) - k.covariates

  if (verbose > 0) {

    test <- "Single Contrast Analysis (T-Test)"

    print.obj <- list(test = test, psi = psi, d = d,
                      n.total = n.total, requested = requested,
                      power = power, ncp = ncp, null.ncp = 0,
                      alpha = alpha, t.alpha = c(-t.alpha, t.alpha), df = df)

    if (pretty) {
      .print.pwrss.contrast(print.obj, verbose = verbose)
    } else {
      .print.ascii.pwrss.contrast(print.obj, verbose = verbose)
    }

  }

  invisible(structure(list(parms = func.parms,
                           test = "t",
                           psi = psi,
                           d = d,
                           df = df,
                           t.alpha = c(-t.alpha, t.alpha),
                           ncp = ncp,
                           ncp.null = 0,
                           power = power,
                           n.vector = n.vector,
                           n.total = n.total),
                      class = c("pwrss", "t", "contrast")))


} # end of power.t.contrast()


adjust.alpha <- function(n, alpha = 0.05,
                         method = c("bonferroni", "holm", "hochberg",
                                    "hommel", "BH", "BY", "fdr", "none")) {

  check.proportion(alpha)
  check.sample.size(n)
  method <- match.arg(method)

  p.adj <- uniroot(function(p) {
    alpha - p.adjust(p = p, method = method, n = n)
  }, interval = c(0, 1))$root

  p.adj

}

power.t.contrasts <- function(x = NULL,
                              mu.vector = NULL,
                              sd.vector = NULL,
                              n.vector = NULL, p.vector = NULL,
                              r.squared = 0, k.covariates = 1,
                              contrast.matrix = NULL,
                              power = NULL, alpha = 0.05,
                              adjust.alpha = c("none", "tukey", "bonferroni",
                                               "holm", "hochberg", "hommel",
                                               "BH", "BY", "fdr"),
                              ceiling = TRUE, verbose = 1, pretty = FALSE) {

  if (!is.null(x)) {

    if (all(c("pwrss", "f", "ancova", "shieh") %in% class(x))) {

      # transfer pwrss.f.ancova.shieh object into the input parameters and remove the object
      mu.vector <- x$parms$mu.vector
      sd.vector <- x$parms$sd.vector
      n.vector <- x$n.vector
      p.vector <- x$parms$p.vector
      r.squared <- x$parms$r.squared
      k.covariates <- x$parms$k.covariates
      contrast.matrix <- x$parms$contrast.matrix
      ceiling <- x$parms$ceiling
      power <- NULL

    } else {

      stop("This function only works with an object of type `pwrss`, `ancova`, and `shieh`.", call. = FALSE)

    }

  } else {

    # value checks
    check.vector(mu.vector, check.numeric)
    check.vector(sd.vector, check.positive)
    if (!is.null(n.vector)) check.vector(n.vector, check.sample.size)
    if (!is.null(p.vector)) check.vector(p.vector, check.proportion)
    check.proportion(r.squared)
    check.nonnegative(k.covariates)
    if (!is.null(power)) check.proportion(power)
    check.proportion(alpha)
    check.logical(ceiling)

  } # if data is null

  rm(x)
  adjust.alpha <- tolower(match.arg(adjust.alpha))
  func.parms <- clean.parms(as.list(environment()))
  verbose <- .ensure_verbose(verbose)

  requested <- check.n_power(n.vector, power)

  if (is.vector(contrast.matrix))
    contrast.matrix <- t(as.matrix(contrast.matrix))
  levels <- colnames(contrast.matrix)

  if (tolower(adjust.alpha == "tukey")) {

    tukey.kramer <- TRUE

  } else {

    tukey.kramer <- FALSE

    if (nrow(contrast.matrix) > 1)
      alpha <- adjust.alpha(alpha = alpha, n = nrow(contrast.matrix),  method = adjust.alpha)

  } # adjust.alpha

  comparison <- NULL
  power.out <- NULL
  for (i in seq_len(nrow(contrast.matrix))) {

    contrast.vector <- contrast.matrix[i, ]
    contrast.sign <- sign(contrast.vector)

    idx.poz <- which(contrast.sign == 1)
    idx.neg <- which(contrast.sign == -1)

    comparison.i <- sprintf("%s <=> %s", paste(levels[idx.poz], collapse = " "), paste(levels[idx.neg], collapse = " "))
    comparison <- rbind(comparison, comparison.i)

    pwr.t.contr.obj <- power.t.contrast(mu.vector = mu.vector,
                                        sd.vector = sd.vector,
                                        n.vector = n.vector,
                                        p.vector = p.vector,
                                        contrast.vector = contrast.vector,
                                        r.squared = r.squared, k.covariates = k.covariates,
                                        alpha = alpha, power = power,
                                        tukey.kramer = tukey.kramer,
                                        ceiling = ceiling,
                                        verbose = 0)

    power.out.i <- cbind(psi = pwr.t.contr.obj$psi,
                         d = pwr.t.contr.obj$d,
                         ncp = pwr.t.contr.obj$ncp,
                         df = pwr.t.contr.obj$df,
                         t.alpha = max(pwr.t.contr.obj$t.alpha),
                         n.total = pwr.t.contr.obj$n.total,
                         power = pwr.t.contr.obj$power)

    power.out <- rbind(power.out, power.out.i)

  }

  rownames(comparison) <- NULL
  contrast.number <- seq_len(nrow(contrast.matrix))

  if (verbose > 0) {

    power.out <- as.data.frame(power.out)
    print.data <- data.frame(contr = contrast.number, comparison = comparison,
                             psi = round(power.out$psi, 3),
                             d = round(power.out$d, 3),
                             ncp = round(power.out$ncp, 3),
                             n.total = round(power.out$n.total, 3),
                             power = round(power.out$power, 3))

    test <- "Multiple Contrast Analyses (T-Tests)"

    print.obj <- list(test = test, requested = requested,
                      alpha = alpha, adjust.alpha = adjust.alpha,
                      null.ncp = 0, data = print.data)

    if (pretty) {
      .print.pwrss.contrasts(print.obj, verbose = verbose)
    } else {
      .print.ascii.pwrss.contrasts(print.obj, verbose = verbose)
    }

  } # verbose

  invisible(structure(c(list(parms = func.parms,
                             test = "t"),
                        data.frame(contrast = contrast.number,
                                   comparison = comparison,
                                   power.out)),
                      class = c("pwrss", "t", "contrasts")))

} # power.t.contrasts
