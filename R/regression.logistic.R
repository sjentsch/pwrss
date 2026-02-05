########################
# logistic regression  #
########################

# dist = c("normal", "poisson", "uniform", "exponential", "binomial", "bernouilli", "lognormal")
# dist = list(dist = "normal", mean = 0, sd = 1)
# dist = list(dist = "poisson", lambda = 1)
# dist = list(dist = "uniform", min = 0, max = 1)
# dist = list(dist = "exponential", rate = 1)
# dist = list(dist = "binomial", size = 1, prob = 0.50)
# dist = list(dist = "bernoulli", prob = 0.50)
# dist = list(dist = "lognormal", meanlog = 0, sdlog = 1)

power.z.logistic <- function(prob = NULL, base.prob = NULL, odds.ratio = NULL,
                             beta0 = NULL, beta1 = NULL,
                             n = NULL, power = NULL,
                             r.squared.predictor = 0,
                             alpha = 0.05, alternative = c("two.sided", "one.sided"),
                             method = c("demidenko(vc)", "demidenko", "hsieh"),
                             distribution = "normal", ceiling = TRUE,
                             verbose = 1, pretty = FALSE) {

  alternative <- tolower(match.arg(alternative))
  method <- tolower(match.arg(method))
  func.parms <- clean.parms(as.list(environment()))
  arg.names <- names(as.list(match.call()))

  check.proportion(r.squared.predictor)
  if (!is.null(n)) check.sample.size(n)
  if (!is.null(power)) check.proportion(power)
  check.proportion(alpha)
  check.logical(ceiling, pretty)
  verbose <- .ensure_verbose(verbose)
  requested <- check.n_power(n, power)

  if (all(c("base.prob", "prob") %in% arg.names)) {
    check.proportion(prob, base.prob)
    if (any(c("odds.ratio", "beta0", "beta1") %in% arg.names) && verbose >= 0)
      message("Using `base.prob` and `prob`, ignoring any specifications to `odds.ratio`, `beta0`, or `beta1`.")
    odds.ratio <- (prob / (1 - prob)) / (base.prob / (1 - base.prob))
    beta0 <- log(base.prob / (1 - base.prob))
    beta1 <- log(odds.ratio)
  } else if (all(c("base.prob", "odds.ratio") %in% arg.names)) {
    check.proportion(base.prob)
    check.positive(odds.ratio)
    if (any(c("prob", "beta0", "beta1") %in% arg.names) && verbose >= 0)
      message("Using `base.prob` and `odds.ratio`, ignoring any specifications to `prob`, `beta0`, or `beta1`.")
    prob <- odds.ratio * (base.prob / (1 - base.prob)) / (1 + odds.ratio * (base.prob / (1 - base.prob)))
    beta0 <- log(base.prob / (1 - base.prob))
    beta1 <- log(odds.ratio)
  } else if (all(c("base.prob", "beta1") %in% arg.names)) {
    check.proportion(base.prob)
    check.numeric(beta1)
    if (any(c("prob", "beta0", "odds.ratio") %in% arg.names) && verbose >= 0)
      message("Using `base.prob` and `beta1`, ignoring any specifications to `prob`, `beta0`, or `odds.ratio`.")
    odds.ratio <- exp(beta1)
    prob <- odds.ratio * (base.prob / (1 - base.prob)) / (1 + odds.ratio * (base.prob / (1 - base.prob)))
    beta0 <- log(base.prob / (1 - base.prob))
  } else if (all(c("beta0", "beta1") %in% arg.names)) {
    check.numeric(beta0, beta1)
    if (any(c("base.prob", "prob", "odds.ratio") %in% arg.names) && verbose >= 0)
      message("Using `beta0` and `beta1`, ignoring any specifications to `base.prob`, `prob`, or `odds.ratio`.")
    base.prob <- exp(beta0) / (1 + exp(beta0))
    odds.ratio <- exp(beta1)
    prob <- odds.ratio * (base.prob / (1 - base.prob)) / (1 + odds.ratio * (base.prob / (1 - base.prob)))
  } else {
    stop("Specify `base.prob` & `prob` \n  or `base.prob` & `odds.ratio` \n  or `base.prob` & `beta1`\n  or `beta0` & `beta1`.", call. = FALSE)
  }

  if (prob == base.prob) stop("`prob` can not have the same value as `base.prob`.", call. = FALSE)

  # check distribution
  if (length(distribution) == 1 && is.character(distribution)) {
    distribution <- switch(tolower(distribution),
                           `normal` = list(dist = "normal", mean = 0, sd = 1),
                           `poisson` = list(dist = "poisson", lambda = 1),
                           `uniform` = list(dist = "uniform", min = 0, max = 1),
                           `exponential` = list(dist = "exponential", rate = 1),
                           `binomial` = list(dist = "binomial", size = 1, prob = 0.50),
                           `bernoulli` = list(dist = "bernoulli", prob = 0.50),
                           `lognormal` = list(dist = "lognormal", meanlog = 0, sdlog = 1))
  } else if (is.list(distribution)) {
    if (length(distribution) > 3) stop("Unknown input type for `distribution`.", call. = FALSE)
    dist.list.names <- names(distribution)
    dist.attrib <- c(dist.list.names, tolower(distribution$dist))
    dist.invalid <- c(any(is.na(match(dist.attrib, c("dist", "normal", "mean", "sd")))),
                      any(is.na(match(dist.attrib, c("dist", "poisson", "lambda")))),
                      any(is.na(match(dist.attrib, c("dist", "uniform", "min", "max")))),
                      any(is.na(match(dist.attrib, c("dist", "exponential", "rate")))),
                      any(is.na(match(dist.attrib, c("dist", "binomial", "size", "prob")))),
                      any(is.na(match(dist.attrib, c("dist", "bernoulli", "prob")))),
                      any(is.na(match(dist.attrib, c("dist", "lognormal", "meanlog", "sdlog")))))
    if (all(dist.invalid == TRUE)) stop("Unknown input type for `distribution`.", call. = FALSE)
  } else {
    stop("Unknown input type for `distribution`.", call. = FALSE)
  }

  # asymptotic variances
  var.beta <- function(beta0, beta1, distribution) {

    # asymptotic variances
    if (tolower(distribution$dist) == "normal") {

      mean <- distribution$mean
      sd <- distribution$sd
      min.norm <- qnorm(.0000001, mean = mean, sd = sd)
      max.norm <- qnorm(.9999999, mean = mean, sd = sd)

      # define the distribution function and calculate mu (e1 = 0 -> x ^ e1 == 1), the log of which is beta0* (beta0s)
      dstFnc <- function(x, e1, b0, b1, e2) {
        x ^ e1 * dnorm(x, mean = mean, sd = sd) * exp(b0 + b1 * x) / (1 + exp(b0 + b1 * x)) ^ e2
      }
      mu  <- integrate(dstFnc, min.norm, max.norm, 0, beta0,  beta1, 1)$value
      beta0s <- log(mu / (1 - mu))

      # variance under null
      i00 <- integrate(dstFnc, min.norm, max.norm, 0, beta0s, 0,     2)$value
      i01 <- integrate(dstFnc, min.norm, max.norm, 1, beta0s, 0,     2)$value
      i11 <- integrate(dstFnc, min.norm, max.norm, 2, beta0s, 0,     2)$value
      var.beta0 <- i00 / (i00 * i11 - i01 ^ 2)

      # variance under alternative
      i00 <- integrate(dstFnc, min.norm, max.norm, 0, beta0,  beta1, 2)$value
      i01 <- integrate(dstFnc, min.norm, max.norm, 1, beta0,  beta1, 2)$value
      i11 <- integrate(dstFnc, min.norm, max.norm, 2, beta0,  beta1, 2)$value
      var.beta1 <- i00 / (i00 * i11 - i01 ^ 2)

    }  else if (tolower(distribution$dist) == "poisson") {

      lambda <- distribution$lambda
      # maximum value
      max.pois <- qpois(.9999999, lambda = lambda)

      # variance under null
      mu  <- sum(sapply(0:max.pois, function(x) { x ^ 0 * dpois(x, lambda = lambda) * exp(beta0 + beta1 * x) /
                                                          (1 + exp(beta0 + beta1 * x)) }), na.rm = TRUE)
      beta0.star <- log(mu / (1 - mu))
      beta1.star <- 0
      i00 <- sum(sapply(0:max.pois, function(x) { x ^ 0 * dpois(x, lambda = lambda) * exp(beta0.star + beta1.star * x) /
                                                          (1 + exp(beta0.star + beta1.star * x)) ^ 2 }), na.rm = TRUE)
      i01 <- sum(sapply(0:max.pois, function(x) { x ^ 1 * dpois(x, lambda = lambda) * exp(beta0.star + beta1.star * x) /
                                                          (1 + exp(beta0.star + beta1.star * x)) ^ 2 }), na.rm = TRUE)
      i11 <- sum(sapply(0:max.pois, function(x) { x ^ 2 * dpois(x, lambda = lambda) * exp(beta0.star + beta1.star * x) /
                                                          (1 + exp(beta0.star + beta1.star * x)) ^ 2 }), na.rm = TRUE)
      var.beta0 <- i00 / (i00 * i11 - i01 ^ 2)

      # variance under alternative
      i00 <- sum(sapply(0:max.pois, function(x) { x ^ 0 * dpois(x, lambda = lambda) * exp(beta0 + beta1 * x) /
                                                          (1 + exp(beta0 + beta1 * x)) ^ 2 }), na.rm = TRUE)
      i01 <- sum(sapply(0:max.pois, function(x) { x ^ 1 * dpois(x, lambda = lambda) * exp(beta0 + beta1 * x) /
                                                          (1 + exp(beta0 + beta1 * x)) ^ 2 }), na.rm = TRUE)
      i11 <- sum(sapply(0:max.pois, function(x) { x ^ 2 * dpois(x, lambda = lambda) * exp(beta0 + beta1 * x) /
                                                          (1 + exp(beta0 + beta1 * x)) ^ 2 }), na.rm = TRUE)
      var.beta1 <- i00 / (i00 * i11 - i01 ^ 2)

    }  else if (tolower(distribution$dist) == "uniform") {

      min <- distribution$min
      max <- distribution$max

      # variance under null
      mu  <- integrate(function(x) { x ^ 0 * dunif(x, min = min, max = max) * exp(beta0 + beta1 * x) /
                                             (1 + exp(beta0 + beta1 * x)) }, min, max)$value
      beta0.star <- log(mu / (1 - mu))
      beta1.star <- 0
      i00 <- integrate(function(x) { x ^ 0 * dunif(x, min = min, max = max) * exp(beta0.star + beta1.star * x) /
                                             (1 + exp(beta0.star + beta1.star * x)) ^ 2 }, min, max)$value
      i01 <- integrate(function(x) { x ^ 1 * dunif(x, min = min, max = max) * exp(beta0.star + beta1.star * x) /
                                             (1 + exp(beta0.star + beta1.star * x)) ^ 2 }, min, max)$value
      i11 <- integrate(function(x) { x ^ 2 * dunif(x, min = min, max = max) * exp(beta0.star + beta1.star * x) /
                                             (1 + exp(beta0.star + beta1.star * x)) ^ 2 }, min, max)$value
      var.beta0 <- i00 / (i00 * i11 - i01 ^ 2)

      # variance under alternative
      i00 <- integrate(function(x) { x ^ 0 * dunif(x, min = min, max = max) * exp(beta0 + beta1 * x) /
                                             (1 + exp(beta0 + beta1 * x)) ^ 2 }, min, max)$value
      i01 <- integrate(function(x) { x ^ 1 * dunif(x, min = min, max = max) * exp(beta0 + beta1 * x) /
                                             (1 + exp(beta0 + beta1 * x)) ^ 2 }, min, max)$value
      i11 <- integrate(function(x) { x ^ 2 * dunif(x, min = min, max = max) * exp(beta0 + beta1 * x) /
                                             (1 + exp(beta0 + beta1 * x)) ^ 2 }, min, max)$value
      var.beta1 <- i00 / (i00 * i11 - i01 ^ 2)

    } else if (tolower(distribution$dist) == "exponential") {

      rate <- distribution$rate
      max.exp <- qexp(.9999999, rate = rate)

      # variance under null
      mu  <- integrate(function(x) { x ^ 0 * dexp(x, rate = rate) * exp(beta0 + beta1 * x) /
                                             (1 + exp(beta0 + beta1 * x)) }, 0, max.exp)$value
      beta0.star <- log(mu / (1 - mu))
      beta1.star <- 0
      i00 <- integrate(function(x) { x ^ 0 * dexp(x, rate = rate) * exp(beta0.star + beta1.star * x) /
                                             (1 + exp(beta0.star + beta1.star * x)) ^ 2 }, 0, max.exp)$value
      i01 <- integrate(function(x) { x ^ 1 * dexp(x, rate = rate) * exp(beta0.star + beta1.star * x) /
                                             (1 + exp(beta0.star + beta1.star * x)) ^ 2 }, 0, max.exp)$value
      i11 <- integrate(function(x) { x ^ 2 * dexp(x, rate = rate) * exp(beta0.star + beta1.star * x) /
                                             (1 + exp(beta0.star + beta1.star * x)) ^ 2 }, 0, max.exp)$value
      var.beta0 <- i00 / (i00 * i11 - i01 ^ 2)

      # variance under alternative
      i00 <- integrate(function(x) { x ^ 0 * dexp(x, rate = rate) * exp(beta0 + beta1 * x) /
                                             (1 + exp(beta0 + beta1 * x)) ^ 2 }, 0, max.exp)$value
      i01 <- integrate(function(x) { x ^ 1 * dexp(x, rate = rate) * exp(beta0 + beta1 * x) /
                                             (1 + exp(beta0 + beta1 * x)) ^ 2 }, 0, max.exp)$value
      i11 <- integrate(function(x) { x ^ 2 * dexp(x, rate = rate) * exp(beta0 + beta1 * x) /
                                             (1 + exp(beta0 + beta1 * x)) ^ 2 }, 0, max.exp)$value
      var.beta1 <- i00 / (i00 * i11 - i01 ^ 2)

    }  else if (tolower(distribution$dist) %in% c("binomial", "bernoulli")) {

      size <- ifelse(tolower(distribution$dist) == "bernoulli", 1, distribution$size)
      prob <- distribution$prob

      # define the distribution function and calculate mu (e1 = 0 -> x ^ e1 == 1), the log of which is beta0* (beta0s)
      dstFnc <- function(x, e1, b0, b1, e2) {
        x ^ e1 * dbinom(x, size = size, prob = prob) * exp(b0 + b1 * x) / (1 + exp(b0 + b1 * x)) ^ e2
      }
      mu  <- sum(sapply(0:size, dstFnc, 0, beta0,  beta1, 1), na.rm = TRUE)
      beta0s <- log(mu / (1 - mu))

      # variance under null
      i00 <- sum(sapply(0:size, dstFnc, 0, beta0s, 0,     2), na.rm = TRUE)
      i01 <- sum(sapply(0:size, dstFnc, 1, beta0s, 0,     2), na.rm = TRUE)
      i11 <- sum(sapply(0:size, dstFnc, 2, beta0s, 0,     2), na.rm = TRUE)
      var.beta0 <- i00 / (i00 * i11 - i01 ^ 2)

      # variance under alternative
      i00 <- sum(sapply(0:size, dstFnc, 0, beta0,  beta1, 2), na.rm = TRUE)
      i01 <- sum(sapply(0:size, dstFnc, 1, beta0,  beta1, 2), na.rm = TRUE)
      i11 <- sum(sapply(0:size, dstFnc, 2, beta0,  beta1, 2), na.rm = TRUE)
      var.beta1 <- i00 / (i00 * i11 - i01 ^ 2)

    } else if (tolower(distribution$dist) == "lognormal") {

      meanlog <- distribution$meanlog
      sdlog <- distribution$sdlog
      min.lnorm <- qlnorm(.0000001, meanlog = meanlog, sdlog = sdlog)
      max.lnorm <- qlnorm(.9999999, meanlog = meanlog, sdlog = sdlog)

      # variance under null
      mu  <- integrate(function(x) { x ^ 0 * dlnorm(x, meanlog = meanlog, sdlog = sdlog) * exp(beta0 + beta1 * x) /
                                             (1 + exp(beta0 + beta1 * x)) }, min.lnorm, max.lnorm)$value
      beta0.star <- log(mu / (1 - mu))
      beta1.star <- 0
      i00 <- integrate(function(x) { x ^ 0 * dlnorm(x, meanlog = meanlog, sdlog = sdlog) * exp(beta0.star + beta1.star * x) /
                                             (1 + exp(beta0.star + beta1.star * x)) ^ 2 }, min.lnorm, max.lnorm)$value
      i01 <- integrate(function(x) { x ^ 1 * dlnorm(x, meanlog = meanlog, sdlog = sdlog) * exp(beta0.star + beta1.star * x) /
                                             (1 + exp(beta0.star + beta1.star * x)) ^ 2 }, min.lnorm, max.lnorm)$value
      i11 <- integrate(function(x) { x ^ 2 * dlnorm(x, meanlog = meanlog, sdlog = sdlog) * exp(beta0.star + beta1.star * x) /
                                             (1 + exp(beta0.star + beta1.star * x)) ^ 2 }, min.lnorm, max.lnorm)$value
      var.beta0 <- i00 / (i00 * i11 - i01 ^ 2)

      # variance under alternative
      i00 <- integrate(function(x) { x ^ 0 * dlnorm(x, meanlog = meanlog, sdlog = sdlog) * exp(beta0 + beta1 * x) /
                                             (1 + exp(beta0 + beta1 * x)) ^ 2 }, min.lnorm, max.lnorm)$value
      i01 <- integrate(function(x) { x ^ 1 * dlnorm(x, meanlog = meanlog, sdlog = sdlog) * exp(beta0 + beta1 * x) /
                                             (1 + exp(beta0 + beta1 * x)) ^ 2 }, min.lnorm, max.lnorm)$value
      i11 <- integrate(function(x) { x ^ 2 * dlnorm(x, meanlog = meanlog, sdlog = sdlog) * exp(beta0 + beta1 * x) /
                                             (1 + exp(beta0 + beta1 * x)) ^ 2 }, min.lnorm, max.lnorm)$value
      var.beta1 <- i00 / (i00 * i11 - i01 ^ 2)

    } # log-normal

    list(var.beta0 = var.beta0, var.beta1 = var.beta1, distribution = tolower(distribution$dist))

  } # var.beta


  # Demidenko, E. (2007). Sample size determination for logistic
  # regression revisited. Statistics in Medicine, 26, 3385-3397.
  pwr.demidenko <- function(beta0, beta1, n,
                            r.squared.predictor, alpha, alternative,
                            method, distribution) {

    # variance correction factor
    if (method == "demidenko(vc)") {
      vcf <- switch(tolower(distribution$dist),
                    `normal` = 1,
                    `poisson` = 1,
                    `uniform` = 1,
                    `exponential` = 1,
                    `binomial` = 0.85,
                    `bernoulli` = 0.85,
                    `lognormal` = 0.75)
    } else if (tolower(method) == "demidenko") {
      vcf <- 0
    }

    var.obj <- var.beta(beta0 = beta0, beta1 = beta1, distribution = distribution)
    var.beta0 <- var.obj$var.beta0
    var.beta1 <- var.obj$var.beta1

    # non-centrality parameter and standard deviation of the non-centrality parameter under alternative
    ncp <- beta1 / sqrt(var.beta1 / (n * (1 - r.squared.predictor)))
    sd.ncp <- sqrt((vcf * var.beta0 + (1 - vcf) * var.beta1) / var.beta1)

    pwr.obj <- power.z.test(mean = ncp, sd = sd.ncp, null.mean = 0,
                            alpha = alpha, alternative = alternative,
                            plot = FALSE, verbose = 0)
    power <- pwr.obj$power
    z.alpha <- pwr.obj$z.alpha

    list(power = power, ncp = ncp, sd.ncp = sd.ncp, vcf = vcf, z.alpha = z.alpha)

  } # pwr.demidenko()

  ss.demidenko <- function(beta0, beta1, power,
                           r.squared.predictor, alpha, alternative,
                           method, distribution) {

    n <- uniroot(function(n) {
      power - pwr.demidenko(beta0 = beta0, beta1 = beta1, n = n,
                            r.squared.predictor = r.squared.predictor,
                            alpha = alpha, alternative = alternative,
                            method = method, distribution = distribution)$power
    }, interval = c(2, 1e10))$root

    n

  } # ss.demidenko()



  # Hsieh, F. Y., Bloch, D. A., & Larsen, M. D. (1998). A simple
  # method of sample size calculation for linear and logistic
  # regression. Statistics in Medicine, 17, 1623-1634.
  ss.hsieh <- function(base.prob, prob,
                       r.squared.predictor,
                       power, alpha, alternative,
                       distribution) {

    if (tolower(distribution$dist) %in% c("binomial", "bernoulli")) {

      if (tolower(distribution$dist) == "binomial" && distribution$size > 1)
        stop(paste("Hsieh et al. (1998) is valid only for a binary covariate or a continuous covariate following normal",
                   "distribution."), call. = FALSE)
      prob <- distribution$prob
      beta <- 1 - power
      z.alpha <- qnorm(ifelse(alternative == "two.sided", alpha / 2, alpha), lower.tail = FALSE)
      z.beta <- qnorm(beta, lower.tail = FALSE)
      p.bar <- (1 - prob) * base.prob + prob * prob
      n <- (z.alpha * sqrt(p.bar * (1 - p.bar) / prob) + z.beta *
            sqrt(base.prob * (1 - base.prob) + prob * (1 - prob) * (1 - prob) / prob)) ^ 2 / ((base.prob - prob) ^ 2 * (1 - prob))
      n <- n / (1 - r.squared.predictor)

    } else if (tolower(distribution$dist) == "normal") {

      beta <- 1 - power
      z.alpha <- qnorm(ifelse(alternative == "two.sided", alpha / 2, alpha), lower.tail = FALSE)
      z.beta <- qnorm(beta, lower.tail = FALSE)
      odds.ratio <- (prob / (1 - prob)) / (base.prob / (1 - base.prob))
      beta1 <- log(odds.ratio)
      n <- (z.alpha + z.beta) ^ 2 / (base.prob * (1 - base.prob) * beta1 ^ 2)
      n <- n / (1 - r.squared.predictor)

    } else {

      stop("Not a valid distribution for the Hsieh et al. (1998) procedure.", call. = FALSE)

    }

    list(n = n, ncp = z.alpha + z.beta, sd.ncp = 1, vcf = NA, z.alpha = z.alpha)

  } # ss.hsieh()


  pwr.hsieh <- function(base.prob, prob,
                       r.squared.predictor,
                       n, alpha, alternative,
                       distribution) {

    power <- uniroot(function(power) {
      n - ss.hsieh(base.prob = base.prob, prob = prob,
                   r.squared.predictor = r.squared.predictor,
                   power = power, alpha = alpha,
                   alternative = alternative,
                   distribution = distribution)$n
    }, interval = c(0.01, 0.999))$root

    power

  } # pwr.hsieh



  if (method %in% c("demidenko(vc)", "demidenko")) {

    if (requested == "n") {

      n <- ss.demidenko(beta0 = beta0, beta1 = beta1, power = power,
                        r.squared.predictor = r.squared.predictor,
                        alpha = alpha, alternative = alternative,
                        method = method, distribution = distribution)

      if (ceiling) n <- ceiling(n)

    }

    # calculate power (if requested == "power") or update it (if requested == "n")
    pwr.obj <- pwr.demidenko(beta0 = beta0, beta1 = beta1, n = n, r.squared.predictor = r.squared.predictor,
                             alpha = alpha, alternative = alternative, method = method, distribution = distribution)

    power <- pwr.obj$power
    z.alpha <- pwr.obj$z.alpha
    ncp <- pwr.obj$ncp
    sd.ncp <- pwr.obj$sd.ncp
    vcf <- pwr.obj$vcf

  } else if (method == "hsieh") {

    if (requested == "n") {

      n <- ss.hsieh(base.prob = base.prob, prob = prob, r.squared.predictor = r.squared.predictor,
                    power = power, alpha = alpha, alternative = alternative, distribution = distribution)$n

      if (ceiling) n <- ceiling(n)

    }

    # calculate power (if requested == "power") or update it (if requested == "n")
    power <- pwr.hsieh(base.prob = base.prob, prob = prob, r.squared.predictor = r.squared.predictor, n = n,
                       alpha = alpha, alternative = alternative, distribution = distribution)

    ss.obj <- ss.hsieh(base.prob = base.prob, prob = prob, r.squared.predictor = r.squared.predictor, power = power,
                       alpha = alpha, alternative = alternative, distribution = distribution)

    z.alpha <- ss.obj$z.alpha
    ncp <- ss.obj$ncp
    sd.ncp <- ss.obj$sd.ncp
    vcf <- ss.obj$vcf

  } # method

  if (verbose > 0) {

    print.obj <- list(requested = requested,
                      test = "Logistic Regression Coefficient (Wald's Z-Test)",
                      method = switch(method,
                                      `demidenko(vc)` = "Demidenko (Variance Corrected)",
                                      `demidenko` = "Demidenko",
                                      `hsieh` = "Hsieh"),
                      dist = switch(tolower(distribution$dist),
                                    `normal` = "Normal",
                                    `poisson` = "Poisson",
                                    `bernoulli` = "Bernoulli",
                                    `binomial` = "Binomial",
                                    `lognormal` = "Log-normal",
                                    `uniform` = "Uniform",
                                    `exponential` = "Exponential"),
                      alt = alternative,
                      base.prob = base.prob,
                      odds.ratio = odds.ratio,
                      n = n,
                      mean.alternative = ncp,
                      sd.alternative = sd.ncp,
                      vcf = vcf,
                      mean.null = 0,
                      sd.null = 1,
                      z.alpha = z.alpha,
                      alpha = alpha,
                      power = power)

    if (pretty) {
      .print.pwrss.logistic(print.obj, verbose = verbose)
    } else {
      .print.ascii.pwrss.logistic(print.obj, verbose = verbose)
    }

  } # verbose

  invisible(structure(list(parms = func.parms,
                           test = "z",
                           odds.ratio = odds.ratio,
                           mean = ncp,
                           sd = sd.ncp,
                           vcf = vcf,
                           null.mean = 0,
                           null.sd = 1,
                           z.alpha = z.alpha,
                           power = power,
                           n = n),
                      class = c("pwrss", "z", "logistic")))
} # end of power.z.logistic()
power.z.logreg <- power.z.logistic


pwrss.z.logistic <- function(p1 = NULL, p0 = NULL, odds.ratio  = NULL,
                             beta0 = NULL, beta1 = NULL,
                             n = NULL, power = NULL, r2.other.x = 0,
                             alpha = 0.05, alternative = c("not equal", "less", "greater"),
                             method = c("demidenko(vc)", "demidenko", "hsieh"),
                             distribution = "normal", verbose = TRUE) {

  method <- tolower(match.arg(method))
  alternative <- tolower(match.arg(alternative))
  verbose <- .ensure_verbose(verbose)

  if (alternative %in% c("less", "greater")) alternative <- "one.sided"
  if (alternative == "not equal") alternative <- "two.sided"

  logreg.obj <- power.z.logistic(prob = p1, base.prob = p0, odds.ratio = odds.ratio,
                                 beta0 = beta0, beta1 = beta1, n = n, power = power,
                                 r.squared.predictor = r2.other.x, alpha = alpha,
                                 alternative = alternative, method = method,
                                 distribution = distribution, ceiling = TRUE,
                                 verbose = verbose)

  # cat("This function will be removed in the future. \n Please use power.z.logistic() function. \n")

  return(invisible(logreg.obj))

} # pwrss.z.logistic
pwrss.z.logreg <- pwrss.z.logistic
