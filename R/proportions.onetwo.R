#############################
# one proportion exact test #
#############################

power.exact.oneprop <- function(prob, null.prob = 0.50,
                                n = NULL, power = NULL, alpha = 0.05,
                                alternative = c("two.sided", "one.sided", "two.one.sided"),
                                verbose = 1, pretty = FALSE) {

  alternative <- tolower(match.arg(alternative))
  func.parms <- clean.parms(as.list(environment()))

  check.proportion(prob)
  # null.prob is checked below
  if (!is.null(n)) check.sample.size(n)
  if (!is.null(power)) check.proportion(power)
  check.proportion(alpha)
  check.logical(pretty)
  verbose <- ensure_verbose(verbose)
  requested <- check.n_power(n, power)
  
  len.null.prob <- ifelse(alternative == "two.one.sided", 2, 1)
  if (length(null.prob) != len.null.prob)
    stop(sprintf("`null.prob` must have the length %d if `alternative` is \"%s\".", len.null.prob, alternative), call. = FALSE)
  check.vector(null.prob, check.proportion, len.null.prob)

  ss.exact <- function(prob, null.prob, power, alpha, alternative) {

    n <- power.z.oneprop(prob = prob, null.prob = null.prob, power = power,
                         alpha = alpha, alternative = alternative, verbose = 0)$n
    n <- ceiling(n)

    if (n > 500) {

      steprob20 <- 20
      achieved.power <- 0

      while (achieved.power < power) {

        achieved.power <- power.binom.test(size = n,
                                           prob = prob,
                                           null.prob = null.prob,
                                           alpha = alpha,
                                           alternative = alternative,
                                           plot = FALSE,
                                           verbose = 0)$power

        if (achieved.power < power) n <- n + steprob20

      } # while

      n <- n - steprob20

    } # n > 500

    if (n > 100) {

      step5 <- 5
      achieved.power <- 0

      while (achieved.power < power) {

        achieved.power <- power.binom.test(size = n,
                                           prob = prob,
                                           null.prob = null.prob,
                                           alpha = alpha,
                                           alternative = alternative,
                                           plot = FALSE,
                                           verbose = 0)$power

        if (achieved.power < power) n <- n + step5

      } # while

      n <- n - step5

    } # n > 100

    steprob1 <- 1
    achieved.power <- 0

    while (achieved.power < power) {

      achieved.power <- power.binom.test(size = n,
                                         prob = prob,
                                         null.prob = null.prob,
                                         alpha = alpha,
                                         alternative = alternative,
                                         plot = FALSE,
                                         verbose = 0)$power

      if (achieved.power < power) n <- n + steprob1

    } # n < 50

    n

  } #  ss.exact()

  if (requested == "n") {

    n <- ss.exact(prob = prob, null.prob = null.prob, power = power,
             alpha = alpha, alternative = alternative)

  }

  # calculate power (if requested == "power") or update it (if requested == "n")
  pwr.obj <- power.binom.test(size = n, prob = prob, null.prob = null.prob, alpha = alpha, alternative = alternative,
                              plot = FALSE, verbose = 0)

  power <- pwr.obj$power
  size <- n
  prob.alternative <- prob
  prob.null <- null.prob
  binom.alpha <- pwr.obj$binom.alpha

  delta <- prob - null.prob
  odds.ratio <- (prob / (1 - prob)) /  (null.prob / (1 - null.prob))

  if (verbose > 0) {

    print.obj <- list(requested = requested,
                      test = "One Proportion",
                      alpha = alpha,
                      alt = alternative,
                      method = "exact",
                      delta = delta,
                      odds.ratio = odds.ratio,
                      size = size,
                      prob.alternative = prob.alternative,
                      prob.null = prob.null,
                      binom.alpha = binom.alpha,
                      power = power,
                      n = n)

    if (pretty) {
      .print.pwrss.oneprop(print.obj, verbose = verbose)
    } else {
      .print.ascii.pwrss.oneprop(print.obj, verbose = verbose)
    }

  } # verbose

  invisible(structure(list(parms = func.parms,
                           test = "exact",
                           delta = delta,
                           odds.ratio = odds.ratio,
                           prob = prob.alternative,
                           null.prob = prob.null,
                           size = size,
                           binom.alpha = binom.alpha,
                           power = power,
                           n = n),
                      class = c("pwrss", "exact", "oneprop")))

} # power.exact.oneprop



#####################################
# one proportion approximate z test #
#####################################

power.z.oneprop <- function(prob, null.prob = 0.50,
                            n = NULL, power = NULL, alpha = 0.05,
                            alternative = c("two.sided", "one.sided", "two.one.sided"),
                            std.error = c("null", "alternative"),
                            arcsine = FALSE, correct = FALSE,
                            ceiling = TRUE, verbose = 1, pretty = FALSE) {

  alternative <- tolower(match.arg(alternative))
  std.error <- tolower(match.arg(std.error))
  func.parms <- clean.parms(as.list(environment()))

  check.proportion(prob)
  # null.prob is checked below
  if (!is.null(n)) check.sample.size(n)
  if (!is.null(power)) check.proportion(power)
  check.proportion(alpha)
  check.logical(arcsine, correct, ceiling, pretty)
  verbose <- ensure_verbose(verbose)
  requested <- check.n_power(n, power)

  len.null.prob <- ifelse(alternative == "two.one.sided", 2, 1)
  if (length(null.prob) != len.null.prob)
    stop(sprintf("`null.prob` must have the length %d if `alternative` is \"%s\".", len.null.prob, alternative), call. = FALSE)
  check.vector(null.prob, check.proportion, len.null.prob)

  if (alternative == "two.one.sided" && std.error == "null") {
    std.error <- "alternative"
    warning("`std.error` = \"null\" is ignored. Using \"alternative\" for equivalence or minimal effect testing.", call. = FALSE)
  }

  if (arcsine && correct) warning("Continuity correction does not apply to arcsine transformation approach.", call. = FALSE)

  pwr <- function(prob, null.prob, n, std.error, arcsine, correct, alpha, alternative) {

    if (arcsine) {

      var.num <- 1
      h <- switch(alternative,
                  `two.sided` = probs.to.h(prob, null.prob, FALSE)$h,
                  `one.sided` = probs.to.h(prob, null.prob, FALSE)$h,
                  `two.one.sided` = c(probs.to.h(prob, null.prob[1], FALSE)$h,
                                   probs.to.h(prob, null.prob[2], FALSE)$h))

    } else {

      var.num <- prob * (1 - prob)

      ifelse(alternative == "two.one.sided",
             k <- c(0, 0),
             k <- 0)

      if (correct) {
        ifelse(alternative == "one.sided" && prob > null.prob,
               one.sided.sign <- -1,
               one.sided.sign <- 1)
        k <- switch(alternative,
                    `one.sided` = one.sided.sign / (2 * n),
                    `two.sided` = -1 / (2 * n),
                    `two.one.sided` =  c(-1 / (2 * n), 1 / (2 * n)))
      }

      h <- switch(alternative,
                  `two.sided` = abs(prob - null.prob) + k,
                  `one.sided` = prob - null.prob + k,
                  `two.one.sided` = c(prob - null.prob[1] + k[1], prob - null.prob[2] + k[2]))

    } # if arcsine

    null.dist.sd <- 1

    if (std.error == "null") {

      if (alternative %in% c("two.sided", "one.sided")) {
        null.dist.sd <- sqrt((null.prob * (1 - null.prob)) / (prob * (1 - prob)))
      }

    } # std.error is null


    lambda <- h / sqrt(var.num / n)
    if (alternative %in% c("two.sided", "one.sided")) {
      pwr.obj <- power.z.test(mean = lambda, sd = 1, null.mean = 0, null.sd = null.dist.sd,
                              alpha = alpha, alternative = alternative,
                              plot = FALSE, verbose = 0)
    } else {
      pwr.obj <- power.z.test(mean = 0, sd = 1, null.mean = lambda, null.sd = null.dist.sd,
                              alpha = alpha, alternative = alternative,
                              plot = FALSE, verbose = 0)
    }


    power <- pwr.obj$power
    mean.alternative <- pwr.obj$mean
    sd.alternative <- pwr.obj$sd
    mean.null <- pwr.obj$null.mean
    sd.null <- pwr.obj$null.sd
    z.alpha <- pwr.obj$z.alpha

    list(power = power,  mean.alternative =  mean.alternative,
         sd.alternative = sd.alternative, mean.null = mean.null,
         sd.null = sd.null, z.alpha = z.alpha)

  } # pwr()

  ss.no.correction <- function(prob, null.prob, power, std.error, arcsine, alpha, alternative) {

    beta <- 1 - power

    if (arcsine) {

      var.num <- 1
      h <- switch(alternative,
                  `two.sided` = probs.to.h(prob, null.prob, FALSE)$h,
                  `one.sided` = probs.to.h(prob, null.prob, FALSE)$h,
                  `two.one.sided` = c(probs.to.h(prob, null.prob[1], FALSE)$h,
                                   probs.to.h(prob, null.prob[2], FALSE)$h))

    } else {

      var.num <- prob * (1 - prob)

      h <- switch(alternative,
                  `two.sided` = abs(prob - null.prob),
                  `one.sided` = prob - null.prob,
                  `two.one.sided` = c(prob - null.prob[1], prob - null.prob[2]))

    } # if arcsine

    ifelse(std.error == "null",
           null.dist.sd <- sqrt((null.prob * (1 - null.prob)) / (prob * (1 - prob))),
           null.dist.sd <- 1)

    if (alternative == "two.sided") {

      M <- stats::qnorm(alpha / 2, sd = null.dist.sd, lower.tail = FALSE) +
        stats::qnorm(beta, sd = null.dist.sd, lower.tail = FALSE)
      n <- M ^ 2 * var.num / h ^ 2
    }

    if (alternative == "one.sided") {

      M <- stats::qnorm(alpha, sd = null.dist.sd, lower.tail = FALSE) +
        stats::qnorm(beta, sd = null.dist.sd, lower.tail = FALSE)
      n <- M ^ 2 * var.num / h ^ 2

    }

    if (alternative == "two.one.sided") {

      if (prob > min(null.prob) && prob < max(null.prob)) {
        # equivalence
        M <- stats::qnorm(alpha, sd = null.dist.sd, lower.tail = FALSE) +
          stats::qnorm(beta / 2, sd = null.dist.sd, lower.tail = FALSE)
      } else {
        # minimal effect
        M <- stats::qnorm(alpha / 2, sd = null.dist.sd, lower.tail = FALSE) +
          stats::qnorm(beta, sd = null.dist.sd, lower.tail = FALSE)
      }
      n <- M ^ 2 * var.num / h ^ 2
      n <- max(n)

    }

    n

  } # ss.no.correction()


  ss <- function(prob, null.prob, power, std.error, arcsine, correct, alpha, alternative) {

    n.init <- ss.no.correction(prob = prob, null.prob = null.prob, power = power,
                               std.error = std.error, arcsine = arcsine,
                               alpha = alpha, alternative = alternative)

    n.init <- floor(n.init)

    stop <- FALSE
    sign.previous <- 0
    sign.switch <- 0
    while (isFALSE(stop)) {

      pwr.est <- pwr(prob = prob, null.prob = null.prob, n = n.init,
                     std.error = std.error, arcsine = arcsine, correct = correct,
                     alpha = alpha, alternative = alternative)$power

      sign.current <- sign(pwr.est - power)

      if (sign.current != 0 &&
         sign.previous != 0 &&
         sign.previous != sign.current) {
        sign.switch <-  sign.switch + 1
      }

      ifelse(sign.current > 0,
             n.init <- n.init - 1,
             n.init <- n.init + 1)

      sign.previous <- sign.current

      if (sign.switch >= 2) {
        if (sign.current > 0) n.init <- n.init + 1
        stop <- TRUE
      }

    } # while

    list(n = n.init)

  } # ss()

  if (requested == "n") {

    n <- ss(prob = prob, null.prob = null.prob, power = power, std.error = std.error, arcsine = arcsine,
            correct = correct, alpha = alpha, alternative = alternative)$n

    if (ceiling) n <- ceiling(n)

  }

  # calculate power (if requested == "power") or update it (if requested == "n")
  pwr.obj <- pwr(prob = prob, null.prob = null.prob, n = n, std.error = std.error,
                 arcsine = arcsine, correct = correct, alpha = alpha, alternative = alternative)

  power <- pwr.obj$power
  mean.alternative <- pwr.obj$mean.alternative
  sd.alternative <- pwr.obj$sd.alternative
  mean.null <- pwr.obj$mean.null
  sd.null <- pwr.obj$sd.null
  z.alpha <- pwr.obj$z.alpha

  delta <- prob - null.prob
  odds.ratio <- (prob / (1 - prob)) /  (null.prob / (1 - null.prob))

  if (verbose > 0) {

    print.obj <- list(requested = requested,
                      test = "One Proportion",
                      alpha = alpha,
                      alt = alternative,
                      method = "z",
                      std.error = std.error,
                      arcsine = arcsine,
                      correct = correct,
                      delta = delta,
                      odds.ratio = odds.ratio,
                      mean.alternative = mean.alternative,
                      sd.alternative = sd.alternative,
                      mean.null = mean.null,
                      sd.null = sd.null,
                      z.alpha = z.alpha,
                      power = power,
                      n = n)

    if (pretty) {
      .print.pwrss.oneprop(print.obj, verbose = verbose)
    } else {
      .print.ascii.pwrss.oneprop(print.obj, verbose = verbose)
    }

  } # verbose

  invisible(structure(list(parms = func.parms,
                           test = "z",
                           delta = delta,
                           odds.ratio = odds.ratio,
                           mean = mean.alternative,
                           sd = sd.alternative,
                           null.mean = mean.null,
                           null.sd = sd.null,
                           z.alpha = z.alpha,
                           power = power,
                           n = n),
                      class = c("pwrss", "z", "oneprop")))
} # power.z.oneprop




pwrss.z.prop <- function(p, p0 = 0.50, margin = 0, arcsin.trans = FALSE, alpha = 0.05,
                          alternative = c("not equal", "greater", "less",
                                          "equivalent", "non-inferior", "superior"),
                          n = NULL, power = NULL, verbose = TRUE) {

  alternative <- tolower(match.arg(alternative))
  verbose <- ensure_verbose(verbose)

  if (alternative %in% c("less", "greater", "non-inferior", "superior")) alternative <- "one.sided"
  if (alternative == "not equal") alternative <- "two.sided"
  if (alternative == "equivalent") alternative <- "two.one.sided"
  std.error <- ifelse(alternative == "two.one.sided", "alternative", "null")

  check.numeric(margin)
  check.logical(arcsin.trans)

  if (margin > 0.99 || margin < -0.99) stop("Provide a reasonable margin consistent with `p` - `p0`.", call. = FALSE)

  if (alternative == "two.one.sided") margin <- c(-margin, margin)
  null.prob <- p0 + margin

  prop.obj <-  power.z.oneprop(prob = p, null.prob = null.prob, arcsine = arcsin.trans,
                                      n = n, power = power, alpha = alpha,
                                      alternative = alternative, std.error = std.error,
                                      ceiling = TRUE, verbose = verbose)

  # cat("This function will be removed in the future. \n Please use power.z.oneprop() function. \n")

  return(invisible(prop.obj))

} # pwrss.z.prop()


##############################
# two proportions exact test #
##############################

power.exact.twoprops <- function(prob1, prob2, n.ratio = 1, n2 = NULL,
                                 power = NULL, alpha = 0.05,
                                 alternative = c("two.sided", "one.sided"),
                                 paired = FALSE, rho.paired = 0.50,
                                 method = c("exact", "approximate"),
                                 ceiling = TRUE, verbose = 1, pretty = FALSE) {

  alternative <- tolower(match.arg(alternative))
  method <- tolower(match.arg(method))

  check.proportion(prob1, prob2)
  check.positive(n.ratio)
  if (!is.null(n2)) check.sample.size(n2)
  if (!is.null(power)) check.proportion(power)
  check.proportion(alpha)
  check.logical(paired)
  check.correlation(rho.paired)
  check.logical(ceiling, pretty)
  verbose <- ensure_verbose(verbose)
  requested <- check.n_power(n2, power)

  if (paired) {

    jp <- joint.probs.2x2(prob1 = prob1, prob2 = prob2, rho = rho.paired, verbose = 0)

    power.exact.mcnemar(prob10 = jp$prob10, prob01 = jp$prob01,
                        power = power, n.paired = n2, alpha = alpha,
                        method =  method, alternative = alternative,
                        ceiling = ceiling, verbose = verbose, pretty = pretty)

  } else {

    power.exact.fisher(prob1 = prob1, prob2 = prob2, n2 = n2, n.ratio = n.ratio,
                       alpha = alpha, power = power,
                       alternative = alternative, method = method,
                       ceiling = ceiling, verbose = verbose, pretty = pretty)

  }

} # power.exact.twoprops()
power.exact.twoprop <- power.exact.twoprops


########################
# two proportions test #
########################

power.z.twoprops <- function(prob1, prob2, margin = 0,
                             n.ratio = 1, n2 = NULL,
                             power = NULL, alpha = 0.05,
                             alternative = c("two.sided", "one.sided", "two.one.sided"),
                             arcsine = FALSE, correct = FALSE,
                             paired = FALSE, rho.paired = 0.50,
                             std.error = c("pooled", "unpooled"),
                             ceiling = TRUE, verbose = 1, pretty = FALSE) {

  alternative <- tolower(match.arg(alternative))
  std.error <- tolower(match.arg(std.error))
  func.parms <- clean.parms(as.list(environment()))

  check.proportion(prob1, prob2)
  check.positive(n.ratio)
  if (!is.null(n2)) check.sample.size(n2)
  if (!is.null(power)) check.proportion(power)
  check.proportion(alpha)
  check.logical(arcsine, correct, paired)
  check.correlation(rho.paired)
  check.logical(ceiling, pretty)
  verbose <- ensure_verbose(verbose)  
  requested <- check.n_power(n2, power)

  if (!is.numeric(margin) || any(margin > 0.99) || any(margin < -0.99))
    stop("Provide a reasonable `margin` consistent with `prob1` - `prob2`.", call. = FALSE)
  if (any(abs((prob1 - prob2) - margin) < 1e-6))
    stop("The value of margin should be different from the prob1 - prob2 difference.", call. = FALSE)
  if (alternative == "two.one.sided" && paired)
      stop("Two one-sided tests are currently not available for paired proportions.", call. = FALSE)
  if (alternative == "two.one.sided" && arcsine)
      stop("Arcsine transformation is currently not available for two one-sided tests.", call. = FALSE)
  if (alternative == "two.one.sided" && length(margin) != 2)
      stop("Provide margins in the form of margin = c(lower, upper).", call. = FALSE)
  if (arcsine && correct)
      stop("Continuity correction does not apply to arcsine transformation approach.", call. = FALSE)
  if (arcsine && paired)
      stop("Arcsine transformation is currently not available for paired proportions.", call. = FALSE)
  if (arcsine && margin != 0)
      stop("Arcsine transformation is currently not available for non-zero null.", call. = FALSE)
  if (correct && paired)
    stop("Continuity correction is currently not available for paired proportions.", call. = FALSE)

  pwr <- function(prob1, prob2, margin, n2, n.ratio, arcsine,
                  std.error, correct, alpha, alternative) {

    n1 <- n.ratio * n2

    if (arcsine) {

      stat <- probs.to.h(prob1, prob2, FALSE)$h
      null.stat <- 0

      stderr <- sqrt(1 / n1 + 1 / n2)
      null.dist.sd <- 1

    } else {

      k <- 0

      if (correct) {
        # k <- -sign(prob1 - prob2)
        # k.margin <- -sign(margin)
        # stat <- prob1 - prob2 - 0 + (k / 2) * (1 / n1 + 1 / n2)
        # null.stat <- margin + (k.margin / 2) * (1 / n1 + 1 / n2)
        k <- -sign(prob1 - prob2 - margin)
        stat <- prob1 - prob2 - 0 + (k / 2) * (1 / n1 + 1 / n2)
      } else {
        stat <- prob1 - prob2
      }  # if correct

      null.stat <- margin
      stderr <- sqrt(prob1 * (1 - prob1) / n1 + prob2 * (1 - prob2) / n2)
      if (std.error == "pooled") {
        p.bar <- (prob1 * n1 + prob2 * n2) / (n1 + n2)
        stderr.pooled <- sqrt(p.bar * (1 - p.bar) * (1 / n1 + 1 / n2))
        null.dist.sd <- stderr.pooled / stderr
      } else {
        null.dist.sd <- 1
      }

    } # if arcsine

    lambda <- stat / stderr
    null.lambda <- null.stat / stderr

    pwr.obj <- power.z.test(mean = lambda, sd = 1,
                            null.mean  = null.lambda, null.sd = null.dist.sd,
                            alpha = alpha, alternative =  alternative,
                            plot = FALSE, verbose = 0)

    list(power = pwr.obj$power,
         mean.alternative = pwr.obj$mean,
         sd.alternative = pwr.obj$sd,
         mean.null = pwr.obj$null.mean,
         sd.null = pwr.obj$null.sd,
         z.alpha = pwr.obj$z.alpha)

  } # pwr()


  ss.nocorrection.unpooled <- function(prob1, prob2, margin, power, n.ratio,
                                      arcsine, alpha, alternative) {

    beta <- 1 - power

    if (arcsine) {

      stat <- probs.to.h(prob1, prob2, FALSE)$h

    } else {

      stat <- prob1 - prob2 - margin

    } # if arcsine

    null.dist.sd <- 1

    if (alternative == "one.sided") {

      z.alpha <- stats::qnorm(alpha, sd = null.dist.sd, lower.tail = FALSE)
      z.beta <- stats::qnorm(beta, sd = null.dist.sd, lower.tail = FALSE)

      if (arcsine) {

        n2 <- ((z.alpha + z.beta) ^ 2 / stat ^ 2) * (n.ratio + 1) / n.ratio
        n1 <- n.ratio * n2

      } else {

        n1 <- (z.alpha + z.beta) ^ 2 * (prob1 * (1 - prob1) + prob2 * (1 - prob2) * n.ratio) / stat ^ 2
        n2 <- n1 / n.ratio

      } # if arcsine

    } # if alt

    if (alternative == "two.sided") {


      z.alpha <- stats::qnorm(alpha / 2, sd = null.dist.sd, lower.tail = FALSE)
      z.beta <- stats::qnorm(beta, sd = null.dist.sd, lower.tail = FALSE)

      if (arcsine) {

        n2 <- ((z.alpha + z.beta) ^ 2 / stat ^ 2) * (n.ratio + 1) / n.ratio
        n1 <- n.ratio * n2

      } else {

        n1 <- (z.alpha + z.beta) ^ 2 * (prob1 * (1 - prob1) + prob2 * (1 - prob2) * n.ratio) / stat ^ 2
        n2 <- n1 / n.ratio

      } # if arcsine

    } # if alt

    if (alternative == "two.one.sided") {

      if (prob1 - prob2 > min(margin) && prob1 - prob2 < max(margin)) {
        z.alpha <- stats::qnorm(alpha, sd = null.dist.sd, lower.tail = FALSE)
        z.beta <- stats::qnorm(beta / 2, sd = null.dist.sd, lower.tail = FALSE)
      } else {
        z.alpha <- stats::qnorm(alpha / 2, sd = null.dist.sd, lower.tail = FALSE)
        z.beta <- stats::qnorm(beta, sd = null.dist.sd, lower.tail = FALSE)
      }

# arcsine currently not permitted (l. 556)
#      if (arcsine) {

#        n2 <- ((z.alpha + z.beta) ^ 2 / stat ^ 2) * (n.ratio + 1) / n.ratio
#        n2 <- max(n2)
#        n1 <- n.ratio * n2

#      } else {

      n1 <- (z.alpha + z.beta) ^ 2 * (prob1 * (1 - prob1) + prob2 * (1 - prob2) * n.ratio) / stat ^ 2
      n1 <- max(n1)
      n2 <- n1 / n.ratio

#      } # if arcsine

    } # if alt

    n2 <- n1 / n.ratio

    list(n1 = n1, n2 = n2)

  } # ss.no.correction()

  ss <- function(prob1, prob2, margin,
                 n.ratio, alpha, power, alternative,
                 arcsine, std.error, correct) {

    n2.init <- ss.nocorrection.unpooled(prob1 = prob1, prob2 = prob2, margin = margin,
                                        power = power, n.ratio = n.ratio, alpha = alpha,
                                        arcsine = arcsine, alternative = alternative)$n2

    n2.init <- floor(n2.init)

    stop <- FALSE
    sign.previous <- 0
    sign.switch <- 0
    while (isFALSE(stop)) {

      pwr.est <- pwr(prob1 = prob1, prob2 = prob2, margin = margin,
                     n2 = n2.init, n.ratio = n.ratio, arcsine = arcsine,
                     std.error = std.error, correct = correct,
                     alpha = alpha, alternative = alternative)$power

      sign.current <- sign(pwr.est - power)

      if (sign.current != 0 &&
         sign.previous != 0 &&
         sign.previous != sign.current) {
        sign.switch <-  sign.switch + 1
      }

      ifelse(sign.current > 0,
             n2.init <- n2.init - 1,
             n2.init <- n2.init + 1)

      sign.previous <- sign.current

      if (sign.switch >= 2) {
        if (sign.current > 0) n2.init <- n2.init + 1
        stop <- TRUE
      }

    } # while

    n2 <- n2.init
    n1 <- n2.init * n.ratio

    list(n1 = n1, n2 = n2)

  } # ss()


  if (paired) {

    if (margin != 0) warning("`margin` argument is ignored.", call. = FALSE)

    jp <- joint.probs.2x2(prob1 = prob1, prob2 = prob2, rho = rho.paired, verbose = 0)

    power.exact.mcnemar(prob10 = jp$prob10, prob01 = jp$prob01,
                        power = power, n.paired = n2, alpha = alpha,
                        method =  "approx",
                        alternative = alternative,
                        ceiling = ceiling, verbose = verbose)

  } else {

    if (requested == "n") {

      ss.obj <- ss(prob1 = prob1, prob2 = prob2, margin = margin,
                   power = power, n.ratio = n.ratio, arcsine = arcsine,
                   std.error = std.error, correct = correct,
                   alpha = alpha, alternative = alternative)
      n1 <- ss.obj$n1
      n2 <- ss.obj$n2

      if (ceiling) {
        n1 <- ceiling(n1)
        n2 <- ceiling(n2)
      }
      n.total <- n1 + n2

    } else if (requested == "power") {

       n1 <- ifelse(ceiling, ceiling(n2 * n.ratio), n2 * n.ratio)

    }

    n.ratio <- n1 / n2
    n.total <- n1 + n2

    # calculate power (if requested == "power") or update it (if requested == "n")
    pwr.obj <- pwr(prob1 = prob1, prob2 = prob2, margin = margin, n2 = n2, n.ratio = n.ratio, arcsine = arcsine,
                   std.error = std.error, correct = correct, alpha = alpha, alternative = alternative)

    power <- pwr.obj$power
    mean.alternative <- pwr.obj$mean.alternative
    sd.alternative <- pwr.obj$sd.alternative
    mean.null <- pwr.obj$mean.null
    sd.null <- pwr.obj$sd.null
    z.alpha <- pwr.obj$z.alpha

    delta <- prob1 - prob2
    odds.ratio <- (prob1 / (1 - prob1)) /  (prob2 / (1 - prob2))

    if (verbose > 0) {

      print.obj <- list(requested = requested,
                        test = "Independent Proportions",
                        alpha = alpha,
                        alt = alternative,
                        method = "z",
                        delta = delta,
                        margin = margin,
                        odds.ratio = odds.ratio,
                        mean.alternative = mean.alternative,
                        sd.alternative = sd.alternative,
                        mean.null = mean.null,
                        sd.null = sd.null,
                        z.alpha = z.alpha,
                        power = power,
                        n = c(n1 = n1, n2 = n2),
                        n.total = n.total)

      if (pretty) {
        .print.pwrss.fisher(print.obj, verbose = verbose)
      } else {
        .print.ascii.pwrss.fisher(print.obj, verbose = verbose)
      }

    } # verbose

    invisible(structure(list(parms = func.parms,
                             test = "z",
                             delta = delta,
                             odds.ratio = odds.ratio,
                             mean = mean.alternative,
                             sd = sd.alternative,
                             null.mean = mean.null,
                             null.sd = sd.null,
                             z.alpha = z.alpha,
                             power = power,
                             n = c(n1 = n1, n2 = n2),
                             n.total = n.total),
                        class = c("pwrss", "z", "twoprops")))

  } # if paired

} # power.z.twoprops()
power.z.twoprop <- power.z.twoprops


pwrss.z.2props <- function(p1, p2, margin = 0, arcsin.trans = FALSE,
                           kappa = 1, alpha = 0.05,
                           alternative = c("not equal", "greater", "less",
                                           "equivalent", "non-inferior", "superior"),
                           n2 = NULL, power = NULL, verbose = TRUE) {

  alternative <- tolower(match.arg(alternative))
  verbose <- ensure_verbose(verbose)

  check.proportion(p1, p2, alpha)
  check.positive(kappa)
  check.numeric(margin)
  check.logical(arcsin.trans)

  if (alternative %in% c("less", "greater", "non-inferior", "superior")) alternative <- "one.sided"
  if (alternative == "not equal") alternative <- "two.sided"
  if (alternative == "equivalent") alternative <- "two.one.sided"

  if (alternative == "two.one.sided") margin <- c(-margin, margin)

  twoprops.obj <- power.z.twoprops(prob1 = p1, prob2 = p2,
                                   margin = margin,
                                   n2 = n2, n.ratio = kappa,
                                   power = power, alpha = alpha,
                                   alternative = alternative,
                                   arcsine = arcsin.trans,
                                   correct = FALSE, paired = FALSE,
                                   std.error = "pooled",
                                   ceiling = TRUE, verbose = verbose)

  # cat("This function will be removed in the future. \n Please use power.z.twoprops() function. \n")

  return(invisible(twoprops.obj))

} # pwrss.z.2props()

pwrss.z.2prop <- pwrss.z.2props
