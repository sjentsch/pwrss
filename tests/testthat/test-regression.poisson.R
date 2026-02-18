test_that("regression.poisson.R works", {
    # power.z.poisson (= pwrss.z.poisson)
    crrRes <- power.z.poisson(beta0 = 0.50, beta1 = -0.10, alpha = 0.05, power = 0.80, dist = "normal", verbose = 0)
    expect_equal(class(crrRes), c("pwrss", "z", "poisson"))
    expect_equal(names(crrRes),
                 c("parms", "test", "base.rate", "rate.ratio", "mean", "sd", "vcf", "null.mean", "null.sd", "z.alpha", "power", "n"))
    expect_equal(crrRes[["parms"]],
                 list(base.rate = NULL, rate.ratio = NULL, beta0 = 0.5, beta1 = -0.1, r.squared.predictor = 0,
                      mean.exposure = 1, alpha = 0.05, alternative = "two.sided", method = "demidenko(vc)",
                      distribution = "normal", ceiling = TRUE, verbose = 0, pretty = FALSE))
    expect_equal(crrRes[c("test", "base.rate", "rate.ratio", "mean", "sd", "vcf", "null.mean", "null.sd", "z.alpha", "power", "n")],
                 list(test = "z", base.rate = exp(0.50), rate.ratio = exp(-0.10), mean = -2.80250964, sd = 0.99999976,
                      vcf = 1, null.mean = 0, null.sd = 1, z.alpha = c(-1.959964, 1.959964), power = 0.80025971, n = 474))
    expect_equal(crrRes, pwrss.z.poisson(beta0 = 0.50, beta1 = -0.10, alpha = 0.05, power = 0.80, distribution = "normal", verbose = 0))
    expect_equal(crrRes, pwrss.z.poisreg(beta0 = 0.50, beta1 = -0.10, alpha = 0.05, power = 0.80, distribution = "normal", verbose = 0))

    crrRes <- power.z.poisson(beta0 = 0.50, beta1 = -0.10, alpha = 0.05, alternative = "one.sided", power = 0.80,
                              dist = "normal", verbose = 0)
    expect_equal(class(crrRes), c("pwrss", "z", "poisson"))
    expect_equal(names(crrRes),
                 c("parms", "test", "base.rate", "rate.ratio", "mean", "sd", "vcf", "null.mean", "null.sd", "z.alpha", "power", "n"))
    expect_equal(crrRes[["parms"]],
                 list(base.rate = NULL, rate.ratio = NULL, beta0 = 0.5, beta1 = -0.1, r.squared.predictor = 0,
                      mean.exposure = 1, alpha = 0.05, alternative = "one.sided", method = "demidenko(vc)",
                      distribution = "normal", ceiling = TRUE, verbose = 0, pretty = FALSE))
    expect_equal(crrRes[c("test", "base.rate", "rate.ratio", "mean", "sd", "vcf", "null.mean", "null.sd", "z.alpha", "power", "n")],
                 list(test = "z", base.rate = exp(0.50), rate.ratio = exp(-0.10), mean = -2.4893946, sd = 0.99999976,
                      vcf = 1, null.mean = 0, null.sd = 1, z.alpha = -1.64485362695147, power = 0.800816471, n = 374))
    expect_equal(crrRes, pwrss.z.poisson(beta0 = 0.50, beta1 = -0.10, alpha = 0.05, alternative = "less", power = 0.80,
                                         distribution = "normal", verbose = 0))
    expect_equal(crrRes, pwrss.z.poisreg(beta0 = 0.50, beta1 = -0.10, alpha = 0.05, alternative = "less", power = 0.80,
                                         distribution = "normal", verbose = 0))

    crrRes <- power.z.poisson(beta0 = 0.50, beta1 = -0.10, alpha = 0.05, method = "demidenko", power = 0.80,
                              dist = "normal", verbose = 0)
    expect_equal(class(crrRes), c("pwrss", "z", "poisson"))
    expect_equal(names(crrRes),
                 c("parms", "test", "base.rate", "rate.ratio", "mean", "sd", "vcf", "null.mean", "null.sd", "z.alpha", "power", "n"))
    expect_equal(crrRes[["parms"]],
                 list(base.rate = NULL, rate.ratio = NULL, beta0 = 0.5, beta1 = -0.1, r.squared.predictor = 0,
                      mean.exposure = 1, alpha = 0.05, alternative = "two.sided", method = "demidenko",
                      distribution = "normal", ceiling = TRUE, verbose = 0, pretty = FALSE))
    expect_equal(crrRes[c("test", "base.rate", "rate.ratio", "mean", "sd", "vcf", "null.mean", "null.sd", "z.alpha", "power", "n")],
                 list(test = "z", base.rate = exp(0.50), rate.ratio = exp(-0.10), mean = -2.80250964, sd = 1,
                      vcf = 0, null.mean = 0, null.sd = 1, z.alpha = c(-1.959964, 1.959964), power = 0.80025966, n = 474))

    crrRes <- power.z.poisson(beta0 = 0.50, beta1 = -0.10, alpha = 0.05, method = "signorini", power = 0.80,
                              dist = "normal", verbose = 0)
    expect_equal(class(crrRes), c("pwrss", "z", "poisson"))
    expect_equal(names(crrRes),
                 c("parms", "test", "base.rate", "rate.ratio", "mean", "sd", "vcf", "null.mean", "null.sd", "z.alpha", "power", "n"))
    expect_equal(crrRes[["parms"]],
                 list(base.rate = NULL, rate.ratio = NULL, beta0 = 0.5, beta1 = -0.1, r.squared.predictor = 0,
                      mean.exposure = 1, alpha = 0.05, alternative = "two.sided", method = "signorini",
                      distribution = "normal", ceiling = TRUE, verbose = 0, pretty = FALSE))
    expect_equal(crrRes[c("test", "base.rate", "rate.ratio", "mean", "sd", "vcf", "null.mean", "null.sd", "z.alpha", "power", "n")],
                 list(test = "z", base.rate = exp(0.50), rate.ratio = exp(-0.10), mean = -2.80251031, sd = 1.00000024,
                      vcf = NA, null.mean = 0, null.sd = 1, z.alpha = c(-1.959964, 1.959964), power = 0.80025979, n = 474))

    crrRes <- power.z.poisson(beta0 = 0.50, beta1 = -0.10, alpha = 0.05, n = 474, dist = "normal", verbose = 0)
    expect_equal(class(crrRes), c("pwrss", "z", "poisson"))
    expect_equal(names(crrRes),
                 c("parms", "test", "base.rate", "rate.ratio", "mean", "sd", "vcf", "null.mean", "null.sd", "z.alpha", "power", "n"))
    expect_equal(crrRes[["parms"]],
                 list(base.rate = NULL, rate.ratio = NULL, beta0 = 0.5, beta1 = -0.1, r.squared.predictor = 0,
                      mean.exposure = 1, alpha = 0.05, alternative = "two.sided", method = "demidenko(vc)",
                      distribution = "normal", ceiling = TRUE, verbose = 0, pretty = FALSE))
    expect_equal(crrRes[c("test", "base.rate", "rate.ratio", "mean", "sd", "vcf", "null.mean", "null.sd", "z.alpha", "power", "n")],
                 list(test = "z", base.rate = exp(0.50), rate.ratio = exp(-0.10), mean = -2.80250964, sd = 0.99999976,
                      vcf = 1, null.mean = 0, null.sd = 1, z.alpha = c(-1.959964, 1.959964), power = 0.80025971, n = 474))

    crrRes <- power.z.poisson(base.rate = exp(0.50), rate.ratio = exp(-0.10), alpha = 0.05, power = 0.80,
                              dist = "normal", verbose = 0)
    expect_equal(class(crrRes), c("pwrss", "z", "poisson"))
    expect_equal(names(crrRes),
                 c("parms", "test", "base.rate", "rate.ratio", "mean", "sd", "vcf", "null.mean", "null.sd", "z.alpha", "power", "n"))
    expect_equal(crrRes[["parms"]],
                 list(base.rate = exp(0.50), rate.ratio = exp(-0.10), beta0 = NULL, beta1 = NULL, r.squared.predictor = 0,
                      mean.exposure = 1, alpha = 0.05, alternative = "two.sided", method = "demidenko(vc)",
                      distribution = "normal", ceiling = TRUE, verbose = 0, pretty = FALSE))
    expect_equal(crrRes[c("test", "base.rate", "rate.ratio", "mean", "sd", "vcf", "null.mean", "null.sd", "z.alpha", "power", "n")],
                 list(test = "z", base.rate = exp(0.50), rate.ratio = exp(-0.10), mean = -2.80250964, sd = 0.99999976,
                      vcf = 1, null.mean = 0, null.sd = 1, z.alpha = c(-1.959964, 1.959964), power = 0.80025971, n = 474))

    crrRes <- power.z.poisson(base.rate = exp(0.50), rate.ratio = exp(-0.10), alpha = 0.05, power = 0.80,
                              dist = list(dist = "normal", mean = 10, sd = 2), verbose = 0)
    expect_equal(class(crrRes), c("pwrss", "z", "poisson"))
    expect_equal(names(crrRes),
                 c("parms", "test", "base.rate", "rate.ratio", "mean", "sd", "vcf", "null.mean", "null.sd", "z.alpha", "power", "n"))
    expect_equal(crrRes[["parms"]],
                 list(base.rate = exp(0.50), rate.ratio = exp(-0.10), beta0 = NULL, beta1 = NULL, r.squared.predictor = 0,
                      mean.exposure = 1, alpha = 0.05, alternative = "two.sided", method = "demidenko(vc)",
                      distribution = list(dist = "normal", mean = 10, sd = 2), ceiling = TRUE, verbose = 0, pretty = FALSE))
    expect_equal(crrRes[c("test", "base.rate", "rate.ratio", "mean", "sd", "vcf", "null.mean", "null.sd", "z.alpha", "power", "n")],
                 list(test = "z", base.rate = exp(0.50), rate.ratio = exp(-0.10), mean = -2.8055045, sd = 0.999998665,
                      vcf = 1, null.mean = 0, null.sd = 1, z.alpha = c(-1.959964, 1.959964), power = 0.8010967, n = 318))

    crrRes <- power.z.poisson(beta0 = 0.50, beta1 = -0.10, alpha = 0.05, power = 0.80, dist = "bernoulli", verbose = 0)
    expect_equal(class(crrRes), c("pwrss", "z", "poisson"))
    expect_equal(names(crrRes),
                 c("parms", "test", "base.rate", "rate.ratio", "mean", "sd", "vcf", "null.mean", "null.sd", "z.alpha", "power", "n"))
    expect_equal(crrRes[["parms"]],
                 list(base.rate = NULL, rate.ratio = NULL, beta0 = 0.5, beta1 = -0.1, r.squared.predictor = 0,
                      mean.exposure = 1, alpha = 0.05, alternative = "two.sided", method = "demidenko(vc)",
                      distribution = "bernoulli", ceiling = TRUE, verbose = 0, pretty = FALSE))
    expect_equal(crrRes[c("test", "base.rate", "rate.ratio", "mean", "sd", "vcf", "null.mean", "null.sd", "z.alpha", "power", "n")],
                 list(test = "z", base.rate = exp(0.50), rate.ratio = exp(-0.10), mean = -2.800627935, sd = 0.9987513,
                      vcf = 1, null.mean = 0, null.sd = 1, z.alpha = c(-1.959964, 1.959964), power = 0.80002719, n = 2003))

    crrRes <- power.z.poisson(beta0 = 0.50, beta1 = -0.10, alpha = 0.05, n = 2003, dist = "bernoulli", verbose = 0)
    expect_equal(class(crrRes), c("pwrss", "z", "poisson"))
    expect_equal(names(crrRes),
                 c("parms", "test", "base.rate", "rate.ratio", "mean", "sd", "vcf", "null.mean", "null.sd", "z.alpha", "power", "n"))
    expect_equal(crrRes[["parms"]],
                 list(base.rate = NULL, rate.ratio = NULL, beta0 = 0.5, beta1 = -0.1, r.squared.predictor = 0,
                      mean.exposure = 1, alpha = 0.05, alternative = "two.sided", method = "demidenko(vc)",
                      distribution = "bernoulli", ceiling = TRUE, verbose = 0, pretty = FALSE))
    expect_equal(crrRes[c("test", "base.rate", "rate.ratio", "mean", "sd", "vcf", "null.mean", "null.sd", "z.alpha", "power", "n")],
                 list(test = "z", base.rate = exp(0.50), rate.ratio = exp(-0.10), mean = -2.800627935, sd = 0.9987513,
                      vcf = 1, null.mean = 0, null.sd = 1, z.alpha = c(-1.959964, 1.959964), power = 0.80002719, n = 2003))

    crrRes <- power.z.poisson(base.rate = exp(0.50), rate.ratio = exp(-0.10), alpha = 0.05, power = 0.80,
                              dist = "bernoulli", verbose = 0)
    expect_equal(class(crrRes), c("pwrss", "z", "poisson"))
    expect_equal(names(crrRes),
                 c("parms", "test", "base.rate", "rate.ratio", "mean", "sd", "vcf", "null.mean", "null.sd", "z.alpha", "power", "n"))
    expect_equal(crrRes[["parms"]],
                 list(base.rate = exp(0.50), rate.ratio = exp(-0.10), beta0 = NULL, beta1 = NULL, r.squared.predictor = 0,
                      mean.exposure = 1, alpha = 0.05, alternative = "two.sided", method = "demidenko(vc)",
                      distribution = "bernoulli", ceiling = TRUE, verbose = 0, pretty = FALSE))
    expect_equal(crrRes[c("test", "base.rate", "rate.ratio", "mean", "sd", "vcf", "null.mean", "null.sd", "z.alpha", "power", "n")],
                 list(test = "z", base.rate = exp(0.50), rate.ratio = exp(-0.10), mean = -2.800627935, sd = 0.9987513,
                      vcf = 1, null.mean = 0, null.sd = 1, z.alpha = c(-1.959964, 1.959964), power = 0.80002719, n = 2003))

    crrRes <- power.z.poisson(base.rate = exp(0.50), rate.ratio = exp(-0.10), alpha = 0.05, power = 0.80,
                              dist = list(dist = "bernoulli", prob = 0.30), verbose = 0)
    expect_equal(class(crrRes), c("pwrss", "z", "poisson"))
    expect_equal(names(crrRes),
                 c("parms", "test", "base.rate", "rate.ratio", "mean", "sd", "vcf", "null.mean", "null.sd", "z.alpha", "power", "n"))
    expect_equal(crrRes[["parms"]],
                 list(base.rate = exp(0.50), rate.ratio = exp(-0.10), beta0 = NULL, beta1 = NULL, r.squared.predictor = 0,
                      mean.exposure = 1, alpha = 0.05, alternative = "two.sided", method = "demidenko(vc)",
                      distribution = list(dist = "bernoulli", prob = 0.3), ceiling = TRUE, verbose = 0, pretty = FALSE))
    expect_equal(crrRes[c("test", "base.rate", "rate.ratio", "mean", "sd", "vcf", "null.mean", "null.sd", "z.alpha", "power", "n")],
                 list(test = "z", base.rate = exp(0.50), rate.ratio = exp(-0.10), mean = -2.78435929, sd = 0.979183926,
                      vcf = 1, null.mean = 0, null.sd = 1, z.alpha = c(-1.959964, 1.959964), power = 0.80008449, n = 2404))

    # example 30.3 from the GPower manual
    crrRes <- power.z.poisson(base.rate = 0.85, rate.ratio = 1.3, alpha = 0.05, power = 0.95, alternative = "one.sided",
                              method = "signorini", dist = "binomial", verbose = 0)
    expect_equal(class(crrRes), c("pwrss", "z", "poisson"))
    expect_equal(names(crrRes),
                 c("parms", "test", "base.rate", "rate.ratio", "mean", "sd", "vcf", "null.mean", "null.sd", "z.alpha", "power", "n"))
    expect_equal(crrRes[["parms"]],
                 list(base.rate = 0.85, rate.ratio = 1.3, beta0 = NULL, beta1 = NULL, r.squared.predictor = 0,
                      mean.exposure = 1, alpha = 0.05, alternative = "one.sided", method = "signorini", distribution = "binomial",
                      ceiling = TRUE, verbose = 0, pretty = FALSE))
    expect_equal(crrRes[c("test", "base.rate", "rate.ratio", "mean", "sd", "vcf", "null.mean", "null.sd", "z.alpha", "power", "n")],
                 list(test = "z", base.rate = 0.85, rate.ratio = 1.3, mean = 3.304117, sd = 1.008616722,
                      vcf = NA, null.mean = 0, null.sd = 1, z.alpha = 1.64485363, power = 0.95002418, n = 649))
    expect_equal(power.z.poisson(base.rate = 0.85, rate.ratio = 1.3, alpha = 0.05, power = 0.95, alternative = "one.sided",
                                 method = "demidenko(vc)", dist = "binomial", verbose = 0)$n, 649)
    expect_equal(power.z.poisson(base.rate = 0.85, rate.ratio = 1.3, alpha = 0.05, power = 0.95, alternative = "one.sided",
                                 method = "demidenko", dist = "binomial", verbose = 0)$n, 655)
    # results are not identical to GPower for Signorini (strangely, it gives N = 649 as Demidenko, not N = 697 as expected)
    # for Demidenko, the N's are correct

    # example 30.3 from the GPower manual, Table 2
    pois_pwr <- function(b1, mth, dst) power.z.poisson(beta0 = 0.5, beta1 = b1, alpha = 0.05, n = 200, alternative = "two.sided",
                                                       method = mth, dist = dst, verbose = 0)$power
    expect_equal(vapply(seq(2, 4) * -0.05, function(b1) pois_pwr(b1, "demidenko",     "normal"), numeric(1)),
                 c(0.444590667, 0.782076179, 0.956208934))
    expect_equal(vapply(seq(2, 4) * -0.05, function(b1) pois_pwr(b1, "demidenko(vc)", "normal"), numeric(1)),
                 c(0.444590654, 0.782076336, 0.956209146))
    expect_equal(vapply(seq(2, 4) * -0.05, function(b1) pois_pwr(b1, "signorini",     "normal"), numeric(1)),
                 c(0.444590854, 0.782076575, 0.956209177))
    expect_equal(vapply(c(1, 2, 4) * -0.2, function(b1) pois_pwr(b1, "demidenko",     "uniform"), numeric(1)),
                 c(0.169362111, 0.474561989, 0.928484710))
    expect_equal(vapply(c(1, 2, 4) * -0.2, function(b1) pois_pwr(b1, "demidenko(vc)", "uniform"), numeric(1)),
                 c(0.169105785, 0.474456655, 0.931624379))
    expect_equal(vapply(c(1, 2, 4) * -0.2, function(b1) pois_pwr(b1, "signorini",     "uniform"), numeric(1)),
                 c(0.169863789, 0.477672343, 0.932597708))
    expect_equal(vapply(seq(1, 3) * -0.05, function(b1) pois_pwr(b1, "demidenko",     "lognormal"), numeric(1)),
                 c(0.320220319, 0.695340894, 0.889820796))
    expect_equal(vapply(seq(1, 3) * -0.05, function(b1) pois_pwr(b1, "demidenko(vc)", "lognormal"), numeric(1)),
                 c(0.290596024, 0.746178854, 0.955110662))
    expect_equal(vapply(seq(1, 3) * -0.05, function(b1) pois_pwr(b1, "signorini",     "lognormal"), numeric(1)),
                 c(0.478044368, 0.873283908, 0.977554656))
    expect_equal(vapply(seq(1, 2) * -0.20, function(b1) pois_pwr(b1, "demidenko",     list(dist = "poisson", lambda = 0.5)), numeric(1)),
                 c(0.602872827, 0.972089832))
    expect_equal(vapply(seq(1, 2) * -0.20, function(b1) pois_pwr(b1, "demidenko(vc)", list(dist = "poisson", lambda = 0.5)), numeric(1)),
                 c(0.613395917, 0.990250577))
    expect_equal(vapply(seq(1, 2) * -0.20, function(b1) pois_pwr(b1, "signorini",     list(dist = "poisson", lambda = 0.5)), numeric(1)),
                 c(0.672691558, 0.988326785))
    expect_equal(vapply(seq(1, 2) * -0.20, function(b1) pois_pwr(b1, "demidenko",     list(dist = "bernoulli", prob = 0.2)), numeric(1)),
                 c(0.267784010, 0.691931262))
    expect_equal(vapply(seq(1, 2) * -0.20, function(b1) pois_pwr(b1, "demidenko(vc)", list(dist = "bernoulli", prob = 0.2)), numeric(1)),
                 c(0.254388220, 0.716318213))
    expect_equal(vapply(seq(1, 2) * -0.20, function(b1) pois_pwr(b1, "signorini",     list(dist = "bernoulli", prob = 0.2)), numeric(1)),
                 c(0.308853444, 0.771371852))
    expect_equal(vapply(c(-0.4, -0.7),     function(b1) pois_pwr(b1, "demidenko",     list(dist = "exponential", rate = 3)), numeric(1)),
                 c(0.518688552, 0.871497202))
    expect_equal(vapply(c(-0.4, -0.7),     function(b1) pois_pwr(b1, "demidenko(vc)", list(dist = "exponential", rate = 3)), numeric(1)),
                 c(0.521140107, 0.918937579))
    expect_equal(vapply(c(-0.4, -0.7),     function(b1) pois_pwr(b1, "signorini",     list(dist = "exponential", rate = 3)), numeric(1)),
                 c(0.609332289, 0.933746771))
    # results are not identical to GPower for Signorini; for Demidenko (both with and without VC), the N's are correct

    expect_error(power.z.poisson(beta0 = 0.50, alpha = 0.05, power = 0.80, verbose = 0),
                 "Specify `base.rate` & `rate.ratio` or\n`beta0` & `beta1`")
    expect_message(power.z.poisson(beta0 = 0.50, beta1 = -0.10, base.rate = exp(0.50), alpha = 0.05, power = 0.80, verbose = 0),
                   "Using `beta0` and `beta1`, ignoring any specifications to `base.rate` or `rate.ratio`.")
    expect_message(power.z.poisson(base.rate = exp(0.50), rate.ratio = exp(-0.10), beta0 = 0.50, alpha = 0.05, power = 0.80, verbose = 0),
                   "Using `base.rate` and `rate.ratio`, ignoring any specifications to `beta0` or `beta1`.")
    expect_error(power.z.poisson(base.rate = exp(0.50), rate.ratio = exp(0.50), alpha = 0.05, power = 0.80, verbose = 0),
                 "`beta0` / `base.rate` can not have the same value as `beta1` / `rate.ratio`.")
    expect_error(power.z.poisson(base.rate = exp(0.50), rate.ratio = exp(-0.10), alpha = 0.05, verbose = 0),
                 "`n` and `power` cannot be NULL at the same time.")
    expect_error(power.z.poisson(base.rate = exp(0.50), rate.ratio = exp(-0.10), alpha = 0.05, power = 0.80, n = 200, verbose = 0),
                 "Exactly / only one of the parameters `n` or `power` should be NULL.")
    expect_error(power.z.poisson(base.rate = exp(0.50), rate.ratio = exp(-0.10), alpha = 0.05, power = 0.80,
                                  distribution = list(dist = "normal", mean = 0, sd = 1, err = 1), verbose = 0),
                 "Unknown input type for `distribution`")
    expect_error(power.z.poisson(base.rate = exp(0.50), rate.ratio = exp(-0.10), alpha = 0.05, power = 0.80,
                                  distribution = list(dist = "normal", mean = 0, sdev = 1), verbose = 0),
                 "Unknown input type for `distribution`")
    expect_error(power.z.poisson(base.rate = exp(0.50), rate.ratio = exp(-0.10), alpha = 0.05, power = 0.80, distribution = NA, verbose = 0),
                 "Unknown input type for `distribution`")
})
