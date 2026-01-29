test_that("regression.poisson.R works", {
    # power.z.poisson (= pwrss.z.poisson)
    crrRes <- power.z.poisson(beta0 = 0.50, beta1 = -0.10, alpha = 0.05, power = 0.80, dist = "normal", verbose = FALSE)
    crrOut <- capture.output(power.z.poisson(beta0 = 0.50, beta1 = -0.10, alpha = 0.05, power = 0.80, dist = "normal"))
    crrDtl <- capture.output(power.z.poisson(beta0 = 0.50, beta1 = -0.10, alpha = 0.05, power = 0.80, dist = "normal", verbose = 2))
    crrPty <- capture.output(power.z.poisson(beta0 = 0.50, beta1 = -0.10, alpha = 0.05, power = 0.80, dist = "normal", pretty = TRUE))
    expect_equal(class(crrRes), c("pwrss", "z", "poisson"))
    expect_equal(names(crrRes),
                 c("parms", "test", "base.rate", "rate.ratio", "mean", "sd", "vcf", "null.mean", "null.sd", "z.alpha", "power", "n"))
    expect_equal(crrRes[["parms"]],
                 list(base.rate = exp(0.50), rate.ratio = exp(-0.10), beta0 = 0.5, beta1 = -0.1, r.squared.predictor = 0,
                      mean.exposure = 1, alpha = 0.05, alternative = "two.sided", method = "demidenko(vc)",
                      distribution = list(dist = "normal", mean = 0, sd = 1), ceiling = TRUE, verbose = 0, pretty = FALSE))
    expect_equal(crrRes[c("test", "base.rate", "rate.ratio", "mean", "sd", "vcf", "null.mean", "null.sd", "z.alpha", "power", "n")],
                 list(test = "z", base.rate = exp(0.50), rate.ratio = exp(-0.10), mean = -2.80250964, sd = 0.99999976,
                      vcf = 1, null.mean = 0, null.sd = 1, z.alpha = c(-1.959964, 1.959964), power = 0.80025971, n = 474))
    expect_equal(crrOut, c("+--------------------------------------------------+",
                           "|             SAMPLE SIZE CALCULATION              |",
                           "+--------------------------------------------------+", "",
                           "Poisson Regression Coefficient (Wald's Z-Test)", "",
                           "  Method          : Demidenko (Variance Corrected)",
                           "  Predictor Dist. : Normal", "",
                           "---------------------------------------------------",
                           "Hypotheses",
                           "---------------------------------------------------",
                           "  H0 (Null Claim) : Rate Ratio = 1 ",
                           "  H1 (Alt. Claim) : Rate Ratio != 1 ", "",
                           "---------------------------------------------------",
                           "Results",
                           "---------------------------------------------------",
                           "  Sample Size          = 474  <<",
                           "  Type 1 Error (alpha) = 0.050",
                           "  Type 2 Error (beta)  = 0.200",
                           "  Statistical Power    = 0.8", ""))
    expect_equal(crrDtl, c("+--------------------------------------------------+",
                           "|             SAMPLE SIZE CALCULATION              |",
                           "+--------------------------------------------------+", "",
                           "Poisson Regression Coefficient (Wald's Z-Test)", "",
                           "  Method          : Demidenko (Variance Corrected)",
                           "  Predictor Dist. : Normal", "",
                           "---------------------------------------------------",
                           "Hypotheses",
                           "---------------------------------------------------",
                           "  H0 (Null Claim) : Rate Ratio = 1 ",
                           "  H1 (Alt. Claim) : Rate Ratio != 1 ", "",
                           "---------------------------------------------------",
                           "Key Parameters",
                           "---------------------------------------------------",
                           "  Base Rate            = 1.649 ",
                           "  Rate Ratio           = 0.905 ",
                           "  Var. Corr. Factor    = 1.000",
                           "  Mean of Alt.         = -2.803",
                           "  SD of Alt.           = 1.000",
                           "  Mean of Null         = 0.000",
                           "  SD of Null           = 1.000",
                           "  Critical Value       = -1.96 and 1.96", "",
                           "---------------------------------------------------",
                           "Results",
                           "---------------------------------------------------",
                           "  Sample Size          = 474  <<",
                           "  Type 1 Error (alpha) = 0.050",
                           "  Type 2 Error (beta)  = 0.200",
                           "  Statistical Power    = 0.8", "",
                           "---------------------------------------------------",
                           "Definitions",
                           "---------------------------------------------------",
                           "  Base Rate  = exp(beta0) ",
                           "  Rate Ratio = exp(beta1) ", ""))
    expect_equal(crrPty, c("╔══════════════════════════════════════════════════╗",
                           "║           \033[34m SAMPLE SIZE CALCULATION \033[0m              ║",
                           "╚══════════════════════════════════════════════════╝", "",
                           "Poisson Regression Coefficient (Wald's Z-Test)", "",
                           "  Method           : Demidenko (Variance Corrected)",
                           "  Predictor Dist.  : Normal", "",
                           "───────────────────────────────────────────────────",
                           "Hypotheses",
                           "───────────────────────────────────────────────────",
                           "  H₀ (Null)        : Rate Ratio (RR) = 1",
                           "  H₁ (Alternative) : Rate Ratio (RR) ≠ 1", "",
                           "───────────────────────────────────────────────────",
                           "Results",
                           "───────────────────────────────────────────────────",
                           " \033[34m Sample Size       = 474\033[0m\033[1;35m  ◄◄ ",
                           "\033[0m  Type 1 Error (α)  = 0.050",
                           "  Type 2 Error (β)  = 0.200",
                           "  Statistical Power = 0.8", ""))
    expect_equal(crrOut, capture.output(power.z.poisson(beta0 = 0.50, beta1 = -0.10, alpha = 0.05, power = 0.80, verbose = "J")))
    expect_equal(crrRes, pwrss.z.poisson(beta0 = 0.50, beta1 = -0.10, alpha = 0.05, power = 0.80, distribution = "normal", verbose = FALSE))
    expect_equal(crrRes, pwrss.z.poisreg(beta0 = 0.50, beta1 = -0.10, alpha = 0.05, power = 0.80, distribution = "normal", verbose = FALSE))

    crrRes <- power.z.poisson(beta0 = 0.50, beta1 = -0.10, alpha = 0.05, alternative = "one.sided", power = 0.80,
                              dist = "normal", verbose = FALSE)
    expect_equal(class(crrRes), c("pwrss", "z", "poisson"))
    expect_equal(names(crrRes),
                 c("parms", "test", "base.rate", "rate.ratio", "mean", "sd", "vcf", "null.mean", "null.sd", "z.alpha", "power", "n"))
    expect_equal(crrRes[["parms"]],
                 list(base.rate = exp(0.50), rate.ratio = exp(-0.10), beta0 = 0.5, beta1 = -0.1, r.squared.predictor = 0,
                      mean.exposure = 1, alpha = 0.05, alternative = "one.sided", method = "demidenko(vc)",
                      distribution = list(dist = "normal", mean = 0, sd = 1), ceiling = TRUE, verbose = 0, pretty = FALSE))
    expect_equal(crrRes[c("test", "base.rate", "rate.ratio", "mean", "sd", "vcf", "null.mean", "null.sd", "z.alpha", "power", "n")],
                 list(test = "z", base.rate = exp(0.50), rate.ratio = exp(-0.10), mean = -2.4893946, sd = 0.99999976,
                      vcf = 1, null.mean = 0, null.sd = 1, z.alpha = -1.64485362695147, power = 0.800816471, n = 374))
    expect_equal(crrRes, pwrss.z.poisson(beta0 = 0.50, beta1 = -0.10, alpha = 0.05, alternative = "less", power = 0.80,
                                         distribution = "normal", verbose = FALSE))
    expect_equal(crrRes, pwrss.z.poisreg(beta0 = 0.50, beta1 = -0.10, alpha = 0.05, alternative = "less", power = 0.80,
                                         distribution = "normal", verbose = FALSE))

    crrRes <- power.z.poisson(beta0 = 0.50, beta1 = -0.10, alpha = 0.05, method = "demidenko", power = 0.80,
                              dist = "normal", verbose = FALSE)
    expect_equal(class(crrRes), c("pwrss", "z", "poisson"))
    expect_equal(names(crrRes),
                 c("parms", "test", "base.rate", "rate.ratio", "mean", "sd", "vcf", "null.mean", "null.sd", "z.alpha", "power", "n"))
    expect_equal(crrRes[["parms"]],
                 list(base.rate = exp(0.50), rate.ratio = exp(-0.10), beta0 = 0.5, beta1 = -0.1, r.squared.predictor = 0,
                      mean.exposure = 1, alpha = 0.05, alternative = "two.sided", method = "demidenko",
                      distribution = list(dist = "normal", mean = 0, sd = 1), ceiling = TRUE, verbose = 0, pretty = FALSE))
    expect_equal(crrRes[c("test", "base.rate", "rate.ratio", "mean", "sd", "vcf", "null.mean", "null.sd", "z.alpha", "power", "n")],
                 list(test = "z", base.rate = exp(0.50), rate.ratio = exp(-0.10), mean = -2.80250964, sd = 1,
                      vcf = 0, null.mean = 0, null.sd = 1, z.alpha = c(-1.959964, 1.959964), power = 0.80025966, n = 474))

    crrRes <- power.z.poisson(beta0 = 0.50, beta1 = -0.10, alpha = 0.05, method = "signorini", power = 0.80,
                              dist = "normal", verbose = FALSE)
    expect_equal(class(crrRes), c("pwrss", "z", "poisson"))
    expect_equal(names(crrRes),
                 c("parms", "test", "base.rate", "rate.ratio", "mean", "sd", "vcf", "null.mean", "null.sd", "z.alpha", "power", "n"))
    expect_equal(crrRes[["parms"]],
                 list(base.rate = exp(0.50), rate.ratio = exp(-0.10), beta0 = 0.5, beta1 = -0.1, r.squared.predictor = 0,
                      mean.exposure = 1, alpha = 0.05, alternative = "two.sided", method = "signorini",
                      distribution = list(dist = "normal", mean = 0, sd = 1), ceiling = TRUE, verbose = 0, pretty = FALSE))
    expect_equal(crrRes[c("test", "base.rate", "rate.ratio", "mean", "sd", "vcf", "null.mean", "null.sd", "z.alpha", "power", "n")],
                 list(test = "z", base.rate = exp(0.50), rate.ratio = exp(-0.10), mean = -2.80251031, sd = 1.00000024,
                      vcf = NA, null.mean = 0, null.sd = 1, z.alpha = c(-1.959964, 1.959964), power = 0.80025979, n = 474))

    crrRes <- power.z.poisson(beta0 = 0.50, beta1 = -0.10, alpha = 0.05, n = 474, dist = "normal", verbose = FALSE)
    crrOut <- capture.output(power.z.poisson(beta0 = 0.50, beta1 = -0.10, alpha = 0.05, power = 0.80, dist = "normal"))
    crrDtl <- capture.output(power.z.poisson(beta0 = 0.50, beta1 = -0.10, alpha = 0.05, power = 0.80, dist = "normal", verbose = 2))
    crrPty <- capture.output(power.z.poisson(beta0 = 0.50, beta1 = -0.10, alpha = 0.05, power = 0.80, dist = "normal", pretty = TRUE))
    expect_equal(class(crrRes), c("pwrss", "z", "poisson"))
    expect_equal(names(crrRes),
                 c("parms", "test", "base.rate", "rate.ratio", "mean", "sd", "vcf", "null.mean", "null.sd", "z.alpha", "power", "n"))
    expect_equal(crrRes[["parms"]],
                 list(base.rate = exp(0.50), rate.ratio = exp(-0.10), beta0 = 0.5, beta1 = -0.1, r.squared.predictor = 0,
                      mean.exposure = 1, alpha = 0.05, alternative = "two.sided", method = "demidenko(vc)",
                      distribution = list(dist = "normal", mean = 0, sd = 1), ceiling = TRUE, verbose = 0, pretty = FALSE))
    expect_equal(crrRes[c("test", "base.rate", "rate.ratio", "mean", "sd", "vcf", "null.mean", "null.sd", "z.alpha", "power", "n")],
                 list(test = "z", base.rate = exp(0.50), rate.ratio = exp(-0.10), mean = -2.80250964, sd = 0.99999976,
                      vcf = 1, null.mean = 0, null.sd = 1, z.alpha = c(-1.959964, 1.959964), power = 0.80025971, n = 474))

    crrRes <- power.z.poisson(base.rate = exp(0.50), rate.ratio = exp(-0.10), alpha = 0.05, power = 0.80,
                              dist = "normal", verbose = FALSE)
    expect_equal(class(crrRes), c("pwrss", "z", "poisson"))
    expect_equal(names(crrRes),
                 c("parms", "test", "base.rate", "rate.ratio", "mean", "sd", "vcf", "null.mean", "null.sd", "z.alpha", "power", "n"))
    expect_equal(crrRes[["parms"]],
                 list(base.rate = exp(0.50), rate.ratio = exp(-0.10), beta0 = 0.5, beta1 = -0.1, r.squared.predictor = 0,
                      mean.exposure = 1, alpha = 0.05, alternative = "two.sided", method = "demidenko(vc)",
                      distribution = list(dist = "normal", mean = 0, sd = 1), ceiling = TRUE, verbose = 0, pretty = FALSE))
    expect_equal(crrRes[c("test", "base.rate", "rate.ratio", "mean", "sd", "vcf", "null.mean", "null.sd", "z.alpha", "power", "n")],
                 list(test = "z", base.rate = exp(0.50), rate.ratio = exp(-0.10), mean = -2.80250964, sd = 0.99999976,
                      vcf = 1, null.mean = 0, null.sd = 1, z.alpha = c(-1.959964, 1.959964), power = 0.80025971, n = 474))
     
    crrRes <- power.z.poisson(base.rate = exp(0.50), rate.ratio = exp(-0.10), alpha = 0.05, power = 0.80,
                              dist = list(dist = "normal", mean = 10, sd = 2), verbose = FALSE)
    expect_equal(class(crrRes), c("pwrss", "z", "poisson"))
    expect_equal(names(crrRes),
                 c("parms", "test", "base.rate", "rate.ratio", "mean", "sd", "vcf", "null.mean", "null.sd", "z.alpha", "power", "n"))
    expect_equal(crrRes[["parms"]],
                 list(base.rate = exp(0.50), rate.ratio = exp(-0.10), beta0 = 0.5, beta1 = -0.1, r.squared.predictor = 0,
                      mean.exposure = 1, alpha = 0.05, alternative = "two.sided", method = "demidenko(vc)",
                      distribution = list(dist = "normal", mean = 10, sd = 2), ceiling = TRUE, verbose = 0, pretty = FALSE))
    expect_equal(crrRes[c("test", "base.rate", "rate.ratio", "mean", "sd", "vcf", "null.mean", "null.sd", "z.alpha", "power", "n")],
                 list(test = "z", base.rate = exp(0.50), rate.ratio = exp(-0.10), mean = -2.8055045, sd = 0.999998665,
                      vcf = 1, null.mean = 0, null.sd = 1, z.alpha = c(-1.959964, 1.959964), power = 0.8010967, n = 318))    

    crrRes <- power.z.poisson(beta0 = 0.50, beta1 = -0.10, alpha = 0.05, power = 0.80, dist = "bernoulli", verbose = FALSE)
    expect_equal(class(crrRes), c("pwrss", "z", "poisson"))
    expect_equal(names(crrRes),
                 c("parms", "test", "base.rate", "rate.ratio", "mean", "sd", "vcf", "null.mean", "null.sd", "z.alpha", "power", "n"))
    expect_equal(crrRes[["parms"]],
                 list(base.rate = exp(0.50), rate.ratio = exp(-0.10), beta0 = 0.5, beta1 = -0.1, r.squared.predictor = 0,
                      mean.exposure = 1, alpha = 0.05, alternative = "two.sided", method = "demidenko(vc)",
                      distribution = list(dist = "bernoulli", prob = 0.5), ceiling = TRUE, verbose = 0, pretty = FALSE))
    expect_equal(crrRes[c("test", "base.rate", "rate.ratio", "mean", "sd", "vcf", "null.mean", "null.sd", "z.alpha", "power", "n")],
                 list(test = "z", base.rate = exp(0.50), rate.ratio = exp(-0.10), mean = -2.800627935, sd = 0.9987513,
                      vcf = 1, null.mean = 0, null.sd = 1, z.alpha = c(-1.959964, 1.959964), power = 0.80002719, n = 2003))    
     
    crrRes <- power.z.poisson(beta0 = 0.50, beta1 = -0.10, alpha = 0.05, n = 2003, dist = "bernoulli", verbose = FALSE)
    expect_equal(class(crrRes), c("pwrss", "z", "poisson"))
    expect_equal(names(crrRes),
                 c("parms", "test", "base.rate", "rate.ratio", "mean", "sd", "vcf", "null.mean", "null.sd", "z.alpha", "power", "n"))
    expect_equal(crrRes[["parms"]],
                 list(base.rate = exp(0.50), rate.ratio = exp(-0.10), beta0 = 0.5, beta1 = -0.1, r.squared.predictor = 0,
                      mean.exposure = 1, alpha = 0.05, alternative = "two.sided", method = "demidenko(vc)",
                      distribution = list(dist = "bernoulli", prob = 0.5), ceiling = TRUE, verbose = 0, pretty = FALSE))
    expect_equal(crrRes[c("test", "base.rate", "rate.ratio", "mean", "sd", "vcf", "null.mean", "null.sd", "z.alpha", "power", "n")],
                 list(test = "z", base.rate = exp(0.50), rate.ratio = exp(-0.10), mean = -2.800627935, sd = 0.9987513,
                      vcf = 1, null.mean = 0, null.sd = 1, z.alpha = c(-1.959964, 1.959964), power = 0.80002719, n = 2003))    
     
    crrRes <- power.z.poisson(base.rate = exp(0.50), rate.ratio = exp(-0.10), alpha = 0.05, power = 0.80,
                              dist = "bernoulli", verbose = FALSE)
    expect_equal(class(crrRes), c("pwrss", "z", "poisson"))
    expect_equal(names(crrRes),
                 c("parms", "test", "base.rate", "rate.ratio", "mean", "sd", "vcf", "null.mean", "null.sd", "z.alpha", "power", "n"))
    expect_equal(crrRes[["parms"]],
                 list(base.rate = exp(0.50), rate.ratio = exp(-0.10), beta0 = 0.5, beta1 = -0.1, r.squared.predictor = 0,
                      mean.exposure = 1, alpha = 0.05, alternative = "two.sided", method = "demidenko(vc)",
                      distribution = list(dist = "bernoulli", prob = 0.5), ceiling = TRUE, verbose = 0, pretty = FALSE))
    expect_equal(crrRes[c("test", "base.rate", "rate.ratio", "mean", "sd", "vcf", "null.mean", "null.sd", "z.alpha", "power", "n")],
                 list(test = "z", base.rate = exp(0.50), rate.ratio = exp(-0.10), mean = -2.800627935, sd = 0.9987513,
                      vcf = 1, null.mean = 0, null.sd = 1, z.alpha = c(-1.959964, 1.959964), power = 0.80002719, n = 2003))    

    crrRes <- power.z.poisson(base.rate = exp(0.50), rate.ratio = exp(-0.10), alpha = 0.05, power = 0.80,
                              dist = list(dist = "bernoulli", prob = 0.30), verbose = FALSE)
    expect_equal(class(crrRes), c("pwrss", "z", "poisson"))
    expect_equal(names(crrRes),
                 c("parms", "test", "base.rate", "rate.ratio", "mean", "sd", "vcf", "null.mean", "null.sd", "z.alpha", "power", "n"))
    expect_equal(crrRes[["parms"]],
                 list(base.rate = exp(0.50), rate.ratio = exp(-0.10), beta0 = 0.5, beta1 = -0.1, r.squared.predictor = 0,
                      mean.exposure = 1, alpha = 0.05, alternative = "two.sided", method = "demidenko(vc)",
                      distribution = list(dist = "bernoulli", prob = 0.3), ceiling = TRUE, verbose = 0, pretty = FALSE))
    expect_equal(crrRes[c("test", "base.rate", "rate.ratio", "mean", "sd", "vcf", "null.mean", "null.sd", "z.alpha", "power", "n")],
                 list(test = "z", base.rate = exp(0.50), rate.ratio = exp(-0.10), mean = -2.78435929, sd = 0.979183926,
                      vcf = 1, null.mean = 0, null.sd = 1, z.alpha = c(-1.959964, 1.959964), power = 0.80008449, n = 2404))    

    expect_error(power.z.poisson(beta0 = 0.50, alpha = 0.05, power = 0.80, verbose = FALSE),
                 "Specify `base.rate` & `rate.ratio` or\n`beta0` & `beta1`")
    expect_message(power.z.poisson(beta0 = 0.50, beta1 = -0.10, base.rate = exp(0.50), alpha = 0.05, power = 0.80, verbose = FALSE),
                   "Using `beta0` and `beta1`, ignoring any specifications to `base.rate` or `rate.ratio`.")
    expect_message(power.z.poisson(base.rate = exp(0.50), rate.ratio = exp(-0.10), beta0 = 0.50, alpha = 0.05, power = 0.80, verbose = FALSE),
                   "Using `base.rate` and `rate.ratio`, ignoring any specifications to `beta0` or `beta1`.")
    expect_error(power.z.poisson(base.rate = exp(0.50), rate.ratio = exp(0.50), alpha = 0.05, power = 0.80, verbose = FALSE),
                 "`beta0` / `base.rate` can not have the same value as `beta1` / `rate.ratio`.")
    expect_error(power.z.poisson(base.rate = exp(0.50), rate.ratio = exp(-0.10), alpha = 0.05, verbose = FALSE),
                 "`n` and `power` cannot be NULL at the same time.")
    expect_error(power.z.poisson(base.rate = exp(0.50), rate.ratio = exp(-0.10), alpha = 0.05, power = 0.80, n = 200, verbose = FALSE),
                 "Exactly one of the `n` or `power` should be NULL.")
    expect_error(power.z.poisson(base.rate = exp(0.50), rate.ratio = exp(-0.10), alpha = 0.05, power = 0.80,
                                  distribution = list(dist = "normal", mean = 0, sd = 1, err = 1), verbose = FALSE),
                 "Unknown input type for `distribution`")
    expect_error(power.z.poisson(base.rate = exp(0.50), rate.ratio = exp(-0.10), alpha = 0.05, power = 0.80,
                                  distribution = list(dist = "normal", mean = 0, sdev = 1), verbose = FALSE),
                 "Unknown input type for `distribution`")
    expect_error(power.z.poisson(base.rate = exp(0.50), rate.ratio = exp(-0.10), alpha = 0.05, power = 0.80, distribution = NA, verbose = FALSE),
                 "Unknown input type for `distribution`")
})
