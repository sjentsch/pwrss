test_that("regression.logistic.R works", {
    # power.z.logistic (= pwrss.z.logistic / pwrss.z.logreg)
    crrRes <- power.z.logistic(base.prob = 0.15, prob = 0.20, alpha = 0.05, power = 0.80, distribution = "normal", verbose = FALSE)
    crrOut <- capture.output(power.z.logistic(base.prob = 0.15, prob = 0.20, alpha = 0.05, power = 0.80, distribution = "normal"))
    crrDtl <- capture.output(power.z.logistic(base.prob = 0.15, prob = 0.20, alpha = 0.05, power = 0.80, distribution = "normal", verbose = 2))
    crrPty <- capture.output(power.z.logistic(base.prob = 0.15, prob = 0.20, alpha = 0.05, power = 0.80, distribution = "normal", pretty = TRUE))
    expect_equal(class(crrRes), c("pwrss", "z", "logistic"))
    expect_equal(names(crrRes), c("parms", "test", "odds.ratio", "mean", "sd", "vcf", "null.mean", "null.sd", "z.alpha", "power", "n"))
    expect_equal(crrRes[["parms"]],
                 list(base.prob = 0.15, prob = 0.2, beta0 = -1.734601055, beta1 = 0.348306694, odds.ratio = 1.41666667,
                      r.squared.predictor = 0, alpha = 0.05, alternative = "two.sided", method = "demidenko(vc)",
                      distribution = list(dist = "normal", mean = 0, sd = 1), verbose = 0))
    expect_equal(crrRes[c("test", "odds.ratio", "mean", "sd", "vcf", "null.mean", "null.sd", "z.alpha", "power", "n")],
                 list(test = "z", odds.ratio = 1.41666667, mean = 2.78366921, sd = 0.976071627, vcf = 1, null.mean = 0, null.sd = 1,
                      z.alpha = c(-1.959964, 1.959964), power = 0.800637479, n = 511))
    expect_equal(crrOut, c("+--------------------------------------------------+",
                           "|             SAMPLE SIZE CALCULATION              |",
                           "+--------------------------------------------------+", "",
                           "Logistic Regression Coefficient (Wald's Z-Test)", "",
                           "  Method          : Demidenko (Variance Corrected)",
                           "  Predictor Dist. : Normal", "",
                           "---------------------------------------------------",
                           "Hypotheses",
                           "---------------------------------------------------",
                           "  H0 (Null Claim) : Odds Ratio = 1",
                           "  H1 (Alt. Claim) : Odds Ratio != 1", "",
                           "---------------------------------------------------",
                           "Results",
                           "---------------------------------------------------",
                           "  Sample Size          = 511  <<",
                           "  Type 1 Error (alpha) = 0.050",
                           "  Type 2 Error (beta)  = 0.199",
                           "  Statistical Power    = 0.801", ""))
    expect_equal(crrDtl, c("+--------------------------------------------------+",
                           "|             SAMPLE SIZE CALCULATION              |",
                           "+--------------------------------------------------+", "",
                           "Logistic Regression Coefficient (Wald's Z-Test)", "",
                           "  Method          : Demidenko (Variance Corrected)",
                           "  Predictor Dist. : Normal", "",
                           "---------------------------------------------------",
                           "Hypotheses",
                           "---------------------------------------------------",
                           "  H0 (Null Claim) : Odds Ratio = 1",
                           "  H1 (Alt. Claim) : Odds Ratio != 1", "",
                           "---------------------------------------------------",
                           "Key Parameters ",
                           "---------------------------------------------------",
                           "  Base Probability     = 0.150 ",
                           "  Odds Ratio           = 1.417 ",
                           "  Var. Corr. Factor    = 1.000 ",
                           "  Mean of Alt.         = 2.784 ",
                           "  SD of Alt.           = 0.976 ",
                           "  Mean of Null         = 0.000 ",
                           "  SD of Null           = 1.000 ",
                           "  Critical Value       = -1.96 and 1.96 ", "",
                           "---------------------------------------------------",
                           "Results",
                           "---------------------------------------------------",
                           "  Sample Size          = 511  <<",
                           "  Type 1 Error (alpha) = 0.050",
                           "  Type 2 Error (beta)  = 0.199",
                           "  Statistical Power    = 0.801", "",
                           "---------------------------------------------------",
                           "Definitions",
                           "---------------------------------------------------",
                           "  Odds Ratio = [prob/(1-prob)] / [base.prob/(1-base.prob)] ",
                           "  prob       : Base probability when predictor = 0 ",
                           "  base.prob  : Probability when predictor = 1 ",
                           "  beta1      = log(Odds Ratio) ",
                           "  beta0      = log[base.prob/(1-base.prob)] ", ""))
    expect_equal(crrPty, c("╔══════════════════════════════════════════════════╗",
                           "║           \033[34m SAMPLE SIZE CALCULATION \033[0m              ║",
                           "╚══════════════════════════════════════════════════╝", "",
                           "Logistic Regression Coefficient (Wald's Z-Test)", "",
                           "  Method           : Demidenko (Variance Corrected)",
                           "  Predictor Dist.  : Normal", "",
                           "───────────────────────────────────────────────────",
                           "Hypotheses",
                           "───────────────────────────────────────────────────",
                           "  H₀ (Null)        : Odds Ratio (OR) = 1",
                           "  H₁ (Alternative) : Odds Ratio (OR) ≠ 1", "",
                           "───────────────────────────────────────────────────",
                           "Results",
                           "───────────────────────────────────────────────────",
                           " \033[34m Sample Size       = 511\033[0m\033[1;35m  ◄◄ ",
                           "\033[0m  Type 1 Error (α)  = 0.050",
                           "  Type 2 Error (β)  = 0.199",
                           "  Statistical Power = 0.801", ""))
    expect_equal(crrOut, capture.output(power.z.logistic(base.prob = 0.15, prob = 0.20, alpha = 0.05, power = 0.80, verbose = "J")))
    expect_equal(crrRes, pwrss.z.logistic(p0 = 0.15, p1 = 0.20, alpha = 0.05, power = 0.80, distribution = "normal", verbose = FALSE))
    expect_equal(crrRes, pwrss.z.logreg(p0 = 0.15, p1 = 0.20, alpha = 0.05, power = 0.80, distribution = "normal", verbose = FALSE))

    crrRes <- power.z.logistic(base.prob = 0.15, prob = 0.20, alpha = 0.05, power = 0.80, distribution = "normal",
                               method = "demidenko", verbose = FALSE)
    expect_equal(class(crrRes), c("pwrss", "z", "logistic"))
    expect_equal(names(crrRes), c("parms", "test", "odds.ratio", "mean", "sd", "vcf", "null.mean", "null.sd", "z.alpha", "power", "n"))
    expect_equal(crrRes[["parms"]],
                 list(base.prob = 0.15, prob = 0.2, beta0 = -1.734601055, beta1 = 0.348306694, odds.ratio = 1.41666667,
                      r.squared.predictor = 0, alpha = 0.05, alternative = "two.sided", method = "demidenko",
                      distribution = list(dist = "normal", mean = 0, sd = 1), verbose = 0))
    expect_equal(crrRes[c("test", "odds.ratio", "mean", "sd", "vcf", "null.mean", "null.sd", "z.alpha", "power", "n")],
                 list(test = "z", odds.ratio = 1.41666667, mean = 2.80267059, sd = 1, vcf = 0, null.mean = 0, null.sd = 1,
                      z.alpha = c(-1.959964, 1.959964), power = 0.800304678, n = 518))

    crrRes <- power.z.logistic(base.prob = 0.15, prob = 0.20, alpha = 0.05, power = 0.80, distribution = "normal",
                               method = "hsieh", verbose = FALSE)
    expect_equal(class(crrRes), c("pwrss", "z", "logistic"))
    expect_equal(names(crrRes), c("parms", "test", "odds.ratio", "mean", "sd", "vcf", "null.mean", "null.sd", "z.alpha", "power", "n"))
    expect_equal(crrRes[["parms"]],
                 list(base.prob = 0.15, prob = 0.2, beta0 = -1.734601055, beta1 = 0.348306694, odds.ratio = 1.41666667,
                      r.squared.predictor = 0, alpha = 0.05, alternative = "two.sided", method = "hsieh",
                      distribution = list(dist = "normal", mean = 0, sd = 1), verbose = 0))
    expect_equal(crrRes[c("test", "odds.ratio", "mean", "sd", "vcf", "null.mean", "null.sd", "z.alpha", "power", "n")],
                 list(test = "z", odds.ratio = 1.41666667, mean = 2.80158522, sd = 1, vcf = NA, null.mean = 0, null.sd = 1,
                      z.alpha = 1.95996398454, power = 0.800442373, n = 508))

    crrRes <- power.z.logistic(base.prob = 0.15, prob = 0.20, alpha = 0.05, n = 511, distribution = "normal", verbose = FALSE)
    expect_equal(class(crrRes), c("pwrss", "z", "logistic"))
    expect_equal(names(crrRes), c("parms", "test", "odds.ratio", "mean", "sd", "vcf", "null.mean", "null.sd", "z.alpha", "power", "n"))
    expect_equal(crrRes[["parms"]],
                 list(base.prob = 0.15, prob = 0.2, beta0 = -1.734601055, beta1 = 0.348306694, odds.ratio = 1.41666667,
                      r.squared.predictor = 0, alpha = 0.05, alternative = "two.sided", method = "demidenko(vc)",
                      distribution = list(dist = "normal", mean = 0, sd = 1), verbose = 0))
    expect_equal(crrRes[c("test", "odds.ratio", "mean", "sd", "vcf", "null.mean", "null.sd", "z.alpha", "power", "n")],
                 list(test = "z", odds.ratio = 1.41666667, mean = 2.78366921, sd = 0.976071627, vcf = 1, null.mean = 0, null.sd = 1,
                      z.alpha = c(-1.959964, 1.959964), power = 0.800637479, n = 511))

    crrRes <- power.z.logistic(base.prob = 0.15, odds.ratio = 1.416667, alpha = 0.05, power = 0.80, distribution = "normal", verbose = FALSE)
    expect_equal(class(crrRes), c("pwrss", "z", "logistic"))
    expect_equal(names(crrRes), c("parms", "test", "odds.ratio", "mean", "sd", "vcf", "null.mean", "null.sd", "z.alpha", "power", "n"))
    expect_equal(crrRes[["parms"]],
                 list(base.prob = 0.15, prob = 0.200000038, beta0 = -1.734601055, beta1 = 0.34830693, odds.ratio = 1.416667,
                      r.squared.predictor = 0, alpha = 0.05, alternative = "two.sided", method = "demidenko(vc)",
                      distribution = list(dist = "normal", mean = 0, sd = 1), verbose = 0))
    expect_equal(crrRes[c("test", "odds.ratio", "mean", "sd", "vcf", "null.mean", "null.sd", "z.alpha", "power", "n")],
                 list(test = "z", odds.ratio = 1.416667, mean = 2.783671049, sd = 0.976071594, vcf = 1, null.mean = 0, null.sd = 1,
                      z.alpha = c(-1.959964, 1.959964), power = 0.80063801, n = 511))
     
    crrRes <- power.z.logistic(beta0 = -1.734601, beta1 = 0.3483067, alpha = 0.05, power = 0.80, distribution = "normal", verbose = FALSE)
    expect_equal(class(crrRes), c("pwrss", "z", "logistic"))
    expect_equal(names(crrRes), c("parms", "test", "odds.ratio", "mean", "sd", "vcf", "null.mean", "null.sd", "z.alpha", "power", "n"))
    expect_equal(crrRes[["parms"]],
                 list(base.prob = 0.150000007, prob = 0.200000010, beta0 = -1.734601, beta1 = 0.3483067, odds.ratio = 1.41666667,
                      r.squared.predictor = 0, alpha = 0.05, alternative = "two.sided", method = "demidenko(vc)",
                      distribution = list(dist = "normal", mean = 0, sd = 1), verbose = 0))
    expect_equal(crrRes[c("test", "odds.ratio", "mean", "sd", "vcf", "null.mean", "null.sd", "z.alpha", "power", "n")],
                 list(test = "z", odds.ratio = 1.41666667, mean = 2.783669306, sd = 0.976071627, vcf = 1, null.mean = 0, null.sd = 1,
                      z.alpha = c(-1.959964, 1.959964), power = 0.800637507, n = 511))
     
    crrRes <- power.z.logistic(base.prob = 0.15, beta1 = 0.3483067, alpha = 0.05, power = 0.80,
                               distribution = list(dist = "normal", mean = 10, sd = 2), verbose = FALSE)
    expect_equal(class(crrRes), c("pwrss", "z", "logistic"))
    expect_equal(names(crrRes), c("parms", "test", "odds.ratio", "mean", "sd", "vcf", "null.mean", "null.sd", "z.alpha", "power", "n"))
    expect_equal(crrRes[["parms"]],
                 list(base.prob = 0.15, prob = 0.2, beta0 = -1.734601055, beta1 = 0.3483067, odds.ratio = 1.41666667,
                      r.squared.predictor = 0, alpha = 0.05, alternative = "two.sided", method = "demidenko(vc)",
                      distribution = list(dist = "normal", mean = 10, sd = 2), verbose = 0))
    expect_equal(crrRes[c("test", "odds.ratio", "mean", "sd", "vcf", "null.mean", "null.sd", "z.alpha", "power", "n")],
                 list(test = "z", odds.ratio = 1.41666667, mean = 2.7241141, sd = 0.902808332, vcf = 1, null.mean = 0, null.sd = 1,
                      z.alpha = c(-1.959964, 1.959964), power = 0.80133934, n = 134))
     
    crrRes <- power.z.logistic(base.prob = 0.15, odds.ratio = 1.416667, alpha = 0.05, power = 0.80, distribution = "bernoulli", verbose = FALSE)
    expect_equal(class(crrRes), c("pwrss", "z", "logistic"))
    expect_equal(names(crrRes), c("parms", "test", "odds.ratio", "mean", "sd", "vcf", "null.mean", "null.sd", "z.alpha", "power", "n"))
    expect_equal(crrRes[["parms"]],
                 list(base.prob = 0.15, prob = 0.200000038, beta0 = -1.734601055, beta1 = 0.34830693, odds.ratio = 1.416667,
                      r.squared.predictor = 0, alpha = 0.05, alternative = "two.sided", method = "demidenko(vc)",
                      distribution = list(dist = "bernoulli", prob = 0.5), verbose = 0))
    expect_equal(crrRes[c("test", "odds.ratio", "mean", "sd", "vcf", "null.mean", "null.sd", "z.alpha", "power", "n")],
                 list(test = "z", odds.ratio = 1.416667, mean = 2.7957685, sd = 0.992726219, vcf = 0.85, null.mean = 0, null.sd = 1,
                      z.alpha = c(-1.959964, 1.959964), power = 0.80008686, n = 1816))

    crrRes <- power.z.logistic(base.prob = 0.15, odds.ratio = 1.416667, alpha = 0.05, power = 0.80, distribution = "bernoulli",
                               method = "demidenko", verbose = FALSE)
    expect_equal(class(crrRes), c("pwrss", "z", "logistic"))
    expect_equal(names(crrRes), c("parms", "test", "odds.ratio", "mean", "sd", "vcf", "null.mean", "null.sd", "z.alpha", "power", "n"))
    expect_equal(crrRes[["parms"]],
                 list(base.prob = 0.15, prob = 0.200000038, beta0 = -1.734601055, beta1 = 0.34830693, odds.ratio = 1.416667,
                      r.squared.predictor = 0, alpha = 0.05, alternative = "two.sided", method = "demidenko",
                      distribution = list(dist = "bernoulli", prob = 0.5), verbose = 0))
    expect_equal(crrRes[c("test", "odds.ratio", "mean", "sd", "vcf", "null.mean", "null.sd", "z.alpha", "power", "n")],
                 list(test = "z", odds.ratio = 1.416667, mean = 2.801919840, sd = 1, vcf = 0, null.mean = 0, null.sd = 1,
                      z.alpha = c(-1.959964, 1.959964), power = 0.800094628, n = 1824))

    crrRes <- power.z.logistic(base.prob = 0.15, odds.ratio = 1.416667, alpha = 0.05, power = 0.80, distribution = "bernoulli",
                               method = "hsieh", verbose = FALSE)
    expect_equal(class(crrRes), c("pwrss", "z", "logistic"))
    expect_equal(names(crrRes), c("parms", "test", "odds.ratio", "mean", "sd", "vcf", "null.mean", "null.sd", "z.alpha", "power", "n"))
    expect_equal(crrRes[["parms"]],
                 list(base.prob = 0.15, prob = 0.200000038, beta0 = -1.734601055, beta1 = 0.34830693, odds.ratio = 1.416667,
                      r.squared.predictor = 0, alpha = 0.05, alternative = "two.sided", method = "hsieh",
                      distribution = list(dist = "bernoulli", prob = 0.5), verbose = 0))
    expect_equal(crrRes[c("test", "odds.ratio", "mean", "sd", "vcf", "null.mean", "null.sd", "z.alpha", "power", "n")],
                 list(test = "z", odds.ratio = 1.416667, mean = 2.80158522, sd = 1, vcf = NA, null.mean = 0, null.sd = 1,
                      z.alpha = 1.959964, power = 0.8015040, n = 54))

    crrRes <- power.z.logistic(base.prob = 0.15, odds.ratio = 1.416667, alpha = 0.05, n = 54, distribution = "bernoulli",
                               method = "hsieh", verbose = FALSE)
    expect_equal(class(crrRes), c("pwrss", "z", "logistic"))
    expect_equal(names(crrRes), c("parms", "test", "odds.ratio", "mean", "sd", "vcf", "null.mean", "null.sd", "z.alpha", "power", "n"))
    expect_equal(crrRes[["parms"]],
                 list(base.prob = 0.15, prob = 0.200000038, beta0 = -1.734601055, beta1 = 0.34830693, odds.ratio = 1.416667,
                      r.squared.predictor = 0, alpha = 0.05, alternative = "two.sided", method = "hsieh",
                      distribution = list(dist = "bernoulli", prob = 0.5), verbose = 0))
    expect_equal(crrRes[c("test", "odds.ratio", "mean", "sd", "vcf", "null.mean", "null.sd", "z.alpha", "power", "n")],
                 list(test = "z", odds.ratio = 1.416667, mean = 2.80696955, sd = 1, vcf = NA, null.mean = 0, null.sd = 1,
                      z.alpha = 1.959964, power = 0.8015040, n = 54))

    crrRes <- power.z.logistic(base.prob = 0.15, odds.ratio = 1.416667, alpha = 0.05, power = 0.80,
                               distribution = list(dist = "bernoulli", prob = 0.30), verbose = FALSE)
    expect_equal(class(crrRes), c("pwrss", "z", "logistic"))
    expect_equal(names(crrRes), c("parms", "test", "odds.ratio", "mean", "sd", "vcf", "null.mean", "null.sd", "z.alpha", "power", "n"))
    expect_equal(crrRes[["parms"]],
                 list(base.prob = 0.15, prob = 0.200000038, beta0 = -1.734601055, beta1 = 0.34830693, odds.ratio = 1.416667,
                      r.squared.predictor = 0, alpha = 0.05, alternative = "two.sided", method = "demidenko(vc)",
                      distribution = list(dist = "bernoulli", prob = 0.3), verbose = 0))
    expect_equal(crrRes[c("test", "odds.ratio", "mean", "sd", "vcf", "null.mean", "null.sd", "z.alpha", "power", "n")],
                 list(test = "z", odds.ratio = 1.416667, mean = 2.8293272, sd = 1.03295386, vcf = 0.85, null.mean = 0, null.sd = 1,
                      z.alpha = c(-1.959964, 1.959964), power = 0.80000377, n = 2114))

    crrRes <- power.z.logistic(base.prob = 0.15, odds.ratio = 1.416667, r.squared.pred = 0.3345431, alpha = 0.05,
                               power = 0.80, distribution = list(dist = "bernoulli", prob = 0.25077), verbose = FALSE)
    expect_equal(class(crrRes), c("pwrss", "z", "logistic"))
    expect_equal(names(crrRes), c("parms", "test", "odds.ratio", "mean", "sd", "vcf", "null.mean", "null.sd", "z.alpha", "power", "n"))
    expect_equal(crrRes[["parms"]],
                 list(base.prob = 0.15, prob = 0.200000038, beta0 = -1.734601055, beta1 = 0.34830693, odds.ratio = 1.416667,
                      r.squared.predictor = 0.3345431, alpha = 0.05, alternative = "two.sided", method = "demidenko(vc)",
                      distribution = list(dist = "bernoulli", prob = 0.25077), verbose = 0))
    expect_equal(crrRes[c("test", "odds.ratio", "mean", "sd", "vcf", "null.mean", "null.sd", "z.alpha", "power", "n")],
                 list(test = "z", odds.ratio = 1.416667, mean = 2.83846121, sd = 1.0435957, vcf = 0.85, null.mean = 0, null.sd = 1,
                      z.alpha = c(-1.959964, 1.959964), power = 0.80005174, n = 3532))

    expect_error(power.z.logistic(base.prob = 0.15, alpha = 0.05, power = 0.80, distribution = "normal", verbose = FALSE),
                 "Specify `base.prob` & `prob` \n  or `base.prob` & `odds.ratio` \n  or `base.prob` & `beta1`\n  or `beta0` & `beta1`.")
    expect_message(power.z.logistic(base.prob = 0.15, prob = 0.20, odds.ratio = 1.5, alpha = 0.05, power = 0.80,
                                    distribution = "normal", verbose = FALSE),
                   "Using `base.prob` and `prob`, ignoring any specifications to `odds.ratio`, `beta0`, or `beta1`.")
    expect_message(power.z.logistic(base.prob = 0.15, odds.ratio = 1.5, beta1 = 0.8, alpha = 0.05, power = 0.80,
                                    distribution = "normal", verbose = FALSE),
                   "Using `base.prob` and `odds.ratio`, ignoring any specifications to `prob`, `beta0`, or `beta1`.")
    expect_message(power.z.logistic(base.prob = 0.15, beta1 = 0.8, beta0 = 0.2, alpha = 0.05, power = 0.80, distribution = "normal", verbose = FALSE),
                   "Using `base.prob` and `beta1`, ignoring any specifications to `prob`, `beta0`, or `odds.ratio`.")
    expect_message(power.z.logistic(prob = 0.2, beta1 = 0.8, beta0 = 0.2, alpha = 0.05, power = 0.80, distribution = "normal", verbose = FALSE),
                   "Using `beta0` and `beta1`, ignoring any specifications to `base.prob`, `prob`, or `odds.ratio`.")
    expect_error(power.z.logistic(base.prob = 0.20, prob = 0.20, alpha = 0.05, power = 0.80, distribution = "normal", verbose = FALSE),
                 "`prob` can not have the same value as `base.prob`.")
    expect_error(power.z.logistic(base.prob = 0.20, prob = 0.40, alpha = 0.05, distribution = "normal", verbose = FALSE),
                 "`n` and `power` cannot be `NULL` at the same time.")
    expect_error(power.z.logistic(base.prob = 0.20, prob = 0.40, alpha = 0.05, power = 0.8, n = 50, distribution = "normal", verbose = FALSE),
                 "Exactly one of the `n` or `power` should be `NULL`.")
    expect_error(power.z.logistic(base.prob = 0.20, prob = 0.40, alpha = 0.05, power = 0.8,
                                  distribution = list(dist = "normal", mean = 0, sd = 1, err = 1), verbose = FALSE),
                 "Unknown input type for `distribution`")
    expect_error(power.z.logistic(base.prob = 0.20, prob = 0.40, alpha = 0.05, power = 0.8,
                                  distribution = list(dist = "normal", mean = 0, sdev = 1), verbose = FALSE),
                 "Unknown input type for `distribution`")
    expect_error(power.z.logistic(base.prob = 0.20, prob = 0.40, alpha = 0.05, power = 0.8, distribution = NA, verbose = FALSE),
                 "Unknown input type for `distribution`")
    expect_error(power.z.logistic(base.prob = 0.20, prob = 0.40, alpha = 0.05, power = 0.8,
                                  distribution = list(dist = "binomial", size = 2, prob = 0.5), method = "hsieh", verbose = FALSE),
                 "Hsieh et al. \\(1998\\) is valid only for a binary covariate or a continuous covariate following normal distribution.")
    expect_error(power.z.logistic(base.prob = 0.20, prob = 0.40, alpha = 0.05, power = 0.8, distribution = "poisson",
                                  method = "hsieh", verbose = FALSE),
                 "Not a valid distribution for the Hsieh et al. \\(1998\\) procedure.")
})
