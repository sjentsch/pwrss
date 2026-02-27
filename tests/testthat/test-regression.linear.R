test_that("regression.linear.R works", {
    # rsq.to.f ---------------------------------------------------------------------------------------------------------
    expect_equal(rsq.to.f(0.009900990),
                 list(f.squared = 0.0100, f = 0.10, r.squared.full = 0.009900990, r.squared.reduced = 0))
    expect_equal(rsq.to.f(0.058823530),
                 list(f.squared = 0.0625, f = 0.25, r.squared.full = 0.058823530, r.squared.reduced = 0))
    expect_equal(rsq.to.f(0.137931034),
                 list(f.squared = 0.1600, f = 0.40, r.squared.full = 0.137931034, r.squared.reduced = 0))
    expect_error(rsq.to.f(0.1, 0.2), "Expecting `r.squared.full` > `r.squared.reduced`.")
    expect_equal(capture.output(rsq.to.f(0.137931034, verbose = 1)),
                 c("        f.squared                 f    r.squared.full r.squared.reduced ",
                   "         0.160000          0.400000          0.137931          0.000000 "))

    # f.to.rsq ---------------------------------------------------------------------------------------------------------
    expect_equal(f.to.rsq(0.10),
                 list(f.squared = 0.0100, f = 0.10, r.squared.full = 0.009900990, r.squared.reduced = 0))
    expect_equal(f.to.rsq(0.25),
                 list(f.squared = 0.0625, f = 0.25, r.squared.full = 0.058823530, r.squared.reduced = 0))
    expect_equal(f.to.rsq(0.40),
                 list(f.squared = 0.1600, f = 0.40, r.squared.full = 0.137931034, r.squared.reduced = 0))
    expect_equal(f.to.rsq(0.40, r.squared.full = 0.00862069, verbose = 0),
                 list(f.squared = 0.1600, f = 0.40, r.squared.full = 0.00862069, r.squared.reduced = -0.15))
    expect_equal(capture.output(f.to.rsq(0.40, verbose = 1)),
                 c("        f.squared                 f    r.squared.full r.squared.reduced ",
                   "         0.160000          0.400000          0.137931          0.000000 "))

    # power.f.regression (= pwrss.f.regression) ------------------------------------------------------------------------
    crrRes <- power.f.regression(r.squared = 0.15, k.total = 3, power = 0.80, verbose = 0)
    expect_equal(class(crrRes), c("pwrss", "f", "regression"))
    expect_equal(names(crrRes), c("parms", "test", "df1", "df2", "ncp", "null.ncp", "f.alpha", "power", "n"))
    expect_equal(crrRes[["parms"]],
                 list(r.squared.change = 0.15, margin = 0, k.total = 3, k.tested = 3, n = NULL, power = 0.80, alpha = 0.05,
                      ceiling = TRUE, verbose = 0, utf = FALSE))
    expect_equal(crrRes[c("test", "df1", "df2", "ncp", "null.ncp", "f.alpha", "power", "n")],
                 list(test = "F", df1 = 3, df2 = 62, ncp = 11.6470588, null.ncp = 0, f.alpha = 2.75296975, power = 0.801253619, n = 66))
    expect_equal(crrRes, pwrss.f.regression(r2 = 0.15, k = 3, power = 0.80, verbose = 0))
    expect_equal(crrRes, pwrss.f.regression(f2 = 0.17647059, k = 3, power = 0.80, verbose = 0))
    expect_equal(crrRes, pwrss.f.reg(r2 = 0.15, k = 3, power = 0.80, verbose = 0))

    crrRes <- power.f.regression(r.squared = 0.15, k.total = 3, power = 0.80, ceiling = FALSE, verbose = 0)
    expect_equal(class(crrRes), c("pwrss", "f", "regression"))
    expect_equal(names(crrRes), c("parms", "test", "df1", "df2", "ncp", "null.ncp", "f.alpha", "power", "n"))
    expect_equal(crrRes[["parms"]],
                 list(r.squared.change = 0.15, margin = 0, k.total = 3, k.tested = 3, n = NULL, power = 0.80, alpha = 0.05,
                      ceiling = FALSE, verbose = 0, utf = FALSE))
    expect_equal(crrRes[c("test", "df1", "df2", "ncp", "null.ncp", "f.alpha", "power", "n")],
                 list(test = "F", df1 = 3, df2 = 61.8227, ncp = 11.61577059, null.ncp = 0, f.alpha = 2.7534088,
                      power = 0.80, n = 65.8227))

    crrRes <- power.f.regression(r.squared.change = 0.10, k.total = 5,  k.tested = 2, power = 0.80, verbose = 0)
    expect_equal(class(crrRes), c("pwrss", "f", "regression"))
    expect_equal(names(crrRes), c("parms", "test", "df1", "df2", "ncp", "null.ncp", "f.alpha", "power", "n"))
    expect_equal(crrRes[["parms"]],
                 list(r.squared.change = 0.10, margin = 0, k.total = 5, k.tested = 2, n = NULL, power = 0.80, alpha = 0.05,
                      ceiling = TRUE, verbose = 0, utf = FALSE))
    expect_equal(crrRes[c("test", "df1", "df2", "ncp", "null.ncp", "f.alpha", "power", "n")],
                 list(test = "F", df1 = 2, df2 = 84, ncp = 10, null.ncp = 0, f.alpha = 3.10515661, power = 0.8005579, n = 90))

    crrRes <- power.f.regression(r.squared.change = 0.10, k.total = 5,  k.tested = 2, n = 90, verbose = 0)
    expect_equal(class(crrRes), c("pwrss", "f", "regression"))
    expect_equal(names(crrRes), c("parms", "test", "df1", "df2", "ncp", "null.ncp", "f.alpha", "power", "n"))
    expect_equal(crrRes[["parms"]],
                 list(r.squared.change = 0.10, margin = 0, k.total = 5, k.tested = 2, n = 90, power = NULL, alpha = 0.05,
                      ceiling = TRUE, verbose = 0, utf = FALSE))
    expect_equal(crrRes[c("test", "df1", "df2", "ncp", "null.ncp", "f.alpha", "power", "n")],
                 list(test = "F", df1 = 2, df2 = 84, ncp = 10, null.ncp = 0, f.alpha = 3.10515661, power = 0.8005579, n = 90))

    crrRes <- power.f.regression(r.squared.change = f.to.rsq(sqrt(1 / 9))$r.squared.full, k.total = 5,  n = 95,
                                 verbose = 0) # example 13.3.1 from GPower manual
    expect_equal(class(crrRes), c("pwrss", "f", "regression"))
    expect_equal(names(crrRes), c("parms", "test", "df1", "df2", "ncp", "null.ncp", "f.alpha", "power", "n"))
    expect_equal(crrRes[["parms"]],
                 list(r.squared.change = 0.10, margin = 0, k.total = 5, k.tested = 5, n = 95, power = NULL, alpha = 0.05,
                      ceiling = TRUE, verbose = 0, utf = FALSE))
    expect_equal(crrRes[c("test", "df1", "df2", "ncp", "null.ncp", "f.alpha", "power", "n")],
                 list(test = "F", df1 = 5, df2 = 89, ncp = 10.55555556, null.ncp = 0, f.alpha = 2.31685752,
                      power = 0.67358577, n = 95))
    # results are identical: ncp ~ 10.555555, f.crit ~ 2.316858, power ~ 0.673586, n = 95

    crrRes <- power.f.regression(r.squared.change = f.to.rsq(0.59309044)$r.squared.full, k.total = 3, n = 22,
                                 verbose = 0) # example 13.3.2 from GPower manual
    expect_equal(class(crrRes), c("pwrss", "f", "regression"))
    expect_equal(names(crrRes), c("parms", "test", "df1", "df2", "ncp", "null.ncp", "f.alpha", "power", "n"))
    expect_equal(crrRes[["parms"]],
                 list(r.squared.change = 0.260221667, margin = 0, k.total = 3, k.tested = 3, n = 22, power = NULL,
                      alpha = 0.05, ceiling = TRUE, verbose = 0, utf = FALSE))
    expect_equal(crrRes[c("test", "df1", "df2", "ncp", "null.ncp", "f.alpha", "power", "n")],
                 list(test = "F", df1 = 3, df2 = 18, ncp = 7.738638, null.ncp = 0, f.alpha = 3.15990759,
                      power = 0.53601059, n = 22))
    # results are identical: power ~ 0.536011, n = 22

    crrRes <- power.f.regression(r.squared.change = 0.25, k.total = 1,  n = 12, verbose = 0) # example 13.3.3 from GPower manual
    expect_equal(class(crrRes), c("pwrss", "f", "regression"))
    expect_equal(names(crrRes), c("parms", "test", "df1", "df2", "ncp", "null.ncp", "f.alpha", "power", "n"))
    expect_equal(crrRes[["parms"]],
                 list(r.squared.change = 0.25, margin = 0, k.total = 1, k.tested = 1, n = 12, power = NULL, alpha = 0.05,
                      ceiling = TRUE, verbose = 0, utf = FALSE))
    expect_equal(crrRes[c("test", "df1", "df2", "ncp", "null.ncp", "f.alpha", "power", "n")],
                 list(test = "F", df1 = 1, df2 = 10, ncp = 4, null.ncp = 0, f.alpha = 4.9646027, power = 0.4396273, n = 12))
    # results are identical: power ~ 0.439627, n = 12

    crrRes <- power.f.regression(r.squared.change = 0.06666660, k.total = 9, k.tested = 4, n = 90, alpha = 0.01,
                                 verbose = 0) # example 14.3.1 from GPower manual
    expect_equal(class(crrRes), c("pwrss", "f", "regression"))
    expect_equal(names(crrRes), c("parms", "test", "df1", "df2", "ncp", "null.ncp", "f.alpha", "power", "n"))
    expect_equal(crrRes[["parms"]],
                 list(r.squared.change = 0.06666660, margin = 0, k.total = 9, k.tested = 4, n = 90, power = NULL,
                      alpha = 0.01, ceiling = TRUE, verbose = 0, utf = FALSE))
    expect_equal(crrRes[c("test", "df1", "df2", "ncp", "null.ncp", "f.alpha", "power", "n")],
                 list(test = "F", df1 = 4, df2 = 80, ncp = 6.4285645, null.ncp = 0, f.alpha = 3.56310963, power = 0.241296126, n = 90))
    # results are identical: ncp ~ 6.42857, f.crit ~ 3.563110, power ~ 0.241297, n = 90

    crrRes <- power.f.regression(r.squared.change = 0.06666660, k.total = 9, k.tested = 4, power = 0.80, alpha = 0.01,
                                 verbose = 0) # example 14.3.1 from GPower manual (further down in the text)
    expect_equal(class(crrRes), c("pwrss", "f", "regression"))
    expect_equal(names(crrRes), c("parms", "test", "df1", "df2", "ncp", "null.ncp", "f.alpha", "power", "n"))
    expect_equal(crrRes[["parms"]],
                 list(r.squared.change = 0.06666660, margin = 0, k.total = 9, k.tested = 4, n = NULL, power = 0.80,
                      alpha = 0.01, ceiling = TRUE, verbose = 0, utf = FALSE))
    expect_equal(crrRes[c("test", "df1", "df2", "ncp", "null.ncp", "f.alpha", "power", "n")],
                 list(test = "F", df1 = 4, df2 = 232, ncp = 17.2856958, null.ncp = 0, f.alpha = 3.4010085,
                      power = 0.80157159, n = 242))
    # results are identical: power ~ 0.80, n = 242

    crrRes <- power.f.regression(r.squared.change = 0.06976744, k.total = 12, k.tested = 3, n = 200, alpha = 0.01,
                                 verbose = 0) # example 14.3.2 from GPower manual
    expect_equal(class(crrRes), c("pwrss", "f", "regression"))
    expect_equal(names(crrRes), c("parms", "test", "df1", "df2", "ncp", "null.ncp", "f.alpha", "power", "n"))
    expect_equal(crrRes[["parms"]],
                 list(r.squared.change = 0.06976744, margin = 0, k.total = 12, k.tested = 3, n = 200, power = NULL,
                      alpha = 0.01, ceiling = TRUE, verbose = 0, utf = FALSE))
    expect_equal(crrRes[c("test", "df1", "df2", "ncp", "null.ncp", "f.alpha", "power", "n")],
                 list(test = "F", df1 = 3, df2 = 187, ncp = 14.99999957, null.ncp = 0, f.alpha = 3.88805227,
                      power = 0.76698955, n = 200))
    # results are identical: ncp ~ 15, f.crit ~ 3.888052, power ~ 0.766990, n = 200

    crrRes <- power.f.regression(r.squared.change = f.to.rsq(0.25)$r.squared.full, k.total = 23, k.tested = 6, n = 120,
                                 alpha = 0.05, verbose = 0) # example 14.3.3 from GPower manual
    expect_equal(class(crrRes), c("pwrss", "f", "regression"))
    expect_equal(names(crrRes), c("parms", "test", "df1", "df2", "ncp", "null.ncp", "f.alpha", "power", "n"))
    expect_equal(crrRes[["parms"]],
                 list(r.squared.change = 0.05882353, margin = 0, k.total = 23, k.tested = 6, n = 120, power = NULL,
                      alpha = 0.05, ceiling = TRUE, verbose = 0, utf = FALSE))
    expect_equal(crrRes[c("test", "df1", "df2", "ncp", "null.ncp", "f.alpha", "power", "n")],
                 list(test = "F", df1 = 6, df2 = 96, ncp = 7.5, null.ncp = 0, f.alpha = 2.19451621, power = 0.47012906, n = 120))
    # results are identical: power ~ 0.470, n = 120

    expect_error(power.f.regression(r.squared.change = 0.10, k.total = 5, k.tested = 2, verbose = 0),
                 "`n` and `power` cannot be NULL at the same time.")
    expect_error(power.f.regression(r.squared.change = 0.10, k.total = 5, k.tested = 2, power = 0.80, n = 90, verbose = 0),
                 "Exactly / only one of the parameters `n` or `power` should be NULL.")
    expect_error(power.f.regression(r.squared.change = 0.10, k.total = 2, k.tested = 5, power = 0.80, verbose = 0),
                 "`k.tested` cannot be greater than `k.total`.")
    expect_error(power.f.regression(r.squared.change = 0, k.total = 5, k.tested = 2, power = 0.80, verbose = 0),
                 "Value for `r.squared.change` must be a finite number that is larger than 0 and smaller than 1.")
    expect_error(power.f.regression(r.squared.change = 0.99, k.total = 5, k.tested = 2, power = 0.80, verbose = 0),
                 "Design is not feasible.")
    expect_error(pwrss.f.regression(r2 = 0.15, f2 = 0.17647059, k = 3, power = 0.80, verbose = 0),
                 "Effect size conflict for the alternative. Specify only either `r2` or `f2`.")

    # power.t.regression (= pwrss.t.regression / pwrss.t.reg) ----------------------------------------------------------
    crrRes <- power.t.regression(beta = 0.20, k.total = 5, r.squared = 0.30, power = 0.80, verbose = 0)
    expect_equal(class(crrRes), c("pwrss", "t", "regression"))
    expect_equal(names(crrRes),
                 c("parms", "test", "std.beta", "std.null.beta", "std.margin", "df", "t.alpha", "ncp", "null.ncp", "power", "n"))
    expect_equal(crrRes[["parms"]],
                 list(beta = 0.2, null.beta = 0, margin = 0, sd.predictor = 1, sd.outcome = 1, r.squared = 0.3,
                      k.total = 5, n = NULL, power = 0.80, alpha = 0.05, alternative = "two.sided", ceiling = TRUE,
                      verbose = 0, utf = FALSE))
    expect_equal(crrRes[c("test", "std.beta", "std.null.beta", "std.margin", "df", "t.alpha", "ncp", "null.ncp", "power", "n")],
                 list(test = "t", std.beta = 0.2, std.null.beta = 0, std.margin = 0, df = 134, t.alpha = c(-1.97782576, 1.97782576),
                      ncp = 2.82842712, null.ncp = 0, power = 0.801820080, n = 140))
    expect_equal(crrRes, pwrss.t.regression(beta1 = 0.20, k = 5, r2 = 0.30, power = 0.80, verbose = 0))
    expect_equal(crrRes, pwrss.t.reg(beta1 = 0.20, k = 5, r2 = 0.30, power = 0.80, verbose = 0))

    crrRes <- power.t.regression(beta = 0.20, k.total = 5, r.squared = 0.30, power = 0.80, ceiling = FALSE, verbose = 0)
    expect_equal(class(crrRes), c("pwrss", "t", "regression"))
    expect_equal(names(crrRes),
                 c("parms", "test", "std.beta", "std.null.beta", "std.margin", "df", "t.alpha", "ncp", "null.ncp", "power", "n"))
    expect_equal(crrRes[["parms"]],
                 list(beta = 0.2, null.beta = 0, margin = 0, sd.predictor = 1, sd.outcome = 1, r.squared = 0.3,
                      k.total = 5, n = NULL, power = 0.80, alpha = 0.05, alternative = "two.sided", ceiling = FALSE,
                      verbose = 0, utf = FALSE))
    expect_equal(crrRes[c("test", "std.beta", "std.null.beta", "std.margin", "df", "t.alpha", "ncp", "null.ncp", "power", "n")],
                 list(test = "t", std.beta = 0.2, std.null.beta = 0, std.margin = 0, df = 133.36039, t.alpha = c(-1.97791219, 1.97791219),
                      ncp = 2.82195869, null.ncp = 0, power = 0.80, n = 139.36039))

    crrRes <- power.t.regression(beta = 0.20, margin = -0.05, alternative = "one.sided", sd.predictor = 0.5, k.total = 5,
                                 r.squared = 0.30, power = 0.80, verbose = 0)
    expect_equal(class(crrRes), c("pwrss", "t", "regression"))
    expect_equal(names(crrRes),
                 c("parms", "test", "std.beta", "std.null.beta", "std.margin", "df", "t.alpha", "ncp", "null.ncp", "power", "n"))
    expect_equal(crrRes[["parms"]],
                 list(beta = 0.2, null.beta = 0, margin = -0.05, sd.predictor = 0.5, sd.outcome = 1, r.squared = 0.3,
                      k.total = 5, n = NULL, power = 0.80, alpha = 0.05, alternative = "one.sided", ceiling = TRUE,
                      verbose = 0, utf = FALSE))
    expect_equal(crrRes[c("test", "std.beta", "std.null.beta", "std.margin", "df", "t.alpha", "ncp", "null.ncp", "power", "n")],
                 list(test = "t", std.beta = 0.1, std.null.beta = 0, std.margin = -0.025, df = 272, t.alpha = 1.1496925537477,
                      ncp = 1.99284434, null.ncp = -0.498211085, power = 0.80043763, n = 278))

    crrRes <- power.t.regression(beta = 0.20, margin = 0.05, alternative = "one.sided", sd.predictor = 0.5, k.total = 5,
                                 r.squared = 0.30, power = 0.80, verbose = 0)
    expect_equal(class(crrRes), c("pwrss", "t", "regression"))
    expect_equal(names(crrRes),
                 c("parms", "test", "std.beta", "std.null.beta", "std.margin", "df", "t.alpha", "ncp", "null.ncp", "power", "n"))
    expect_equal(crrRes[["parms"]],
                 list(beta = 0.2, null.beta = 0, margin = 0.05, sd.predictor = 0.5, sd.outcome = 1, r.squared = 0.3,
                      k.total = 5, n = NULL, power = 0.80, alpha = 0.05, alternative = "one.sided", ceiling = TRUE,
                      verbose = 0, utf = FALSE))
    expect_equal(crrRes[c("test", "std.beta", "std.null.beta", "std.margin", "df", "t.alpha", "ncp", "null.ncp", "power", "n")],
                 list(test = "t", std.beta = 0.1, std.null.beta = 0, std.margin = 0.025, df = 767, t.alpha = 2.4797221,
                      ncp = 3.32307947, null.ncp = 0.83076987, power = 0.8002402, n = 773))
    expect_equal(crrRes, pwrss.t.regression(beta1 = 0.20, margin = 0.05, alternative = "superior", sdx = 0.5, k = 5,
                                            r2 = 0.30, power = 0.80, verbose = 0))

    crrRes <- power.t.regression(beta = 0, margin = c(-0.05, 0.05), alternative = "two.one.sided", sd.predictor = 0.5,
                                 k.total = 5, r.squared = 0.30, power = 0.80, verbose = 0)
    expect_equal(class(crrRes), c("pwrss", "t", "regression"))
    expect_equal(names(crrRes),
                 c("parms", "test", "std.beta", "std.null.beta", "std.margin", "df", "t.alpha", "ncp", "null.ncp", "power", "n"))
    expect_equal(crrRes[["parms"]],
                 list(beta = 0, null.beta = 0, margin = c(-0.05, 0.05), sd.predictor = 0.5, sd.outcome = 1, r.squared = 0.3,
                      k.total = 5, n = NULL, power = 0.80, alpha = 0.05, alternative = "two.one.sided", ceiling = TRUE,
                      verbose = 0, utf = FALSE))
    expect_equal(crrRes[c("test", "std.beta", "std.null.beta", "std.margin", "df", "t.alpha", "ncp", "null.ncp", "power", "n")],
                 list(test = "t", std.beta = 0, std.null.beta = 0, std.margin = c(-0.025, 0.025), df = 9587,
                      t.alpha = c(-1.28174196476245, 1.28174196476601), ncp = 0, null.ncp = c(-2.92663263349341, 2.92663263349341),
                      power = 0.800035824, n = 9593))
    expect_equal(crrRes, pwrss.t.regression(beta1 = 0, margin = 0.05, alternative = "equivalent", sdx = 0.5, k = 5,
                                            r2 = 0.30, power = 0.80, verbose = 0))

    crrRes <- power.t.regression(beta = 0, margin = c(-0.05, 0.05), alternative = "two.one.sided", sd.predictor = sqrt(2 / 3 * 1 / 3),
                                 k.total = 5, r.squared = 0.30, power = 0.80, verbose = 0)
    expect_equal(class(crrRes), c("pwrss", "t", "regression"))
    expect_equal(names(crrRes),
                 c("parms", "test", "std.beta", "std.null.beta", "std.margin", "df", "t.alpha", "ncp", "null.ncp", "power", "n"))
    expect_equal(crrRes[["parms"]],
                 list(beta = 0, null.beta = 0, margin = c(-0.05, 0.05), sd.predictor = sqrt(2 / 3 * 1 / 3), sd.outcome = 1,
                      r.squared = 0.3, k.total = 5, n = NULL, power = 0.80, alpha = 0.05, alternative = "two.one.sided",
                      ceiling = TRUE, verbose = 0, utf = FALSE))
    expect_equal(crrRes[c("test", "std.beta", "std.null.beta", "std.margin", "df", "t.alpha", "ncp", "null.ncp", "power", "n")],
                 list(test = "t", std.beta = 0, std.null.beta = 0, std.margin = c(-0.023570226, 0.023570226), df = 10786,
                      t.alpha = c(-1.28172913, 1.28172913), ncp = 0, null.ncp = c(-2.926615680, 2.926615680),
                      power = 0.800034768, n = 10792))

    crrRes <- power.t.regression(beta = 0, margin = c(-0.05, 0.05), alternative = "two.one.sided", sd.predictor = sqrt(2 / 3 * 1 / 3),
                                 k.total = 5, r.squared = 0.30, n = 10792, verbose = 0)
    expect_equal(class(crrRes), c("pwrss", "t", "regression"))
    expect_equal(names(crrRes),
                 c("parms", "test", "std.beta", "std.null.beta", "std.margin", "df", "t.alpha", "ncp", "null.ncp", "power", "n"))
    expect_equal(crrRes[["parms"]],
                 list(beta = 0, null.beta = 0, margin = c(-0.05, 0.05), sd.predictor = sqrt(2 / 3 * 1 / 3), sd.outcome = 1,
                      r.squared = 0.3, k.total = 5, n = 10792, power = NULL, alpha = 0.05, alternative = "two.one.sided",
                      ceiling = TRUE, verbose = 0, utf = FALSE))
    expect_equal(crrRes[c("test", "std.beta", "std.null.beta", "std.margin", "df", "t.alpha", "ncp", "null.ncp", "power", "n")],
                 list(test = "t", std.beta = 0, std.null.beta = 0, std.margin = c(-0.023570226, 0.023570226), df = 10786,
                      t.alpha = c(-1.28172913, 1.28172913), ncp = 0, null.ncp = c(-2.926615680, 2.926615680),
                      power = 0.800034768, n = 10792))
    expect_equal(crrRes[-1],
                 power.t.regression(beta = 0, margin = 0.05, alternative = "two.one.sided", sd.predictor = sqrt(2 / 3 * 1 / 3),
                                    k.total = 5, r.squared = 0.30, n = 10792, verbose = 0)[-1])

    crrRes <- power.t.regression(beta = -0.0667, sd.predictor = 7.5, sd.outcome = 4, n = 100, verbose = 0) # example 12.3 from GPower
    expect_equal(class(crrRes), c("pwrss", "t", "regression"))
    expect_equal(names(crrRes),
                 c("parms", "test", "std.beta", "std.null.beta", "std.margin", "df", "t.alpha", "ncp", "null.ncp", "power", "n"))
    expect_equal(crrRes[["parms"]],
                 list(beta = -0.0667, null.beta = 0, margin = 0, sd.predictor = 7.5, sd.outcome = 4, r.squared = 0.015640629,
                      k.total = 1, n = 100, power = NULL, alpha = 0.05, alternative = "two.sided", ceiling = TRUE,
                      verbose = 0, utf = FALSE))
    expect_equal(crrRes[c("test", "std.beta", "std.null.beta", "std.margin", "df", "t.alpha", "ncp", "null.ncp", "power", "n")],
                 list(test = "t", std.beta = -0.1250625, std.null.beta = 0, std.margin = 0, df = 98,
                      t.alpha = 1.98446745 * c(-1, 1), ncp = -1.26052152, null.ncp = 0, power = 0.238969257, n = 100))
    # results are identical: ncp ~ -1.260522, t crit ~ 1.984467, power ~ 0.2389693, n = 100

    crrRes <- power.t.regression(beta = 0.25, power = 0.95, alternative = "one.sided", verbose = 0) # example 16.3 from GPower
    expect_equal(class(crrRes), c("pwrss", "t", "regression"))
    expect_equal(names(crrRes),
                 c("parms", "test", "std.beta", "std.null.beta", "std.margin", "df", "t.alpha", "ncp", "null.ncp", "power", "n"))
    expect_equal(crrRes[["parms"]],
                 list(beta = 0.25, null.beta = 0, margin = 0, sd.predictor = 1, sd.outcome = 1, r.squared = 0.0625,
                      k.total = 1, n = NULL, power = 0.95, alpha = 0.05, alternative = "one.sided", ceiling = TRUE,
                      verbose = 0, utf = FALSE))
    expect_equal(crrRes[c("test", "std.beta", "std.null.beta", "std.margin", "df", "t.alpha", "ncp", "null.ncp", "power", "n")],
                 list(test = "t", std.beta = 0.25, std.null.beta = 0, std.margin = 0, df = 162, t.alpha = 1.654313957,
                      ncp = 3.30655914, null.ncp = 0, power = 0.95030825, n = 164))
    # results are identical: ncp ~ 3.306559, t crit ~ 1.654314, power ~ 0.950308, n = 164

    expect_error(power.t.regression(beta = 0.20, k.total = 5, r.squared = 0.30, verbose = 0),
                 "`n` and `power` cannot be NULL at the same time.")
    expect_error(power.t.regression(beta = 0.20, k.total = 5, r.squared = 0.30, power = 0.80, n = 140, verbose = 0),
                 "Exactly / only one of the parameters `n` or `power` should be NULL.")
    expect_error(power.t.regression(beta = 0.20, k.total = 5, r.squared = -0.01, power = 0.80, verbose = 0),
                 "Incorrect value for `r.squared`, specify `r.squared` explicitly or modify `beta`, `sd.predictor`, `sd.outcome`.")
    expect_error(power.t.regression(beta = 0, margin = c(-0.05, 0.05), alternative = "one.sided", sd.predictor = 0.5, k.total = 5,
                                    r.squared = 0.30, power = 0.80, verbose = 0),
                 "If `alternative` is \"two.sided\" or \"one.sided\", `margin` must be of length one.")
    expect_error(power.t.regression(beta = 0.20, k.total = 5, r.squared = 0.999, power = 0.80, verbose = 0),
                 "Design is not feasible.")
    expect_warning(power.t.regression(beta = 0.20, k.total = 5, r.squared = 0.01, power = 0.80, verbose = 0),
                 "`r.squared` is possibly larger.")

    # pwrss.z.regression (not longer supported) ------------------------------------------------------------------------
    expect_error(pwrss.z.regression(), "This function is no longer available. Please use `power.t.regression\\(\\)`.")
})
