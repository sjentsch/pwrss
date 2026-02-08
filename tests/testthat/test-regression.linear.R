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
    crrOut <- capture.output(power.f.regression(r.squared = 0.15, k.total = 3, power = 0.80))
    crrDtl <- capture.output(power.f.regression(r.squared = 0.15, k.total = 3, power = 0.80, verbose = 2))
    crrPty <- capture.output(power.f.regression(r.squared = 0.15, k.total = 3, power = 0.80, pretty = TRUE))
    crrPnD <- capture.output(power.f.regression(r.squared = 0.15, k.total = 3, power = 0.80, verbose = 2, pretty = TRUE))
    expect_equal(class(crrRes), c("pwrss", "f", "regression"))
    expect_equal(names(crrRes), c("parms", "test", "df1", "df2", "ncp", "null.ncp", "f.alpha", "power", "n"))
    expect_equal(crrRes[["parms"]],
                 list(r.squared.change = 0.15, margin = 0, k.total = 3, k.tested = 3, alpha = 0.05, ceiling = TRUE, verbose = 0, pretty = FALSE))
    expect_equal(crrRes[c("test", "df1", "df2", "ncp", "null.ncp", "f.alpha", "power", "n")],
                 list(test = "F", df1 = 3, df2 = 62, ncp = 11.6470588, null.ncp = 0, f.alpha = 2.75296975, power = 0.801253619, n = 66))
    expect_equal(crrOut, c("+--------------------------------------------------+",
                           "|             SAMPLE SIZE CALCULATION              |",
                           "+--------------------------------------------------+", "",
                           "Linear Regression (F-Test)", "",
                           "----------------------------------------------------",
                           "Hypotheses",
                           "----------------------------------------------------",
                           "  H0 (Null Claim) : R-squared = 0",
                           "  H1 (Alt. Claim) : R-squared > 0", "",
                           "----------------------------------------------------",
                           "Results",
                           "----------------------------------------------------",
                           "  Sample Size          = 66  <<",
                           "  Type 1 Error (alpha) = 0.050",
                           "  Type 2 Error (beta)  = 0.199",
                           "  Statistical Power    = 0.801", ""))
    expect_equal(crrDtl, c("+--------------------------------------------------+",
                           "|             SAMPLE SIZE CALCULATION              |",
                           "+--------------------------------------------------+", "",
                           "Linear Regression (F-Test)", "",
                           "----------------------------------------------------",
                           "Hypotheses",
                           "----------------------------------------------------",
                           "  H0 (Null Claim) : R-squared = 0",
                           "  H1 (Alt. Claim) : R-squared > 0", "",
                           "----------------------------------------------------",
                           "Key Parameters",
                           "----------------------------------------------------",
                           "  R-squared              = 0.150 ",
                           "  Margin                 = 0.000 ",
                           "  Num. Deg. of Freedom   = 3",
                           "  Denom. Deg. of Freedom = 62",
                           "  Non-centrality of Alt. = 11.647 ",
                           "  Non-centrality of Null = 0.000 ",
                           "  Critical Value         = 2.753 ", "",
                           "----------------------------------------------------",
                           "Results",
                           "----------------------------------------------------",
                           "  Sample Size          = 66  <<",
                           "  Type 1 Error (alpha) = 0.050",
                           "  Type 2 Error (beta)  = 0.199",
                           "  Statistical Power    = 0.801", "",
                           "----------------------------------------------------",
                           "Definitions",
                           "----------------------------------------------------",
                           "  Margin : Smallest R-squared that matters ", ""))
    expect_equal(crrPty, c("╔══════════════════════════════════════════════════╗",
                           "║           \033[34m SAMPLE SIZE CALCULATION \033[0m              ║",
                           "╚══════════════════════════════════════════════════╝", "",
                           "Linear Regression (F-Test)", "",
                           "────────────────────────────────────────────────────",
                           "Hypotheses",
                           "────────────────────────────────────────────────────",
                           "  H₀ (Null)        : R² = 0 ",
                           "  H₁ (Alternative) : R² > 0 ", "",
                           "────────────────────────────────────────────────────",
                           "Results",
                           "────────────────────────────────────────────────────",
                           "  \033[34mSample Size        = 66\033[0m  \033[1;35m◄◄\033[0m",
                           "  Type 1 Error (α)   = 0.050",
                           "  Type 2 Error (β)   = 0.199",
                           "  Statistical Power  = 0.801", ""))
    expect_equal(crrPnD, c("╔══════════════════════════════════════════════════╗",
                           "║           \033[34m SAMPLE SIZE CALCULATION \033[0m              ║",
                           "╚══════════════════════════════════════════════════╝", "",
                           "Linear Regression (F-Test)", "",
                           "────────────────────────────────────────────────────",
                           "Hypotheses",
                           "────────────────────────────────────────────────────",
                           "  H₀ (Null)        : R² = 0 ",
                           "  H₁ (Alternative) : R² > 0 ", "",
                           "────────────────────────────────────────────────────",
                           "Key Parameters",
                           "────────────────────────────────────────────────────",
                           "  R²                = 0.150",
                           "  δ                 = 0.000",
                           "  df1               = 3",
                           "  df2               = 62",
                           "  λ                 = 11.647",
                           "  λ₀                 = 0.000",
                           "  F⁻¹(α, λ₀)          = 2.753 ", "",
                           "────────────────────────────────────────────────────",
                           "Results",
                           "────────────────────────────────────────────────────",
                           "  \033[34mSample Size        = 66\033[0m  \033[1;35m◄◄\033[0m",
                           "  Type 1 Error (α)   = 0.050",
                           "  Type 2 Error (β)   = 0.199",
                           "  Statistical Power  = 0.801", "",
                           "\033[36m────────────────────────────────────────────────────\033[0m",
                           "\033[36mDefinitions\033[0m",
                           "\033[36m────────────────────────────────────────────────────\033[0m",
                           "\033[36m  δ   : Margin - ignorable R² or ΔR² ",
                           "\033[0m\033[36m  λ   : Non-centrality parameter under alternative ",
                           "\033[0m\033[36m  λ₀ : Non-centrality parameter under null ", "", "\033[0m"))
    expect_equal(crrOut, capture.output(power.f.regression(r.squared = 0.15, k.total = 3, power = 0.80, verbose = NULL)))
    expect_equal(crrRes, pwrss.f.regression(r2 = 0.15, k = 3, power = 0.80, verbose = 0))
    expect_equal(crrRes, pwrss.f.regression(f2 = 0.17647059, k = 3, power = 0.80, verbose = 0))
    expect_equal(crrRes, pwrss.f.reg(r2 = 0.15, k = 3, power = 0.80, verbose = 0))

    crrRes <- power.f.regression(r.squared = 0.15, k.total = 3, power = 0.80, ceiling = FALSE, verbose = 0)
    expect_equal(class(crrRes), c("pwrss", "f", "regression"))
    expect_equal(names(crrRes), c("parms", "test", "df1", "df2", "ncp", "null.ncp", "f.alpha", "power", "n"))
    expect_equal(crrRes[["parms"]],
                 list(r.squared.change = 0.15, margin = 0, k.total = 3, k.tested = 3, alpha = 0.05, ceiling = FALSE, verbose = 0, pretty = FALSE))
    expect_equal(crrRes[c("test", "df1", "df2", "ncp", "null.ncp", "f.alpha", "power", "n")],
                 list(test = "F", df1 = 3, df2 = 61.8227, ncp = 11.61577059, null.ncp = 0, f.alpha = 2.7534088, power = 0.8, n = 65.8227))

    crrRes <- power.f.regression(r.squared.change = 0.10, k.total = 5,  k.tested = 2, power = 0.80, verbose = 0)
    expect_equal(class(crrRes), c("pwrss", "f", "regression"))
    expect_equal(names(crrRes), c("parms", "test", "df1", "df2", "ncp", "null.ncp", "f.alpha", "power", "n"))
    expect_equal(crrRes$parms, list(r.squared.change = 0.10, margin = 0, k.total = 5, k.tested = 2, alpha = 0.05,
                                    ceiling = TRUE, verbose = 0, pretty = FALSE))
    expect_equal(crrRes[c("test", "df1", "df2", "ncp", "null.ncp", "f.alpha", "power", "n")],
                 list(test = "F", df1 = 2, df2 = 84, ncp = 10, null.ncp = 0, f.alpha = 3.10515661, power = 0.8005579, n = 90))

    crrRes <- power.f.regression(r.squared.change = 0.10, k.total = 5,  k.tested = 2, n = 90, verbose = 0)
    expect_equal(class(crrRes), c("pwrss", "f", "regression"))
    expect_equal(names(crrRes), c("parms", "test", "df1", "df2", "ncp", "null.ncp", "f.alpha", "power", "n"))
    expect_equal(crrRes$parms, list(r.squared.change = 0.10, margin = 0, k.total = 5, k.tested = 2, alpha = 0.05,
                                    ceiling = TRUE, verbose = 0, pretty = FALSE))
    expect_equal(crrRes[c("test", "df1", "df2", "ncp", "null.ncp", "f.alpha", "power", "n")],
                 list(test = "F", df1 = 2, df2 = 84, ncp = 10, null.ncp = 0, f.alpha = 3.10515661, power = 0.8005579, n = 90))

    expect_error(power.f.regression(r.squared.change = 0.10, k.total = 5, k.tested = 2, verbose = 0),
                 "`n` and `power` cannot be NULL at the same time.")
    expect_error(power.f.regression(r.squared.change = 0.10, k.total = 5, k.tested = 2, power = 0.8, n = 90, verbose = 0),
                 "Exactly / only one of the parameters `n` or `power` should be NULL.")
    expect_error(power.f.regression(r.squared.change = 0.10, k.total = 2, k.tested = 5, power = 0.8, verbose = 0),
                 "`k.tested` cannot be greater than `k.total`.")
    expect_error(power.f.regression(r.squared.change = 0, k.total = 5, k.tested = 2, power = 0.8, verbose = 0),
                 "Value for `r.squared.change` must be a finite number that is larger than 0 and smaller than 1.")
    expect_error(power.f.regression(r.squared.change = 0.99, k.total = 5, k.tested = 2, power = 0.8, verbose = 0),
                 "Design is not feasible.")
    expect_error(pwrss.f.regression(r2 = 0.15, f2 = 0.17647059, k = 3, power = 0.80, verbose = 0),
                 "Effect size conflict for the alternative. Specify only either `r2` or `f2`.")

    # power.t.regression (= pwrss.t.regression / pwrss.t.reg) ----------------------------------------------------------
    crrRes <- power.t.regression(beta = 0.20, k.total = 5, r.squared = 0.30, power = 0.80, verbose = 0)
    crrOut <- capture.output(power.t.regression(beta = 0.20, k.total = 5, r.squared = 0.30, power = 0.80))
    crrDtl <- capture.output(power.t.regression(beta = 0.20, k.total = 5, r.squared = 0.30, power = 0.80, verbose = 2))
    crrPty <- capture.output(power.t.regression(beta = 0.20, k.total = 5, r.squared = 0.30, power = 0.80, pretty = TRUE))
    crrPnD <- capture.output(power.t.regression(beta = 0.20, k.total = 5, r.squared = 0.30, power = 0.80, verbose = 2, pretty = TRUE))
    expect_equal(class(crrRes), c("pwrss", "t", "regression"))
    expect_equal(names(crrRes),
                 c("parms", "test", "std.beta", "std.null.beta", "std.margin", "df", "t.alpha", "ncp", "null.ncp", "power", "n"))
    expect_equal(crrRes[["parms"]],
                 list(beta = 0.2, null.beta = 0, margin = 0, sd.predictor = 1, sd.outcome = 1, r.squared = 0.3,
                      k.total = 5, alpha = 0.05, alternative = "two.sided", ceiling = TRUE, verbose = 0, pretty = FALSE))
    expect_equal(crrRes[c("test", "std.beta", "std.null.beta", "std.margin", "df", "t.alpha", "ncp", "null.ncp", "power", "n")],
                 list(test = "t", std.beta = 0.2, std.null.beta = 0, std.margin = 0, df = 134, t.alpha = c(-1.97782576, 1.97782576),
                      ncp = 2.82842712, null.ncp = 0, power = 0.801820080, n = 140))
    expect_equal(crrOut, c("+--------------------------------------------------+",
                           "|             SAMPLE SIZE CALCULATION              |",
                           "+--------------------------------------------------+", "",
                           "Linear Regression Coefficient (T-Test)", "",
                           "----------------------------------------------------",
                           "Hypotheses",
                           "----------------------------------------------------",
                           "  H0 (Null Claim) : beta - null.beta  = 0",
                           "  H1 (Alt. Claim) : beta - null.beta != 0", "",
                           "----------------------------------------------------",
                           "Results",
                           "----------------------------------------------------",
                           "  Sample Size          = 140  <<",
                           "  Type 1 Error (alpha) = 0.050",
                           "  Type 2 Error (beta)  = 0.198",
                           "  Statistical Power    = 0.802", ""))
    expect_equal(crrDtl, c("+--------------------------------------------------+",
                           "|             SAMPLE SIZE CALCULATION              |",
                           "+--------------------------------------------------+", "",
                           "Linear Regression Coefficient (T-Test)", "",
                           "----------------------------------------------------",
                           "Hypotheses",
                           "----------------------------------------------------",
                           "  H0 (Null Claim) : beta - null.beta  = 0",
                           "  H1 (Alt. Claim) : beta - null.beta != 0", "",
                           "----------------------------------------------------",
                           "Key Parameters",
                           "----------------------------------------------------",
                           "  Std. Beta Under Alt.   = 0.200 ",
                           "  Std. Beta Under Null   = 0.000 ",
                           "  Std. Margin            = 0 ",
                           "  Degrees of Freedom     = 134 ",
                           "  Non-centrality of Alt. = 2.828",
                           "  Non-centrality of Null = 0 ",
                           "  Critical Value         = -1.978 and 1.978 ", "",
                           "----------------------------------------------------",
                           "Results",
                           "----------------------------------------------------",
                           "  Sample Size          = 140  <<",
                           "  Type 1 Error (alpha) = 0.050",
                           "  Type 2 Error (beta)  = 0.198",
                           "  Statistical Power    = 0.802", "",
                           "----------------------------------------------------",
                           "Definitions",
                           "----------------------------------------------------",
                           "  beta                 : Regression coefficient under alt. ",
                           "  null.beta            : Regression coefficient under null ",
                           "  margin               : Smallest beta - null.beta difference that matters ", "",
                           "  Std. Beta Under Alt. = beta * [SD(X) / SD(Y)]",
                           "  Std. Beta Under Null = null.beta * [SD(X) / SD(Y)]",
                           "  Std. Margin          = margin * [SD(X) / SD(Y)] ", "",
                           "  SD(X)                : Standard deviation of the predictor ",
                           "  SD(Y)                : Standard deviation of the outcome ", ""))
    expect_equal(crrPty, c("╔══════════════════════════════════════════════════╗",
                           "║           \033[34m SAMPLE SIZE CALCULATION \033[0m              ║",
                           "╚══════════════════════════════════════════════════╝", "",
                           "Linear Regression Coefficient (T-Test)", "",
                           "────────────────────────────────────────────────────",
                           "Hypotheses",
                           "────────────────────────────────────────────────────",
                           "  H₀ (Null)        : β - β₀ = 0 ",
                           "  H₁ (Alternative) : β - β₀ ≠ 0 ", "",
                           "────────────────────────────────────────────────────",
                           "Results",
                           "────────────────────────────────────────────────────",
                           "  \033[34mSample Size        = 140\033[0m  \033[1;35m◄◄\033[0m",
                           "  Type 1 Error (α)   = 0.050",
                           "  Type 2 Error (β)   = 0.198",
                           "  Statistical Power  = 0.802", ""))
    expect_equal(crrPnD, c("╔══════════════════════════════════════════════════╗",
                           "║           \033[34m SAMPLE SIZE CALCULATION \033[0m              ║",
                           "╚══════════════════════════════════════════════════╝", "",
                           "Linear Regression Coefficient (T-Test)", "",
                           "────────────────────────────────────────────────────",
                           "Hypotheses",
                           "────────────────────────────────────────────────────",
                           "  H₀ (Null)        : β - β₀ = 0 ",
                           "  H₁ (Alternative) : β - β₀ ≠ 0 ", "",
                           "────────────────────────────────────────────────────",
                           "Key Parameters",
                           "────────────────────────────────────────────────────",
                           "  Std. β            = 0.200 ",
                           "  Std. β₀            = 0.000 ",
                           "  Std. δ            = 0 ",
                           "  df                = 134 ",
                           "  λ (Alternative)   = 2.828",
                           "  λ₀ (Null)          = 0 ",
                           "  T⁻¹(α, λ₀)          = -1.978 and 1.978 ", "",
                           "────────────────────────────────────────────────────",
                           "Results",
                           "────────────────────────────────────────────────────",
                           "  \033[34mSample Size        = 140\033[0m  \033[1;35m◄◄\033[0m",
                           "  Type 1 Error (α)   = 0.050",
                           "  Type 2 Error (β)   = 0.198",
                           "  Statistical Power  = 0.802", "",
                           "\033[36m────────────────────────────────────────────────────\033[0m",
                           "\033[36mDefinitions\033[0m",
                           "\033[36m────────────────────────────────────────────────────\033[0m",
                           "\033[36m  β      : Regression coefficient under alternative ",
                           "\033[0m\033[36m  β₀      : Regression coefficient under null ",
                           "\033[0m\033[36m  δ      : Margin(s) - ignorable β - β₀ difference ", "",
                           "\033[0m\033[36m  Std. β = β * [σ(X) / σ(Y)] ",
                           "\033[0m\033[36m  Std. β₀ = β₀ * [σ(X) / σ(Y)] ",
                           "\033[0m\033[36m  Std. δ = δ * [σ(X) / σ(Y)] ", "",
                           "\033[0m\033[36m  σ(X)   : Standard devition of the predictor ",
                           "\033[0m\033[36m  σ(Y)   : Standard devition of the outcome ", "",
                           "\033[0m\033[36m  λ      : Non-centrality parameter under alternative ",
                           "\033[0m\033[36m  λ₀      : Non-centrality parameter under null ", "", "\033[0m"))
    expect_equal(crrOut, capture.output(power.t.regression(beta = 0.20, k.total = 5, r.squared = 0.30, power = 0.80, verbose = NULL)))
    expect_equal(crrRes, pwrss.t.regression(beta1 = 0.20, k = 5, r2 = 0.30, power = 0.80, verbose = 0))
    expect_equal(crrRes, pwrss.t.reg(beta1 = 0.20, k = 5, r2 = 0.30, power = 0.80, verbose = 0))

    crrRes <- power.t.regression(beta = 0.20, k.total = 5, r.squared = 0.30, power = 0.80, ceiling = FALSE, verbose = 0)
    expect_equal(class(crrRes), c("pwrss", "t", "regression"))
    expect_equal(names(crrRes),
                 c("parms", "test", "std.beta", "std.null.beta", "std.margin", "df", "t.alpha", "ncp", "null.ncp", "power", "n"))
    expect_equal(crrRes[["parms"]],
                 list(beta = 0.2, null.beta = 0, margin = 0, sd.predictor = 1, sd.outcome = 1, r.squared = 0.3,
                      k.total = 5, alpha = 0.05, alternative = "two.sided", ceiling = FALSE, verbose = 0, pretty = FALSE))
    expect_equal(crrRes[c("test", "std.beta", "std.null.beta", "std.margin", "df", "t.alpha", "ncp", "null.ncp", "power", "n")],
                 list(test = "t", std.beta = 0.2, std.null.beta = 0, std.margin = 0, df = 133.36039, t.alpha = c(-1.97791219, 1.97791219),
                      ncp = 2.82195869, null.ncp = 0, power = 0.8, n = 139.36039))

    crrRes <- power.t.regression(beta = 0.20, margin = -0.05, alternative = "one.sided", sd.predictor = 0.5, k.total = 5,
                                 r.squared = 0.30, power = 0.80, verbose = 0)
    expect_equal(class(crrRes), c("pwrss", "t", "regression"))
    expect_equal(names(crrRes),
                 c("parms", "test", "std.beta", "std.null.beta", "std.margin", "df", "t.alpha", "ncp", "null.ncp", "power", "n"))
    expect_equal(crrRes[["parms"]],
                 list(beta = 0.2, null.beta = 0, margin = -0.05, sd.predictor = 0.5, sd.outcome = 1, r.squared = 0.3,
                      k.total = 5, alpha = 0.05, alternative = "one.sided", ceiling = TRUE, verbose = 0, pretty = FALSE))
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
                      k.total = 5, alpha = 0.05, alternative = "one.sided", ceiling = TRUE, verbose = 0, pretty = FALSE))
    expect_equal(crrRes[c("test", "std.beta", "std.null.beta", "std.margin", "df", "t.alpha", "ncp", "null.ncp", "power", "n")],
                 list(test = "t", std.beta = 0.1, std.null.beta = 0, std.margin = 0.025, df = 767, t.alpha = 2.4797221,
                      ncp = 3.32307947, null.ncp = 0.83076987, power = 0.8002402, n = 773))
    expect_equal(crrRes, pwrss.t.regression(beta1 = 0.20, margin = 0.05, alternative = "superior", sdx = 0.5, k = 5,
                                            r2 = 0.30, power = 0.80, verbose = 0))

    crrRes <- power.t.regression(beta = 0, margin = c(-0.05, 0.05), alternative = "two.one.sided", sd.predictor = 0.5,
                                 k.total = 5, r.squared = 0.30, power = 0.80, verbose = 0)
    crrOut <- capture.output(power.t.regression(beta = 0, margin = c(-0.05, 0.05), alternative = "two.one.sided",
                                                sd.predictor = 0.5, k.total = 5, r.squared = 0.30, power = 0.80))
    expect_equal(class(crrRes), c("pwrss", "t", "regression"))
    expect_equal(names(crrRes),
                 c("parms", "test", "std.beta", "std.null.beta", "std.margin", "df", "t.alpha", "ncp", "null.ncp", "power", "n"))
    expect_equal(crrRes[["parms"]],
                 list(beta = 0, null.beta = 0, margin = c(-0.05, 0.05), sd.predictor = 0.5, sd.outcome = 1, r.squared = 0.3,
                      k.total = 5, alpha = 0.05, alternative = "two.one.sided", ceiling = TRUE, verbose = 0, pretty = FALSE))
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
                      r.squared = 0.3, k.total = 5, alpha = 0.05, alternative = "two.one.sided", ceiling = TRUE, verbose = 0,
                      pretty = FALSE))
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
                      r.squared = 0.3, k.total = 5, alpha = 0.05, alternative = "two.one.sided", ceiling = TRUE, verbose = 0,
                      pretty = FALSE))
    expect_equal(crrRes[c("test", "std.beta", "std.null.beta", "std.margin", "df", "t.alpha", "ncp", "null.ncp", "power", "n")],
                 list(test = "t", std.beta = 0, std.null.beta = 0, std.margin = c(-0.023570226, 0.023570226), df = 10786,
                      t.alpha = c(-1.28172913, 1.28172913), ncp = 0, null.ncp = c(-2.926615680, 2.926615680),
                      power = 0.800034768, n = 10792))

    expect_error(power.t.regression(beta = 0.20, k.total = 5, r.squared = 0.30, verbose = 0),
                 "`n` and `power` cannot be NULL at the same time.")
    expect_error(power.t.regression(beta = 0.20, k.total = 5, r.squared = 0.30, power = 0.80, n = 140, verbose = 0),
                 "Exactly / only one of the parameters `n` or `power` should be NULL.")
    expect_error(power.t.regression(beta = 0.20, k.total = 5, r.squared = -0.01, power = 0.80, verbose = 0),
                 "Incorrect value for `r.squared`, specify `r.squared` explicitly or modify `beta`, `sd.predictor`, `sd.outcome`.")
    expect_error(power.t.regression(beta = 0, margin = 0.05, alternative = "two.one.sided", sd.predictor = 0.5, k.total = 5,
                                    r.squared = 0.30, power = 0.8, verbose = 0),
                 "Specify `margin` in the form of margin = c\\(lower, upper\\).")
    expect_error(power.t.regression(beta = 0, margin = c(-0.05, 0.05), alternative = "one.sided", sd.predictor = 0.5, k.total = 5,
                                    r.squared = 0.30, power = 0.8, verbose = 0),
                 "Specify only one value for the `margin`.")
    expect_error(power.t.regression(beta = 0.20, k.total = 5, r.squared = 0.999, power = 0.80, verbose = 0),
                 "Design is not feasible.")
    expect_warning(power.t.regression(beta = 0.20, k.total = 5, r.squared = 0.01, power = 0.80, verbose = 0),
                 "`r.squared` is possibly larger.")

    # pwrss.z.regression (not longer supported) ------------------------------------------------------------------------
    expect_error(pwrss.z.regression(), "This function is no longer available. Please use `power.t.regression\\(\\)`.")
})
