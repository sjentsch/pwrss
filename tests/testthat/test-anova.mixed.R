test_that("anova.mixed.R works", {
    # power.f.mixed.anova (= pwrss.f.rmanova)
    crrRes <- power.f.mixed.anova(eta.squared = 0.022, factor.levels = c(1, 2), rho.within = 0.50, effect = "within",
                                  power = 0.80, alpha = 0.05, verbose = 0)
    crrOut <- capture.output(power.f.mixed.anova(eta.squared = 0.022,   factor.levels = c(1, 2), rho.within = 0.50,
                                                 effect = "within", power = 0.80, alpha = 0.05))
    crrDtl <- capture.output(power.f.mixed.anova(eta.squared = 0.022,   factor.levels = c(1, 2), rho.within = 0.50,
                                                 effect = "within", power = 0.80, alpha = 0.05, verbose = 2))
    crrPty <- capture.output(power.f.mixed.anova(eta.squared = 0.022,   factor.levels = c(1, 2), rho.within = 0.50,
                                                 effect = "within", power = 0.80, alpha = 0.05, pretty = TRUE))
    crrPnD <- capture.output(power.f.mixed.anova(eta.squared = 0.022,   factor.levels = c(1, 2), rho.within = 0.50,
                                                 effect = "within", power = 0.80, alpha = 0.05, verbose = 2, pretty = TRUE))
    expect_equal(class(crrRes), c("pwrss", "f", "anova_mixed"))
    expect_equal(names(crrRes), c("parms", "test", "effect", "df1", "df2", "ncp", "null.ncp", "f.alpha", "power", "n.total"))
    expect_equal(crrRes[["parms"]],
                 list(eta.squared = 0.022, null.eta.squared = 0, factor.levels = c(1, 2), factor.type = c("between", "within"),
                      rho.within = 0.5, epsilon = 1, alpha = 0.05, effect = "within", ceiling = TRUE, verbose = 0, pretty = FALSE))
    expect_equal(crrRes[c("test", "effect", "df1", "df2", "ncp", "null.ncp", "f.alpha", "power", "n.total")],
                 list(test = "F", effect = "W(2)|B(1)", df1 = 1, df2 = 89, ncp = 8.09815951, null.ncp = 0,
                      f.alpha = 3.94808435, power = 0.803705778, n.total = 90))
    expect_equal(crrOut, c("+--------------------------------------------------+",
                           "|             SAMPLE SIZE CALCULATION              |",
                           "+--------------------------------------------------+", "",
                           "Repeated Measures Analysis of Variance (F-Test)", "",
                           "----------------------------------------------------",
                           "Hypotheses",
                           "----------------------------------------------------",
                           "  H0 (Null Claim) : eta.squared = 0",
                           "  H1 (Alt. Claim) : eta.squared > 0", "",
                           "----------------------------------------------------",
                           "Results",
                           "----------------------------------------------------",
                           "  Total Sample Size    = 90  <<",
                           "  Type 1 Error (alpha) = 0.050",
                           "  Type 2 Error (beta)  = 0.196",
                           "  Statistical Power    = 0.804", ""))
    expect_equal(crrDtl, c("+--------------------------------------------------+",
                           "|             SAMPLE SIZE CALCULATION              |",
                           "+--------------------------------------------------+", "",
                           "Repeated Measures Analysis of Variance (F-Test)", "",
                           "----------------------------------------------------",
                           "Hypotheses",
                           "----------------------------------------------------",
                           "  H0 (Null Claim) : eta.squared = 0",
                           "  H1 (Alt. Claim) : eta.squared > 0", "",
                           "----------------------------------------------------",
                           "Key Parameters",
                           "----------------------------------------------------",
                           "  Design                 = W(2)|B(1) ",
                           "  Num. Deg. of Freedom   = 1 ",
                           "  Denom. Deg. of Freedom = 89 ",
                           "  Non-centrality of Alt. = 8.098 ",
                           "  Non-centrality of Null = 0.000 ",
                           "  Critical Value         = 3.948 ", "",
                           "----------------------------------------------------",
                           "Results",
                           "----------------------------------------------------",
                           "  Total Sample Size    = 90  <<",
                           "  Type 1 Error (alpha) = 0.050",
                           "  Type 2 Error (beta)  = 0.196",
                           "  Statistical Power    = 0.804", "",
                           "----------------------------------------------------",
                           "Definitions",
                           "----------------------------------------------------",
                           "  eta.squared      : (Partial) Eta-squared under alt. ", ""))
    expect_equal(crrPty, c("╔══════════════════════════════════════════════════╗",
                           "║           \033[34m SAMPLE SIZE CALCULATION \033[0m              ║",
                           "╚══════════════════════════════════════════════════╝", "",
                           "Repeated Measures Analysis of Variance (F-Test)", "",
                           "────────────────────────────────────────────────────",
                           "Hypotheses",
                           "────────────────────────────────────────────────────",
                           "  H₀ (Null)             : η² = 0 ",
                           "  H₁ (Alternative)      : η² > 0 ", "",
                           "────────────────────────────────────────────────────",
                           "Results",
                           "────────────────────────────────────────────────────",
                           "  \033[34mTotal Sample Size  = 90\033[0m  \033[1;35m◄◄\033[0m",
                           "  Type 1 Error (α)   = 0.050",
                           "  Type 2 Error (β)   = 0.196",
                           "  Statistical Power  = 0.804", ""))
    expect_equal(crrPnD, c("╔══════════════════════════════════════════════════╗",
                           "║           \033[34m SAMPLE SIZE CALCULATION \033[0m              ║",
                           "╚══════════════════════════════════════════════════╝", "",
                           "Repeated Measures Analysis of Variance (F-Test)", "",
                           "────────────────────────────────────────────────────",
                           "Hypotheses",
                           "────────────────────────────────────────────────────",
                           "  H₀ (Null)             : η² = 0 ",
                           "  H₁ (Alternative)      : η² > 0 ", "",
                           "────────────────────────────────────────────────────",
                           "Key Parameters",
                           "────────────────────────────────────────────────────",
                           "  Design                = W(2)|B(1) ",
                           "  df1                   = 1",
                           "  df2                   = 89",
                           "  λ                     = 8.098",
                           "  λ₀                     = 0.000",
                           "  F⁻¹(α, λ₀)              = 3.948 ", "",
                           "────────────────────────────────────────────────────",
                           "Results",
                           "────────────────────────────────────────────────────",
                           "  \033[34mTotal Sample Size  = 90\033[0m  \033[1;35m◄◄\033[0m",
                           "  Type 1 Error (α)   = 0.050",
                           "  Type 2 Error (β)   = 0.196",
                           "  Statistical Power  = 0.804", "",
                           "\033[36m────────────────────────────────────────────────────\033[0m",
                           "\033[36mDefinitions\033[0m",
                           "\033[36m────────────────────────────────────────────────────\033[0m",
                           "\033[36m  η²   : (Partial) Eta-squared under alternative ",
                           "\033[0m\033[36m  λ    : Non-centrality parameter under alternative ",
                           "\033[0m\033[36m  λ₀  : Non-centrality parameter under null ", "", "\033[0m"))
    expect_equal(crrRes, pwrss.f.rmanova(eta2 = 0.022, n.levels = 1, n.rm = 2, corr.rm = 0.50, type = "within",
                                         power = 0.80, alpha = 0.05, verbose = 0))
    expect_equal(crrRes, pwrss.f.rmanova(f2 = 0.0224948875, n.levels = 1, n.rm = 2, corr.rm = 0.50, type = "within",
                                         power = 0.80, alpha = 0.05, verbose = 0))

    crrRes <- power.f.mixed.anova(eta.squared = 0.022, factor.levels = c(1, 2), rho.within = 0.50, effect = "within",
                                  n.total = 120, alpha = 0.05, verbose = 0)
    crrOut <- capture.output(power.f.mixed.anova(eta.squared = 0.022,   factor.levels = c(1, 2), rho.within = 0.50,
                                                 effect = "within", n.total = 120, alpha = 0.05))
    crrDtl <- capture.output(power.f.mixed.anova(eta.squared = 0.022,   factor.levels = c(1, 2), rho.within = 0.50,
                                                 effect = "within", n.total = 120, alpha = 0.05, verbose = 2))
    crrPty <- capture.output(power.f.mixed.anova(eta.squared = 0.022,   factor.levels = c(1, 2), rho.within = 0.50,
                                                 effect = "within", n.total = 120, alpha = 0.05, pretty = TRUE))
    crrPnD <- capture.output(power.f.mixed.anova(eta.squared = 0.022,   factor.levels = c(1, 2), rho.within = 0.50,
                                                 effect = "within", n.total = 120, alpha = 0.05, verbose = 2, pretty = TRUE))
    expect_equal(class(crrRes), c("pwrss", "f", "anova_mixed"))
    expect_equal(names(crrRes), c("parms", "test", "effect", "df1", "df2", "ncp", "null.ncp", "f.alpha", "power", "n.total"))
    expect_equal(crrRes[["parms"]],
                 list(eta.squared = 0.022, null.eta.squared = 0, factor.levels = c(1, 2), factor.type = c("between", "within"),
                      rho.within = 0.5, epsilon = 1, alpha = 0.05, effect = "within", ceiling = TRUE, verbose = 0, pretty = FALSE))
    expect_equal(crrRes[c("test", "effect", "df1", "df2", "ncp", "null.ncp", "f.alpha", "power", "n.total")],
                 list(test = "F", effect = "W(2)|B(1)", df1 = 1, df2 = 119, ncp = 10.797546, null.ncp = 0,
                      f.alpha = 3.9207955, power = 0.9030908, n.total = 120))
    expect_equal(crrOut, c("+--------------------------------------------------+",
                           "|                POWER CALCULATION                 |",
                           "+--------------------------------------------------+", "",
                           "Repeated Measures Analysis of Variance (F-Test)", "",
                           "----------------------------------------------------",
                           "Hypotheses",
                           "----------------------------------------------------",
                           "  H0 (Null Claim) : eta.squared = 0",
                           "  H1 (Alt. Claim) : eta.squared > 0", "",
                           "----------------------------------------------------",
                           "Results",
                           "----------------------------------------------------",
                           "  Total Sample Size    = 120",
                           "  Type 1 Error (alpha) = 0.050",
                           "  Type 2 Error (beta)  = 0.097",
                           "  Statistical Power    = 0.903  <<", ""))
    expect_equal(crrDtl, c("+--------------------------------------------------+",
                           "|                POWER CALCULATION                 |",
                           "+--------------------------------------------------+", "",
                           "Repeated Measures Analysis of Variance (F-Test)", "",
                           "----------------------------------------------------",
                           "Hypotheses",
                           "----------------------------------------------------",
                           "  H0 (Null Claim) : eta.squared = 0",
                           "  H1 (Alt. Claim) : eta.squared > 0", "",
                           "----------------------------------------------------",
                           "Key Parameters",
                           "----------------------------------------------------",
                           "  Design                 = W(2)|B(1) ",
                           "  Num. Deg. of Freedom   = 1 ",
                           "  Denom. Deg. of Freedom = 119 ",
                           "  Non-centrality of Alt. = 10.798 ",
                           "  Non-centrality of Null = 0.000 ",
                           "  Critical Value         = 3.921 ", "",
                           "----------------------------------------------------",
                           "Results",
                           "----------------------------------------------------",
                           "  Total Sample Size    = 120",
                           "  Type 1 Error (alpha) = 0.050",
                           "  Type 2 Error (beta)  = 0.097",
                           "  Statistical Power    = 0.903  <<", "",
                           "----------------------------------------------------",
                           "Definitions",
                           "----------------------------------------------------",
                           "  eta.squared      : (Partial) Eta-squared under alt. ", ""))
    expect_equal(crrPty, c("╔══════════════════════════════════════════════════╗",
                           "║               \033[34m POWER CALCULATION \033[0m                ║",
                           "╚══════════════════════════════════════════════════╝", "",
                           "Repeated Measures Analysis of Variance (F-Test)", "",
                           "────────────────────────────────────────────────────",
                           "Hypotheses",
                           "────────────────────────────────────────────────────",
                           "  H₀ (Null)             : η² = 0 ",
                           "  H₁ (Alternative)      : η² > 0 ", "",
                           "────────────────────────────────────────────────────",
                           "Results",
                           "────────────────────────────────────────────────────",
                           "  Total Sample Size  = 120",
                           "  Type 1 Error (α)   = 0.050",
                           "  Type 2 Error (β)   = 0.097",
                           "  \033[34mStatistical Power  = 0.903\033[0m  \033[1;35m◄◄\033[0m", ""))
    expect_equal(crrPnD, c("╔══════════════════════════════════════════════════╗",
                           "║               \033[34m POWER CALCULATION \033[0m                ║",
                           "╚══════════════════════════════════════════════════╝", "",
                           "Repeated Measures Analysis of Variance (F-Test)", "",
                           "────────────────────────────────────────────────────",
                           "Hypotheses",
                           "────────────────────────────────────────────────────",
                           "  H₀ (Null)             : η² = 0 ",
                           "  H₁ (Alternative)      : η² > 0 ", "",
                           "────────────────────────────────────────────────────",
                           "Key Parameters",
                           "────────────────────────────────────────────────────",
                           "  Design                = W(2)|B(1) ",
                           "  df1                   = 1",
                           "  df2                   = 119",
                           "  λ                     = 10.798",
                           "  λ₀                     = 0.000",
                           "  F⁻¹(α, λ₀)              = 3.921 ", "",
                           "────────────────────────────────────────────────────",
                           "Results",
                           "────────────────────────────────────────────────────",
                           "  Total Sample Size  = 120",
                           "  Type 1 Error (α)   = 0.050",
                           "  Type 2 Error (β)   = 0.097",
                           "  \033[34mStatistical Power  = 0.903\033[0m  \033[1;35m◄◄\033[0m", "",
                           "\033[36m────────────────────────────────────────────────────\033[0m",
                           "\033[36mDefinitions\033[0m",
                           "\033[36m────────────────────────────────────────────────────\033[0m",
                           "\033[36m  η²   : (Partial) Eta-squared under alternative ",
                           "\033[0m\033[36m  λ    : Non-centrality parameter under alternative ",
                           "\033[0m\033[36m  λ₀  : Non-centrality parameter under null ", "", "\033[0m"))
    expect_equal(crrRes, pwrss.f.rmanova(eta2 = 0.022, n.levels = 1, n.rm = 2, corr.rm = 0.50, type = "within",
                                         n = 120, alpha = 0.05, verbose = 0))
    expect_equal(crrRes, pwrss.f.rmanova(f2 = 0.0224948875, n.levels = 1, n.rm = 2, corr.rm = 0.50, type = "within",
                                         n = 120, alpha = 0.05, verbose = 0))

    crrRes <- suppressWarnings(power.f.mixed.anova(eta.squared = 0.08255, factor.levels = c(1, 2), rho.within = NA,
                                                   effect = "within", power = 0.80, alpha = 0.05, verbose = 0))
    expect_equal(class(crrRes), c("pwrss", "f", "anova_mixed"))
    expect_equal(names(crrRes), c("parms", "test", "effect", "df1", "df2", "ncp", "null.ncp", "f.alpha", "power", "n.total"))
    expect_equal(crrRes[["parms"]],
                 list(eta.squared = 0.08255, null.eta.squared = 0, factor.levels = c(1, 2), factor.type = c("between", "within"),
                      rho.within = NA, epsilon = 1, alpha = 0.05, effect = "within", ceiling = TRUE, verbose = 0, pretty = FALSE))
    expect_equal(crrRes[c("test", "effect", "df1", "df2", "ncp", "null.ncp", "f.alpha", "power", "n.total")],
                 list(test = "F", effect = "W(2)|B(1)", df1 = 1, df2 = 89, ncp = 8.097989, null.ncp = 0,
                      f.alpha = 3.94808435, power = 0.803697575, n.total = 90))

    crrRes <- suppressWarnings(power.f.mixed.anova(eta.squared = 0.08255, factor.levels = c(1, 2), rho.within = NA,
                                                   effect = "within", n.total = 90, alpha = 0.05, verbose = 0))
    expect_equal(class(crrRes), c("pwrss", "f", "anova_mixed"))
    expect_equal(names(crrRes), c("parms", "test", "effect", "df1", "df2", "ncp", "null.ncp", "f.alpha", "power", "n.total"))
    expect_equal(crrRes[["parms"]],
                 list(eta.squared = 0.08255, null.eta.squared = 0, factor.levels = c(1, 2), factor.type = c("between", "within"),
                      rho.within = NA, epsilon = 1, alpha = 0.05, effect = "within", ceiling = TRUE, verbose = 0, pretty = FALSE))
    expect_equal(crrRes[c("test", "effect", "df1", "df2", "ncp", "null.ncp", "f.alpha", "power", "n.total")],
                 list(test = "F", effect = "W(2)|B(1)", df1 = 1, df2 = 89, ncp = 8.097989, null.ncp = 0,
                      f.alpha = 3.94808435, power = 0.803697575, n.total = 90))

    crrRes <- power.f.mixed.anova(eta.squared = 0.059, factor.levels = c(2, 1), effect = "between", alpha = 0.05,
                                  power = 0.80, verbose = 0)
    crrOut <- capture.output(power.f.mixed.anova(eta.squared = 0.059, factor.levels = c(2, 1), effect = "between",
                                                 alpha = 0.05, power = 0.80))
    expect_equal(class(crrRes), c("pwrss", "f", "anova_mixed"))
    expect_equal(names(crrRes), c("parms", "test", "effect", "df1", "df2", "ncp", "null.ncp", "f.alpha", "power", "n.total"))
    expect_equal(crrRes[["parms"]],
                 list(eta.squared = 0.059, null.eta.squared = 0, factor.levels = c(2, 1), factor.type = c("between", "within"),
                      rho.within = 0.5, epsilon = 1, alpha = 0.05, effect = "between", ceiling = TRUE, verbose = 0, pretty = FALSE))
    expect_equal(crrRes[c("test", "effect", "df1", "df2", "ncp", "null.ncp", "f.alpha", "power", "n.total")],
                 list(test = "F", effect = "B(2)|W(1)", df1 = 1, df2 = 126, ncp = 8.0255048, null.ncp = 0,
                      f.alpha = 3.9163246, power = 0.8027032, n.total = 128))
    expect_equal(crrOut[5], "Analysis of Variance (F-Test)")

    crrRes <- power.f.mixed.anova(eta.squared = 0.059, factor.levels = c(2, 1), effect = "between", alpha = 0.05,
                                  n.total = 128, verbose = 0)
    expect_equal(class(crrRes), c("pwrss", "f", "anova_mixed"))
    expect_equal(names(crrRes), c("parms", "test", "effect", "df1", "df2", "ncp", "null.ncp", "f.alpha", "power", "n.total"))
    expect_equal(crrRes[["parms"]],
                 list(eta.squared = 0.059, null.eta.squared = 0, factor.levels = c(2, 1), factor.type = c("between", "within"),
                      rho.within = 0.5, epsilon = 1, alpha = 0.05, effect = "between", ceiling = TRUE, verbose = 0, pretty = FALSE))
    expect_equal(crrRes[c("test", "effect", "df1", "df2", "ncp", "null.ncp", "f.alpha", "power", "n.total")],
                 list(test = "F", effect = "B(2)|W(1)", df1 = 1, df2 = 126, ncp = 8.0255048, null.ncp = 0,
                      f.alpha = 3.9163246, power = 0.8027032, n.total = 128))

    crrRes <- power.f.mixed.anova(eta.squared = 0.038, factor.levels = c(2, 2),  rho.within = 0.50, effect = "between",
                                  power = 0.80, alpha = 0.05, verbose = 0)
    crrOut <- capture.output(power.f.mixed.anova(eta.squared = 0.038, factor.levels = c(2, 2),  rho.within = 0.50,
                                                 effect = "between", power = 0.80, alpha = 0.05))
    expect_equal(class(crrRes), c("pwrss", "f", "anova_mixed"))
    expect_equal(names(crrRes), c("parms", "test", "effect", "df1", "df2", "ncp", "null.ncp", "f.alpha", "power", "n.total"))
    expect_equal(crrRes[["parms"]],
                 list(eta.squared = 0.038, null.eta.squared = 0, factor.levels = c(2, 2), factor.type = c("between", "within"),
                      rho.within = 0.5, epsilon = 1, alpha = 0.05, effect = "between", ceiling = TRUE, verbose = 0, pretty = FALSE))
    expect_equal(crrRes[c("test", "effect", "df1", "df2", "ncp", "null.ncp", "f.alpha", "power", "n.total")],
                 list(test = "F", effect = "B(2)|W(2)", df1 = 1, df2 = 150, ncp = 8.005544, null.ncp = 0,
                      f.alpha = 3.9042019, power = 0.802695047, n.total = 152))
    expect_equal(crrOut[5], "Mixed-Effects Analysis of Variance (F-Test)")

    crrRes <- power.f.mixed.anova(eta.squared = 0.038, factor.levels = c(2, 2),  rho.within = 0.50, effect = "between",
                                  n.total = 152, alpha = 0.05, verbose = 0)
    expect_equal(class(crrRes), c("pwrss", "f", "anova_mixed"))
    expect_equal(names(crrRes), c("parms", "test", "effect", "df1", "df2", "ncp", "null.ncp", "f.alpha", "power", "n.total"))
    expect_equal(crrRes[["parms"]],
                 list(eta.squared = 0.038, null.eta.squared = 0, factor.levels = c(2, 2), factor.type = c("between", "within"),
                      rho.within = 0.5, epsilon = 1, alpha = 0.05, effect = "between", ceiling = TRUE, verbose = 0, pretty = FALSE))
    expect_equal(crrRes[c("test", "effect", "df1", "df2", "ncp", "null.ncp", "f.alpha", "power", "n.total")],
                 list(test = "F", effect = "B(2)|W(2)", df1 = 1, df2 = 150, ncp = 8.005544, null.ncp = 0,
                      f.alpha = 3.9042019, power = 0.802695047, n.total = 152))

    crrRes <- power.f.mixed.anova(eta.squared = 0.01, factor.levels = c(2, 2), rho.within = 0.50, effect = "interaction",
                                  power = 0.80, alpha = 0.05, verbose = 0)
    expect_equal(class(crrRes), c("pwrss", "f", "anova_mixed"))
    expect_equal(names(crrRes), c("parms", "test", "effect", "df1", "df2", "ncp", "null.ncp", "f.alpha", "power", "n.total"))
    expect_equal(crrRes[["parms"]],
                 list(eta.squared = 0.01, null.eta.squared = 0, factor.levels = c(2, 2), factor.type = c("between", "within"),
                      rho.within = 0.5, epsilon = 1, alpha = 0.05, effect = "interaction", ceiling = TRUE, verbose = 0, pretty = FALSE))
    expect_equal(crrRes[c("test", "effect", "df1", "df2", "ncp", "null.ncp", "f.alpha", "power", "n.total")],
                 list(test = "F", effect = "B(2):W(2)", df1 = 1, df2 = 196, ncp = 8.00, null.ncp = 0,
                      f.alpha = 3.889341, power = 0.8036086, n.total = 198))

    crrRes <- power.f.mixed.anova(eta.squared = 0.01, factor.levels = c(2, 2), rho.within = 0.50, effect = "interaction",
                                  n.total = 198, alpha = 0.05, verbose = 0)
    expect_equal(class(crrRes), c("pwrss", "f", "anova_mixed"))
    expect_equal(names(crrRes), c("parms", "test", "effect", "df1", "df2", "ncp", "null.ncp", "f.alpha", "power", "n.total"))
    expect_equal(crrRes[["parms"]],
                 list(eta.squared = 0.01, null.eta.squared = 0, factor.levels = c(2, 2), factor.type = c("between", "within"),
                      rho.within = 0.5, epsilon = 1, alpha = 0.05, effect = "interaction", ceiling = TRUE, verbose = 0, pretty = FALSE))
    expect_equal(crrRes[c("test", "effect", "df1", "df2", "ncp", "null.ncp", "f.alpha", "power", "n.total")],
                 list(test = "F", effect = "B(2):W(2)", df1 = 1, df2 = 196, ncp = 8.00, null.ncp = 0,
                      f.alpha = 3.889341, power = 0.8036086, n.total = 198))

    crrRes <- power.f.mixed.anova(eta.squared = 0.01, factor.levels = c(2, 2), rho.within = 0.50, effect = "within",
                                  power = 0.80, alpha = 0.05, verbose = 0)
    expect_equal(class(crrRes), c("pwrss", "f", "anova_mixed"))
    expect_equal(names(crrRes), c("parms", "test", "effect", "df1", "df2", "ncp", "null.ncp", "f.alpha", "power", "n.total"))
    expect_equal(crrRes[["parms"]],
                 list(eta.squared = 0.01, null.eta.squared = 0, factor.levels = c(2, 2), factor.type = c("between", "within"),
                      rho.within = 0.5, epsilon = 1, alpha = 0.05, effect = "within", ceiling = TRUE, verbose = 0, pretty = FALSE))
    expect_equal(crrRes[c("test", "effect", "df1", "df2", "ncp", "null.ncp", "f.alpha", "power", "n.total")],
                 list(test = "F", effect = "W(2)|B(2)", df1 = 1, df2 = 196, ncp = 8.00, null.ncp = 0,
                      f.alpha = 3.889341, power = 0.8036086, n.total = 198))

    expect_error(power.f.mixed.anova(eta.squared = 0.022, factor.levels = c(2, 2), factor.type = c("within", "within"),
                                     rho.within = 0.50, effect = "within", power = 0.80, alpha = 0.05, verbose = 0),
                 paste("The `factor.type` argument must be specified as either c\\('between', 'within'\\) or",
                       "c\\('within', 'between'\\), indicating the order in which the corresponding values in",
                       "`factor.levels` are interpreted - specifically, which factor is treated as between-subjects",
                       "and which as within-subjects."))
    expect_error(power.f.mixed.anova(eta.squared = 0.022, factor.levels = c(2, 2), factor.type = c("within", "wrong"),
                                     rho.within = 0.50, effect = "within", power = 0.80, alpha = 0.05, verbose = 0),
                 paste("The `factor.type` argument must be specified as either c\\('between', 'within'\\) or",
                       "c\\('within', 'between'\\), indicating the order in which the corresponding values in",
                       "`factor.levels` are interpreted - specifically, which factor is treated as between-subjects",
                       "and which as within-subjects."))
    expect_error(power.f.mixed.anova(eta.squared = 0.022, factor.levels = c(2, 2, 2), factor.type = c("within", "between"),
                                     rho.within = 0.50, effect = "within", power = 0.80, alpha = 0.05, verbose = 0),
                 "Excatly two factors are allowed in this procedure.")
    expect_error(power.f.mixed.anova(eta.squared = 0.022, factor.levels = c(2, 2), rho.within = 0.50, effect = "within",
                                     epsilon = 0.1, power = 0.80, alpha = 0.05, verbose = 0),
                 "Incorrect value for the non-sphericity correction factor \\(`epsilon`\\).")
    expect_error(power.f.mixed.anova(eta.squared = 1e-12, factor.levels = c(4, 4), rho.within = 0.50, effect = "within",
                                     power = 0.99999, alpha = 0.05, verbose = 0),
                 "Design is not feasible.")
    expect_error(power.f.mixed.anova(eta.squared = 0.022, factor.levels = c(1, 1), rho.within = 0.50, effect = "within",
                                     power = 0.80, alpha = 0.05, verbose = 0),
                 "Design is not feasible.")
    expect_error(pwrss.f.rmanova(eta2 = 0.022, f2 = 0.0224948875, n.levels = 1, n.rm = 2, type = "within", n = 120, verbose = 0),
                 "Effect size conflict for the alternative. Specify only either `eta2` or `f2`.")
})
