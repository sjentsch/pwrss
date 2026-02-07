test_that("ancova.R works", {
    # power.f.ancova (= pwrss.f.ancova) --------------------------------------------------------------------------------
    crrRes <- power.f.ancova(eta.squared = 0.059, factor.levels = 2, alpha = 0.05, power = .80, verbose = 0)
    crrOut <- capture.output(power.f.ancova(eta.squared = 0.059, factor.levels = 2, alpha = 0.05, power = .80))
    crrDtl <- capture.output(power.f.ancova(eta.squared = 0.059, factor.levels = 2, alpha = 0.05, power = .80, verbose = 2))
    crrPty <- capture.output(power.f.ancova(eta.squared = 0.059, factor.levels = 2, alpha = 0.05, power = .80, pretty = TRUE))
    crrPnD <- capture.output(power.f.ancova(eta.squared = 0.059, factor.levels = 2, alpha = 0.05, power = .80, verbose = 2, pretty = TRUE))
    expect_equal(class(crrRes), c("pwrss", "f", "ancova"))
    expect_equal(names(crrRes),
                 c("parms", "test", "effect", "df1", "df2", "ncp", "null.ncp", "f.alpha", "power", "n.total"))
    expect_equal(crrRes[["parms"]],
                 list(eta.squared = 0.059, null.eta.squared = 0, factor.levels = 2, k.covariates = 0, alpha = 0.05,
                      ceiling = TRUE, verbose = 0, pretty = FALSE))
    expect_equal(crrRes[c("test", "effect", "df1", "df2", "ncp", "null.ncp", "f.alpha", "power", "n.total")],
                 list(test = "F", effect = "A(2)", df1 = 1, df2 = 126, ncp = 8.02550478, null.ncp = 0, f.alpha = 3.91632464,
                      power = 0.8027032, n.total = 128))
    expect_equal(crrOut, c("+--------------------------------------------------+",
                           "|             SAMPLE SIZE CALCULATION              |",
                           "+--------------------------------------------------+", "",
                           "One-way Analysis of Variance (F-Test)", "",
                           "----------------------------------------------------",
                           "Hypotheses",
                           "----------------------------------------------------",
                           "  H0 (Null Claim) : eta.squared = 0 ",
                           "  H1 (Alt. Claim) : eta.squared > 0 ", "",
                           "----------------------------------------------------",
                           "Results",
                           "----------------------------------------------------",
                           "  Total Sample Size    = 128  <<",
                           "  Type 1 Error (alpha) = 0.050",
                           "  Type 2 Error (beta)  = 0.197",
                           "  Statistical Power    = 0.803", ""))
    expect_equal(crrDtl, c("+--------------------------------------------------+",
                           "|             SAMPLE SIZE CALCULATION              |",
                           "+--------------------------------------------------+", "",
                           "One-way Analysis of Variance (F-Test)", "",
                           "----------------------------------------------------",
                           "Hypotheses",
                           "----------------------------------------------------",
                           "  H0 (Null Claim) : eta.squared = 0 ",
                           "  H1 (Alt. Claim) : eta.squared > 0 ", "",
                           "----------------------------------------------------",
                           "Key Parameters",
                           "----------------------------------------------------",
                           "  Design                 = A(2) ",
                           "  Num. Deg. of Freedom   = 1 ",
                           "  Denom. Deg. of Freedom = 126 ",
                           "  Non-centrality of Alt. = 8.026 ",
                           "  Non-centrality of Null = 0.000 ",
                           "  Critical Value         = 3.916 ", "",
                           "----------------------------------------------------",
                           "Results",
                           "----------------------------------------------------",
                           "  Total Sample Size    = 128  <<",
                           "  Type 1 Error (alpha) = 0.050",
                           "  Type 2 Error (beta)  = 0.197",
                           "  Statistical Power    = 0.803", "",
                           "----------------------------------------------------",
                           "Definitions",
                           "----------------------------------------------------",
                           "  eta.squared      : (Partial) Eta-squared under alt. ", ""))
    expect_equal(crrPty, c("╔══════════════════════════════════════════════════╗",
                           "║           \033[34m SAMPLE SIZE CALCULATION \033[0m              ║",
                           "╚══════════════════════════════════════════════════╝", "",
                           "One-way Analysis of Variance (F-Test)", "",
                           "────────────────────────────────────────────────────",
                           "Hypotheses",
                           "────────────────────────────────────────────────────",
                           "  H₀ (Null)             : η² = 0 ",
                           "  H₁ (Alternative)      : η² > 0 ", "",
                           "────────────────────────────────────────────────────",
                           "Results",
                           "────────────────────────────────────────────────────",
                           "  \033[34mTotal Sample Size = 128\033[0m  \033[1;35m◄◄\033[0m",
                           "  Type 1 Error (α)  = 0.050",
                           "  Type 2 Error (β)  = 0.197",
                           "  Statistical Power = 0.803", ""))
    expect_equal(crrPnD, c("╔══════════════════════════════════════════════════╗",
                           "║           \033[34m SAMPLE SIZE CALCULATION \033[0m              ║",
                           "╚══════════════════════════════════════════════════╝", "",
                           "One-way Analysis of Variance (F-Test)", "",
                           "────────────────────────────────────────────────────",
                           "Hypotheses",
                           "────────────────────────────────────────────────────",
                           "  H₀ (Null)             : η² = 0 ",
                           "  H₁ (Alternative)      : η² > 0 ", "",
                           "────────────────────────────────────────────────────",
                           "Key Parameters",
                           "────────────────────────────────────────────────────",
                           "  Design                = A(2) ",
                           "  df1                   = 1",
                           "  df2                   = 126",
                           "  λ                     = 8.026",
                           "  λ₀                     = 0.000",
                           "  F⁻¹(α, λ₀)              = 3.916 ", "",
                           "────────────────────────────────────────────────────",
                           "Results",
                           "────────────────────────────────────────────────────",
                           "  \033[34mTotal Sample Size = 128\033[0m  \033[1;35m◄◄\033[0m",
                           "  Type 1 Error (α)  = 0.050",
                           "  Type 2 Error (β)  = 0.197",
                           "  Statistical Power = 0.803", "",
                           "\033[36m────────────────────────────────────────────────────\033[0m",
                           "\033[36mDefinitions\033[0m",
                           "\033[36m────────────────────────────────────────────────────\033[0m",
                           "\033[36m  η²   : (Partial) Eta-squared under alternative ",
                           "\033[0m\033[36m  λ    : Non-centrality parameter under alternative ",
                           "\033[0m\033[36m  λ₀  : Non-centrality parameter under null ", "", "\033[0m"))
    expect_equal(crrRes, pwrss.f.ancova(eta2 = 0.059, n.levels = 2, alpha = 0.05, power = .80, verbose = 0))
    expect_equal(crrRes, pwrss.f.ancova(f2 = 0.059 / (1 - 0.059), n.levels = 2, alpha = 0.05, power = .80, verbose = 0))

    crrRes <- power.f.ancova(eta.squared = 0.059, factor.levels = 4, alpha = 0.05, power = .80, verbose = 0)
    expect_equal(class(crrRes), c("pwrss", "f", "ancova"))
    expect_equal(names(crrRes),
                 c("parms", "test", "effect", "df1", "df2", "ncp", "null.ncp", "f.alpha", "power", "n.total"))
    expect_equal(crrRes[["parms"]],
                 list(eta.squared = 0.059, null.eta.squared = 0, factor.levels = 4, k.covariates = 0, alpha = 0.05,
                      ceiling = TRUE, verbose = 0, pretty = FALSE))
    expect_equal(crrRes[c("test", "effect", "df1", "df2", "ncp", "null.ncp", "f.alpha", "power", "n.total")],
                 list(test = "F", effect = "A(4)", df1 = 3, df2 = 176, ncp = 11.2858661, null.ncp = 0, f.alpha = 2.65593888,
                      power = 0.805367138, n.total = 180))

    crrRes <- power.f.ancova(eta.squared = 0.059, factor.levels = 4, alpha = 0.05, n.total = 180, verbose = 0)
    expect_equal(class(crrRes), c("pwrss", "f", "ancova"))
    expect_equal(names(crrRes),
                 c("parms", "test", "effect", "df1", "df2", "ncp", "null.ncp", "f.alpha", "power", "n.total"))
    expect_equal(crrRes[["parms"]],
                 list(eta.squared = 0.059, null.eta.squared = 0, factor.levels = 4, k.covariates = 0, alpha = 0.05,
                      ceiling = TRUE, verbose = 0, pretty = FALSE))
    expect_equal(crrRes[c("test", "effect", "df1", "df2", "ncp", "null.ncp", "f.alpha", "power", "n.total")],
                 list(test = "F", effect = "A(4)", df1 = 3, df2 = 176, ncp = 11.2858661, null.ncp = 0, f.alpha = 2.65593888,
                      power = 0.805367138, n.total = 180))

    crrRes <- power.f.ancova(eta.squared = 0.030, factor.levels = c(2, 2), alpha = 0.05, power = 0.80, verbose = 0)
    expect_equal(class(crrRes), c("pwrss", "f", "ancova"))
    expect_equal(names(crrRes),
                 c("parms", "test", "effect", "df1", "df2", "ncp", "null.ncp", "f.alpha", "power", "n.total"))
    expect_equal(crrRes[["parms"]],
                 list(eta.squared = 0.030, null.eta.squared = 0, factor.levels = c(2, 2), k.covariates = 0, alpha = 0.05,
                      ceiling = TRUE, verbose = 0, pretty = FALSE))
    expect_equal(crrRes[c("test", "effect", "df1", "df2", "ncp", "null.ncp", "f.alpha", "power", "n.total")],
                 list(test = "F", effect = "A(2):B(2)", df1 = 1, df2 = 252, ncp = 7.91752577, null.ncp = 0, f.alpha = 3.87862445,
                      power = 0.800416655, n.total = 256))
    
    crrRes <- power.f.ancova(eta.squared = 0.048, factor.levels = 2, k.covariates = 1, alpha = 0.05, power = .80, verbose = 0)
    expect_equal(class(crrRes), c("pwrss", "f", "ancova"))
    expect_equal(names(crrRes),
                 c("parms", "test", "effect", "df1", "df2", "ncp", "null.ncp", "f.alpha", "power", "n.total"))
    expect_equal(crrRes[["parms"]],
                 list(eta.squared = 0.048, null.eta.squared = 0, factor.levels = 2, k.covariates = 1, alpha = 0.05,
                      ceiling = TRUE, verbose = 0, pretty = FALSE))
    expect_equal(crrRes[c("test", "effect", "df1", "df2", "ncp", "null.ncp", "f.alpha", "power", "n.total")],
                 list(test = "F", effect = "A(2)", df1 = 1, df2 = 155, ncp = 7.96638655, null.ncp = 0, f.alpha = 3.90215432,
                      power = 0.8009416, n.total = 158))
     
    crrRes <- power.f.ancova(eta.squared = 0.020, factor.levels = c(2, 2), k.covariates = 1, alpha = 0.05, power = .80, verbose = 0)
    expect_equal(class(crrRes), c("pwrss", "f", "ancova"))
    expect_equal(names(crrRes),
                 c("parms", "test", "effect", "df1", "df2", "ncp", "null.ncp", "f.alpha", "power", "n.total"))
    expect_equal(crrRes[["parms"]],
                 list(eta.squared = 0.020, null.eta.squared = 0, factor.levels = c(2, 2), k.covariates = 1, alpha = 0.05,
                      ceiling = TRUE, verbose = 0, pretty = FALSE))
    expect_equal(crrRes[c("test", "effect", "df1", "df2", "ncp", "null.ncp", "f.alpha", "power", "n.total")],
                 list(test = "F", effect = "A(2):B(2)", df1 = 1, df2 = 383, ncp = 7.9183673, null.ncp = 0, f.alpha = 3.8658527,
                      power = 0.80148462, n.total = 388))

    crrRes <- power.f.ancova(eta.squared = 0.020, factor.levels = c(2, 2, 2), k.covariates = 1, alpha = 0.05, power = .80, verbose = 0)
    expect_equal(class(crrRes), c("pwrss", "f", "ancova"))
    expect_equal(names(crrRes),
                 c("parms", "test", "effect", "df1", "df2", "ncp", "null.ncp", "f.alpha", "power", "n.total"))
    expect_equal(crrRes[["parms"]],
                 list(eta.squared = 0.020, null.eta.squared = 0, factor.levels = c(2, 2, 2), k.covariates = 1, alpha = 0.05,
                      ceiling = TRUE, verbose = 0, pretty = FALSE))
    expect_equal(crrRes[c("test", "effect", "df1", "df2", "ncp", "null.ncp", "f.alpha", "power", "n.total")],
                 list(test = "F", effect = "A(2):B(2):C(2)", df1 = 1, df2 = 383, ncp = 8, null.ncp = 0, f.alpha = 3.86585275,
                      power = 0.8054821, n.total = 392))

    expect_error(power.f.ancova(eta.squared = 0.059, factor.levels = 2, alpha = 0.05, verbose = 0),
                 "`n.total` and `power` cannot be NULL at the same time.")
    expect_error(power.f.ancova(eta.squared = 0.059, factor.levels = 2, alpha = 0.05, power = 0.80, n.total = 1000, verbose = 0),
                 "Exactly / only one of the parameters `n.total` or `power` should be NULL.")
    expect_error(power.f.ancova(eta.squared = 0.059, factor.levels = rep(2, 4), alpha = 0.05, power = 0.80, verbose = 0),
                 "More than three-way ANOVA or ANCOVA is not allowed at the moment.")
    expect_error(pwrss.f.ancova(eta2 = 0.059, f2 = 0.059 / (1 - 0.059), n.levels = 2, alpha = 0.05, power = .80, verbose = 0),
                 "Effect size conflict for the alternative. Specify only either `eta2` or `f2`.")

    # power.f.ancova.keppel --------------------------------------------------------------------------------------------
    crrRes <-  power.f.ancova.keppel(mu.vector = c(0.50, 0.00), sd.vector = rep(1, 2), p.vector = rep(0.50, 2),
                                     k.covariates = 1, r.squared = 0.50, alpha = 0.05, power = 0.80, verbose = 0)
    crrOut <- capture.output(power.f.ancova.keppel(mu.vector = c(0.50, 0.00), sd.vector = rep(1, 2), p.vector = rep(0.50, 2),
                                                   k.covariates = 1, r.squared = 0.50, alpha = 0.05, power = 0.80))
    crrDtl <- capture.output(power.f.ancova.keppel(mu.vector = c(0.50, 0.00), sd.vector = rep(1, 2), p.vector = rep(0.50, 2),
                                                   k.covariates = 1, r.squared = 0.50, alpha = 0.05, power = 0.80, verbose = 2))
    crrPty <- capture.output(power.f.ancova.keppel(mu.vector = c(0.50, 0.00), sd.vector = rep(1, 2), p.vector = rep(0.50, 2),
                                                   k.covariates = 1, r.squared = 0.50, alpha = 0.05, power = 0.80, pretty = TRUE))
    crrPnD <- capture.output(power.f.ancova.keppel(mu.vector = c(0.50, 0.00), sd.vector = rep(1, 2), p.vector = rep(0.50, 2),
                                                   k.covariates = 1, r.squared = 0.50, alpha = 0.05, power = 0.80, verbose = 2, pretty = TRUE))
    expect_equal(class(crrRes), c("pwrss", "f", "ancova", "keppel"))
    expect_equal(names(crrRes),
                 c("parms", "test", "effect", "eta.squared", "f", "df1", "df2", "ncp", "null.ncp", "f.alpha", "power",
                   "n.vector", "n.total"))
    expect_equal(crrRes[["parms"]],
                 list(mu.vector = c(0.50, 0.00), sd.vector = rep(1, 2), p.vector = rep(0.50, 2), factor.levels = NULL,
                      r.squared = 0.50, k.covariates = 1, alpha = 0.05, ceiling = TRUE, verbose = 0, pretty = FALSE))
    expect_equal(crrRes[c("test", "effect", "eta.squared", "f", "df1", "df2", "ncp", "null.ncp", "f.alpha", "power", "n.vector", "n.total")],
                 list(test = "F", effect = "A(2)", eta.squared = 0.111111111, f = 0.353553391, df1 = 1, df2 = 63, ncp = 8.25,
                      null.ncp = 0, f.alpha = 3.993364924, power = 0.807379456, n.vector = c(33, 33), n.total = 66))
    expect_equal(crrOut, c("+--------------------------------------------------+",
                           "|             SAMPLE SIZE CALCULATION              |",
                           "+--------------------------------------------------+", "",
                           "One-way Analysis of Covariance (F-Test)", "",
                           "----------------------------------------------------",
                           "Hypotheses",
                           "----------------------------------------------------",
                           "  H0 (Null Claim) : eta.squared = 0 ",
                           "  H1 (Alt. Claim) : eta.squared > 0 ", "",
                           "----------------------------------------------------",
                           "Results",
                           "----------------------------------------------------",
                           "  Total Sample Size    = 66  <<",
                           "  Type 1 Error (alpha) = 0.050",
                           "  Type 2 Error (beta)  = 0.193",
                           "  Statistical Power    = 0.807", ""))
    expect_equal(crrDtl, c("+--------------------------------------------------+",
                           "|             SAMPLE SIZE CALCULATION              |",
                           "+--------------------------------------------------+", "",
                           "One-way Analysis of Covariance (F-Test)", "",
                           "----------------------------------------------------",
                           "Hypotheses",
                           "----------------------------------------------------",
                           "  H0 (Null Claim) : eta.squared = 0 ",
                           "  H1 (Alt. Claim) : eta.squared > 0 ", "",
                           "----------------------------------------------------",
                           "Key Parameters",
                           "----------------------------------------------------",
                           "  Design                 = A(2) ",
                           "  Num. Deg. of Freedom   = 1 ",
                           "  Denom. Deg. of Freedom = 63 ",
                           "  Non-centrality of Alt. = 8.250 ",
                           "  Non-centrality of Null = 0.000 ",
                           "  Critical Value         = 3.993 ", "",
                           "----------------------------------------------------",
                           "Results",
                           "----------------------------------------------------",
                           "  Total Sample Size    = 66  <<",
                           "  Type 1 Error (alpha) = 0.050",
                           "  Type 2 Error (beta)  = 0.193",
                           "  Statistical Power    = 0.807", "",
                           "----------------------------------------------------",
                           "Definitions",
                           "----------------------------------------------------",
                           "  eta.squared      : (Partial) Eta-squared under alt. ", ""))
    expect_equal(crrPty, c("╔══════════════════════════════════════════════════╗",
                           "║           \033[34m SAMPLE SIZE CALCULATION \033[0m              ║",
                           "╚══════════════════════════════════════════════════╝", "",
                           "One-way Analysis of Covariance (F-Test)", "",
                           "────────────────────────────────────────────────────",
                           "Hypotheses",
                           "────────────────────────────────────────────────────",
                           "  H₀ (Null)             : η² = 0 ",
                           "  H₁ (Alternative)      : η² > 0 ", "",
                           "────────────────────────────────────────────────────",
                           "Results",
                           "────────────────────────────────────────────────────",
                           "  \033[34mTotal Sample Size = 66\033[0m  \033[1;35m◄◄\033[0m",
                           "  Type 1 Error (α)  = 0.050",
                           "  Type 2 Error (β)  = 0.193",
                           "  Statistical Power = 0.807", ""))
    expect_equal(crrPnD, c("╔══════════════════════════════════════════════════╗",
                           "║           \033[34m SAMPLE SIZE CALCULATION \033[0m              ║",
                           "╚══════════════════════════════════════════════════╝", "",
                           "One-way Analysis of Covariance (F-Test)", "",
                           "────────────────────────────────────────────────────",
                           "Hypotheses",
                           "────────────────────────────────────────────────────",
                           "  H₀ (Null)             : η² = 0 ",
                           "  H₁ (Alternative)      : η² > 0 ", "",
                           "────────────────────────────────────────────────────",
                           "Key Parameters",
                           "────────────────────────────────────────────────────",
                           "  Design                = A(2) ",
                           "  df1                   = 1",
                           "  df2                   = 63",
                           "  λ                     = 8.250",
                           "  λ₀                     = 0.000",
                           "  F⁻¹(α, λ₀)              = 3.993 ", "",
                           "────────────────────────────────────────────────────",
                           "Results",
                           "────────────────────────────────────────────────────",
                           "  \033[34mTotal Sample Size = 66\033[0m  \033[1;35m◄◄\033[0m",
                           "  Type 1 Error (α)  = 0.050",
                           "  Type 2 Error (β)  = 0.193",
                           "  Statistical Power = 0.807", "",
                           "\033[36m────────────────────────────────────────────────────\033[0m",
                           "\033[36mDefinitions\033[0m",
                           "\033[36m────────────────────────────────────────────────────\033[0m",
                           "\033[36m  η²   : (Partial) Eta-squared under alternative ",
                           "\033[0m\033[36m  λ    : Non-centrality parameter under alternative ",
                           "\033[0m\033[36m  λ₀  : Non-centrality parameter under null ", "", "\033[0m"))

    crrRes <-  power.f.ancova.keppel(mu.vector = c(0.50, 0.00), sd.vector = rep(1, 2), n.vector = rep(33, 2), p.vector = rep(0.50, 2),
                                     k.covariates = 1, r.squared = 0.50, alpha = 0.05, verbose = 0)
    expect_equal(class(crrRes), c("pwrss", "f", "ancova", "keppel"))
    expect_equal(names(crrRes),
                 c("parms", "test", "effect", "eta.squared", "f", "df1", "df2", "ncp", "null.ncp", "f.alpha", "power",
                   "n.vector", "n.total"))
    expect_equal(crrRes[["parms"]],
                 list(mu.vector = c(0.50, 0.00), sd.vector = rep(1, 2), p.vector = rep(0.50, 2), factor.levels = NULL,
                      r.squared = 0.50, k.covariates = 1, alpha = 0.05, ceiling = TRUE, verbose = 0, pretty = FALSE))
    expect_equal(crrRes[c("test", "effect", "eta.squared", "f", "df1", "df2", "ncp", "null.ncp", "f.alpha", "power", "n.vector", "n.total")],
                 list(test = "F", effect = "A(2)", eta.squared = 0.111111111, f = 0.353553391, df1 = 1, df2 = 63, ncp = 8.25,
                      null.ncp = 0, f.alpha = 3.993364924, power = 0.807379456, n.vector = c(33, 33), n.total = 66))

    crrRes <-  power.f.ancova.keppel(mu.vector = c(0.50, 0.00, 0.40, 0.20), sd.vector = rep(1, 4), p.vector = rep(0.25, 4),
                                     factor.levels = 4, k.covariates = 1, r.squared = 0.50, alpha = 0.05, power = 0.80, verbose = 0)
    expect_equal(class(crrRes), c("pwrss", "f", "ancova", "keppel"))
    expect_equal(names(crrRes),
                 c("parms", "test", "effect", "eta.squared", "f", "df1", "df2", "ncp", "null.ncp", "f.alpha", "power",
                   "n.vector", "n.total"))
    expect_equal(crrRes[["parms"]],
                 list(mu.vector = c(0.50, 0.00, 0.40, 0.20), sd.vector = rep(1, 4), p.vector = rep(0.25, 4), factor.levels = 4,
                      r.squared = 0.50, k.covariates = 1, alpha = 0.05, ceiling = TRUE, verbose = 0, pretty = FALSE))
    expect_equal(crrRes[c("test", "effect", "eta.squared", "f", "df1", "df2", "ncp", "null.ncp", "f.alpha", "power", "n.vector", "n.total")],
                 list(test = "F", effect = "A(4)", eta.squared = 0.068684517, f = 0.27156951, df1 = 3, df2 = 147, ncp = 11.21,
                      null.ncp = 0, f.alpha = 2.6661488, power = 0.800524082, n.vector = rep(38, 4), n.total = 152))

    crrRes <-  power.f.ancova.keppel(mu.vector = c(0.50, 0.00, 0.40, 0.20), sd.vector = rep(1, 4), n.vector = rep(38, 4),
                                     p.vector = rep(0.25, 4), factor.levels = 4, k.covariates = 1, r.squared = 0.50,
                                     alpha = 0.05, verbose = 0)
    expect_equal(class(crrRes), c("pwrss", "f", "ancova", "keppel"))
    expect_equal(names(crrRes),
                 c("parms", "test", "effect", "eta.squared", "f", "df1", "df2", "ncp", "null.ncp", "f.alpha", "power",
                   "n.vector", "n.total"))
    expect_equal(crrRes[["parms"]],
                 list(mu.vector = c(0.50, 0.00, 0.40, 0.20), sd.vector = rep(1, 4), p.vector = rep(0.25, 4), factor.levels = 4,
                      r.squared = 0.50, k.covariates = 1, alpha = 0.05, ceiling = TRUE, verbose = 0, pretty = FALSE))
    expect_equal(crrRes[c("test", "effect", "eta.squared", "f", "df1", "df2", "ncp", "null.ncp", "f.alpha", "power", "n.vector", "n.total")],
                 list(test = "F", effect = "A(4)", eta.squared = 0.068684517, f = 0.27156951, df1 = 3, df2 = 147, ncp = 11.21,
                      null.ncp = 0, f.alpha = 2.6661488, power = 0.800524082, n.vector = rep(38, 4), n.total = 152))

    expect_error(power.f.ancova.keppel(mu.vector = NULL, k.covariates = 1, r.squared = 0.50, alpha = 0.05, power = 0.80, verbose = 0),
                 "Provide a vector of means \\(`mu.vector`\\) with its length equal to number of groups.")
    expect_error(power.f.ancova.keppel(mu.vector = c(0.50, 0.00, 0.20, 0.30), sd.vector = rep(1, 4), p.vector = rep(0.50, 4),
                                       factor.levels = c(2, 2), k.covariates = 1, r.squared = 0.50, alpha = 0.05, power = 0.80),
                 "Factorial designs are not allowed in Keppel's approach.")
    expect_error(power.f.ancova.keppel(mu.vector = c(0.50, 0.00, 0.20, 0.30), sd.vector = rep(1, 4), p.vector = rep(0.50, 4),
                                       factor.levels = 6, k.covariates = 1, r.squared = 0.50, alpha = 0.05, power = 0.80),
                 "Length of the vector of means \\(`mu.vector`\\) does not match number of levels.")
    expect_error(power.f.ancova.keppel(mu.vector = c(0.50, 0.00), sd.vector = rep(1, 2), p.vector = rep(0.50, 2),
                                       k.covariates = 1, r.squared = 1.01, alpha = 0.05, power = 0.80, verbose = 0),
                 "R-squared \\(explanatory power of covariates\\) takes a value between 0 and 1.")
    expect_error(power.f.ancova.keppel(mu.vector = c(0.50, 0.00), sd.vector = rep(1, 2), p.vector = rep(0.50, 2),
                                       k.covariates = 0, r.squared = 0.50, alpha = 0.05, power = 0.80, verbose = 0),
                 "Explanatory power of covariates is expected to be non-zero when number of covariates is non-zero.")
    expect_error(power.f.ancova.keppel(mu.vector = c(0.50, 0.00), sd.vector = rep(1, 2), p.vector = rep(0.50, 2),
                                       k.covariates = 1, r.squared = 0.50, alpha = 0.05, verbose = 0),
                 "`n.vector` and `power` cannot be NULL at the same time.")
    expect_error(power.f.ancova.keppel(mu.vector = c(0.50, 0.00), sd.vector = rep(1, 2), n.vector = rep(50, 2), p.vector = rep(0.50, 2),
                                       k.covariates = 1, r.squared = 0.50, alpha = 0.05, power = 0.80, verbose = 0),
                 "Exactly / only one of the parameters `n.vector` or `power` should be NULL.")
    expect_error(power.f.ancova.keppel(mu.vector = c(0.50, 0.00, 0.20, 0.30), sd.vector = rep(1, 4),
                                       factor.levels = 4, k.covariates = 1, r.squared = 0.50, alpha = 0.05, power = 0.80),
                 "`p.vector` cannot be NULL when sample size is requested")
    expect_error(power.f.ancova.keppel(mu.vector = c(0.50, 0.00, 0.20, 0.30), sd.vector = rep(1, 4), p.vector = rep(0.24, 4),
                                       factor.levels = 4, k.covariates = 1, r.squared = 0.50, alpha = 0.05, power = 0.80),
                 "The elements of the `p.vector` should sum to 1")

    # factorial.contrasts ----------------------------------------------------------------------------------------------
    crrRes <- factorial.contrasts(factor.levels = 3, coding = "deviation", verbose = 0)
    expect_equal(class(crrRes), "list")
    expect_equal(names(crrRes), c("factor.levels", "factor.data", "model.matrix", "contrast.matrix"))
    expect_equal(crrRes$factor.levels, 3)
    expect_equal(crrRes$factor.data, data.frame(A = gl(3, 1)))
    expect_equal(crrRes$model.matrix, model.matrix(~ A, data.frame(A = as.factor(seq(3))), contrasts = list(A = contr.sum(3, 3))))
    expect_equal(crrRes$contrast.matrix, matrix(c(2 / 3, -1 / 3, -1 / 3, 2 / 3, -1 / 3, -1 / 3), nrow = 2,
                                                dimnames = list(c("A1", "A2"), c("A1", "A2", "A3"))))
    expect_equal(capture.output(factorial.contrasts(factor.levels = 3, coding = "deviation", verbose = 1)),
                 c("       A1     A2     A3", "A1  0.667 -0.333 -0.333", "A2 -0.333  0.667 -0.333"))

    crrRes <- factorial.contrasts(factor.levels = 3, coding = "deviation", intercept = TRUE, verbose = 0)
    expect_equal(class(crrRes), "list")
    expect_equal(names(crrRes), c("factor.levels", "factor.data", "model.matrix", "contrast.matrix"))
    expect_equal(crrRes$factor.levels, 3)
    expect_equal(crrRes$factor.data, data.frame(A = gl(3, 1)))
    expect_equal(crrRes$model.matrix, model.matrix(~ A, data.frame(A = as.factor(seq(3))), contrasts = list(A = contr.sum(3, 3))))
    expect_equal(crrRes$contrast.matrix, matrix(c(1 / 3, 2 / 3, -1 / 3, 1 / 3, -1 / 3, 2 / 3, 1 / 3, -1 / 3, -1 / 3), nrow = 3,
                                                dimnames = list(c("(Intercept)", "A1", "A2"), c("A1", "A2", "A3"))))
    expect_equal(capture.output(factorial.contrasts(factor.levels = 3, coding = "deviation", verbose = 1)),
                 c("       A1     A2     A3", "A1  0.667 -0.333 -0.333", "A2 -0.333  0.667 -0.333"))

    crrRes <- factorial.contrasts(factor.levels = 3, coding = "treatment", verbose = 0)
    expect_equal(class(crrRes), "list")
    expect_equal(names(crrRes), c("factor.levels", "factor.data", "model.matrix", "contrast.matrix"))
    expect_equal(crrRes$factor.levels, 3)
    expect_equal(crrRes$factor.data, data.frame(A = gl(3, 1)))
    expect_equal(crrRes$model.matrix, model.matrix(~ A, data.frame(A = as.factor(seq(3))), contrasts = list(A = contr.treatment(3, 3))))
    expect_equal(crrRes$contrast.matrix, matrix(c(1, 0, 0, 1, -1, -1), nrow = 2, dimnames = list(c("A1", "A2"), c("A1", "A2", "A3"))))
    expect_equal(capture.output(factorial.contrasts(factor.levels = 3, coding = "treatment", verbose = 1)),
                 c("   A1 A2 A3", "A1  1  0 -1", "A2  0  1 -1"))

    crrRes <- factorial.contrasts(factor.levels = 3, base = 1, coding = "treatment", verbose = 0)
    expect_equal(class(crrRes), "list")
    expect_equal(names(crrRes), c("factor.levels", "factor.data", "model.matrix", "contrast.matrix"))
    expect_equal(crrRes$factor.levels, 3)
    expect_equal(crrRes$factor.data, data.frame(A = gl(3, 1)))
    expect_equal(crrRes$model.matrix, model.matrix(~ A, data.frame(A = as.factor(seq(3))), contrasts = list(A = contr.treatment(3, 1))))
    expect_equal(crrRes$contrast.matrix, matrix(c(-1, -1, 1, 0, 0, 1), nrow = 2, dimnames = list(c("A2", "A3"), c("A1", "A2", "A3"))))
    expect_equal(capture.output(factorial.contrasts(factor.levels = 3, coding = "treatment", verbose = 1)),
                 c("   A1 A2 A3", "A1  1  0 -1", "A2  0  1 -1"))

    crrRes <- factorial.contrasts(factor.levels = 3, coding = "helmert", verbose = 0)
    expect_equal(class(crrRes), "list")
    expect_equal(names(crrRes), c("factor.levels", "factor.data", "model.matrix", "contrast.matrix"))
    expect_equal(crrRes$factor.levels, 3)
    expect_equal(crrRes$factor.data, data.frame(A = gl(3, 1)))
    expect_equal(crrRes$model.matrix, model.matrix(~ A, data.frame(A = as.factor(seq(3))), contrasts = list(A = contr.helmert(3, 3))))
    expect_equal(crrRes$contrast.matrix, matrix(c(-1 / 2, -1 / 6, 1 / 2, -1 / 6, 0, 1 / 3), nrow = 2,
                                                dimnames = list(c("A1", "A2"), c("A1", "A2", "A3"))))
    expect_equal(capture.output(factorial.contrasts(factor.levels = 3, coding = "helmert", verbose = 1)),
                 c("       A1     A2    A3", "A1 -0.500  0.500 0.000", "A2 -0.167 -0.167 0.333"))

    crrRes <- factorial.contrasts(factor.levels = 3, coding = "poly", verbose = 0)
    expect_equal(class(crrRes), "list")
    expect_equal(names(crrRes), c("factor.levels", "factor.data", "model.matrix", "contrast.matrix"))
    expect_equal(crrRes$factor.levels, 3)
    expect_equal(crrRes$factor.data, data.frame(A = gl(3, 1)))
    expect_equal(crrRes$model.matrix, model.matrix(~ A, data.frame(A = as.factor(seq(3))), contrasts = list(A = contr.poly)))
    expect_equal(crrRes$contrast.matrix, t(matrix(as.vector(contr.poly(3)), ncol = 2, dimnames = list(c("A1", "A2", "A3"), c("A.L", "A.Q")))))
    expect_equal(capture.output(factorial.contrasts(factor.levels = 3, coding = "poly", verbose = 1)),
                 c("        A1     A2    A3", "A.L -0.707  0.000 0.707", "A.Q  0.408 -0.816 0.408"))

    crrRes <- suppressMessages(factorial.contrasts(factor.levels = c(4, 3), coding.scheme = "deviation", intercept = FALSE, verbose = 0))
    crrOut <- suppressMessages(capture.output(factorial.contrasts(factor.levels = c(4, 3), coding.scheme = "deviation",
                                                                  intercept = FALSE, verbose = 1)))
    crrMdM <- model.matrix(~ A * B, data.frame(A = gl(4, 3), B = gl(3, 1, 3 * 4)), contrasts = list(A = contr.sum, B = contr.sum))
    expect_equal(class(crrRes), "list")
    expect_equal(names(crrRes), c("factor.levels", "factor.data", "model.matrix", "contrast.matrix"))
    expect_equal(crrRes$factor.levels, c(4, 3))
    expect_equal(crrRes$factor.data, data.frame(A = gl(4, 3), B = gl(3, 1, 3 * 4)))
    expect_equal(crrRes$model.matrix, crrMdM)
    expect_equal(unname(crrRes$contrast.matrix), unname(solve(crrMdM)[-1, ]))
    expect_equal(dimnames(crrRes$contrast.matrix),
                 list(c("A1", "A2", "A3", "B1", "B2", "A1:B1", "A2:B1", "A3:B1", "A1:B2", "A2:B2", "A3:B2"),
                      c("A1:B1", "A1:B2", "A1:B3", "A2:B1", "A2:B2", "A2:B3", "A3:B1", "A3:B2", "A3:B3", "A4:B1", "A4:B2", "A4:B3")))
    expect_equal(crrOut, c("       A1:B1  A1:B2  A1:B3  A2:B1  A2:B2  A2:B3  A3:B1  A3:B2  A3:B3  A4:B1",
                           "A1     0.250  0.250  0.250 -0.083 -0.083 -0.083 -0.083 -0.083 -0.083 -0.083",
                           "A2    -0.083 -0.083 -0.083  0.250  0.250  0.250 -0.083 -0.083 -0.083 -0.083",
                           "A3    -0.083 -0.083 -0.083 -0.083 -0.083 -0.083  0.250  0.250  0.250 -0.083",
                           "B1     0.167 -0.083 -0.083  0.167 -0.083 -0.083  0.167 -0.083 -0.083  0.167",
                           "B2    -0.083  0.167 -0.083 -0.083  0.167 -0.083 -0.083  0.167 -0.083 -0.083",
                           "A1:B1  0.500 -0.250 -0.250 -0.167  0.083  0.083 -0.167  0.083  0.083 -0.167",
                           "A2:B1 -0.167  0.083  0.083  0.500 -0.250 -0.250 -0.167  0.083  0.083 -0.167",
                           "A3:B1 -0.167  0.083  0.083 -0.167  0.083  0.083  0.500 -0.250 -0.250 -0.167",
                           "A1:B2 -0.250  0.500 -0.250  0.083 -0.167  0.083  0.083 -0.167  0.083  0.083",
                           "A2:B2  0.083 -0.167  0.083 -0.250  0.500 -0.250  0.083 -0.167  0.083  0.083",
                           "A3:B2  0.083 -0.167  0.083  0.083 -0.167  0.083 -0.250  0.500 -0.250  0.083",
                           "       A4:B2  A4:B3", "A1    -0.083 -0.083", "A2    -0.083 -0.083", "A3    -0.083 -0.083",
                           "B1    -0.083 -0.083", "B2     0.167 -0.083", "A1:B1  0.083  0.083", "A2:B1  0.083  0.083",
                           "A3:B1  0.083  0.083", "A1:B2 -0.167  0.083", "A2:B2 -0.167  0.083", "A3:B2 -0.167  0.083"))

    crrRes <- suppressMessages(factorial.contrasts(factor.levels = c(4, 3), coding.scheme = "poly", intercept = FALSE, verbose = 0))
    crrOut <- suppressMessages(capture.output(factorial.contrasts(factor.levels = c(4, 3), coding.scheme = "poly",
                                                                  intercept = FALSE, verbose = 1)))
    crrMdM <- model.matrix(~ A * B, data.frame(A = gl(4, 3), B = gl(3, 1, 3 * 4)), contrasts = list(A = contr.poly, B = contr.poly))
    expect_equal(class(crrRes), "list")
    expect_equal(names(crrRes), c("factor.levels", "factor.data", "model.matrix", "contrast.matrix"))
    expect_equal(crrRes$factor.levels, c(4, 3))
    expect_equal(crrRes$factor.data, data.frame(A = gl(4, 3), B = gl(3, 1, 3 * 4)))
    expect_equal(crrRes$model.matrix, crrMdM)
    expect_equal(unname(crrRes$contrast.matrix), unname(solve(crrMdM)[-1, ]))
    expect_equal(dimnames(crrRes$contrast.matrix),
                 list(c("A.L", "A.Q", "A.C", "B.L", "B.Q", "A.L:B.L", "A.Q:B.L", "A.C:B.L", "A.L:B.Q", "A.Q:B.Q", "A.C:B.Q"),
                      c("A1:B1", "A1:B2", "A1:B3", "A2:B1", "A2:B2", "A2:B3", "A3:B1", "A3:B2", "A3:B3", "A4:B1", "A4:B2", "A4:B3")))
    expect_equal(crrOut, c("         A1:B1  A1:B2  A1:B3  A2:B1  A2:B2  A2:B3  A3:B1  A3:B2  A3:B3  A4:B1",
                           "A.L     -0.224 -0.224 -0.224 -0.075 -0.075 -0.075  0.075  0.075  0.075  0.224",
                           "A.Q      0.167  0.167  0.167 -0.167 -0.167 -0.167 -0.167 -0.167 -0.167  0.167",
                           "A.C     -0.075 -0.075 -0.075  0.224  0.224  0.224 -0.224 -0.224 -0.224  0.075",
                           "B.L     -0.177  0.000  0.177 -0.177  0.000  0.177 -0.177  0.000  0.177 -0.177",
                           "B.Q      0.102 -0.204  0.102  0.102 -0.204  0.102  0.102 -0.204  0.102  0.102",
                           "A.L:B.L  0.474  0.000 -0.474  0.158  0.000 -0.158 -0.158  0.000  0.158 -0.474",
                           "A.Q:B.L -0.354  0.000  0.354  0.354  0.000 -0.354  0.354  0.000 -0.354 -0.354",
                           "A.C:B.L  0.158  0.000 -0.158 -0.474  0.000  0.474  0.474  0.000 -0.474 -0.158",
                           "A.L:B.Q -0.274  0.548 -0.274 -0.091  0.183 -0.091  0.091 -0.183  0.091  0.274",
                           "A.Q:B.Q  0.204 -0.408  0.204 -0.204  0.408 -0.204 -0.204  0.408 -0.204  0.204",
                           "A.C:B.Q -0.091  0.183 -0.091  0.274 -0.548  0.274 -0.274  0.548 -0.274  0.091",
                           "         A4:B2 A4:B3", "A.L      0.224 0.224", "A.Q      0.167 0.167", "A.C      0.075 0.075",
                           "B.L      0.000 0.177", "B.Q     -0.204 0.102", "A.L:B.L  0.000 0.474", "A.Q:B.L  0.000 0.354",
                           "A.C:B.L  0.000 0.158", "A.L:B.Q -0.548 0.274", "A.Q:B.Q -0.408 0.204", "A.C:B.Q -0.183 0.091"))

    crrRes <- suppressMessages(factorial.contrasts(factor.levels = c(4, 3), coding.scheme = c("deviation", "poly"), intercept = FALSE, verbose = 0))
    crrOut <- suppressMessages(capture.output(factorial.contrasts(factor.levels = c(4, 3), coding.scheme = c("deviation", "poly"),
                                                                  intercept = FALSE, verbose = 1)))
    crrMdM <- model.matrix(~ A * B, data.frame(A = gl(4, 3), B = gl(3, 1, 3 * 4)), contrasts = list(A = contr.sum, B = contr.poly))
    expect_equal(class(crrRes), "list")
    expect_equal(names(crrRes), c("factor.levels", "factor.data", "model.matrix", "contrast.matrix"))
    expect_equal(crrRes$factor.levels, c(4, 3))
    expect_equal(crrRes$factor.data, data.frame(A = gl(4, 3), B = gl(3, 1, 3 * 4)))
    expect_equal(crrRes$model.matrix, crrMdM)
    expect_equal(unname(crrRes$contrast.matrix), unname(solve(crrMdM)[-1, ]))
    expect_equal(dimnames(crrRes$contrast.matrix),
                 list(c("A1", "A2", "A3", "B.L", "B.Q", "A1:B.L", "A2:B.L", "A3:B.L", "A1:B.Q", "A2:B.Q", "A3:B.Q"),
                      c("A1:B1", "A1:B2", "A1:B3", "A2:B1", "A2:B2", "A2:B3", "A3:B1", "A3:B2", "A3:B3", "A4:B1", "A4:B2", "A4:B3")))
    expect_equal(crrOut, c("        A1:B1  A1:B2  A1:B3  A2:B1  A2:B2  A2:B3  A3:B1  A3:B2  A3:B3  A4:B1",
                           "A1      0.250  0.250  0.250 -0.083 -0.083 -0.083 -0.083 -0.083 -0.083 -0.083",
                           "A2     -0.083 -0.083 -0.083  0.250  0.250  0.250 -0.083 -0.083 -0.083 -0.083",
                           "A3     -0.083 -0.083 -0.083 -0.083 -0.083 -0.083  0.250  0.250  0.250 -0.083",
                           "B.L    -0.177  0.000  0.177 -0.177  0.000  0.177 -0.177  0.000  0.177 -0.177",
                           "B.Q     0.102 -0.204  0.102  0.102 -0.204  0.102  0.102 -0.204  0.102  0.102",
                           "A1:B.L -0.530  0.000  0.530  0.177  0.000 -0.177  0.177  0.000 -0.177  0.177",
                           "A2:B.L  0.177  0.000 -0.177 -0.530  0.000  0.530  0.177  0.000 -0.177  0.177",
                           "A3:B.L  0.177  0.000 -0.177  0.177  0.000 -0.177 -0.530  0.000  0.530  0.177",
                           "A1:B.Q  0.306 -0.612  0.306 -0.102  0.204 -0.102 -0.102  0.204 -0.102 -0.102",
                           "A2:B.Q -0.102  0.204 -0.102  0.306 -0.612  0.306 -0.102  0.204 -0.102 -0.102",
                           "A3:B.Q -0.102  0.204 -0.102 -0.102  0.204 -0.102  0.306 -0.612  0.306 -0.102",
                           "        A4:B2  A4:B3", "A1     -0.083 -0.083", "A2     -0.083 -0.083", "A3     -0.083 -0.083",
                           "B.L     0.000  0.177", "B.Q    -0.204  0.102", "A1:B.L  0.000 -0.177", "A2:B.L  0.000 -0.177",
                           "A3:B.L  0.000 -0.177", "A1:B.Q  0.204 -0.102", "A2:B.Q  0.204 -0.102", "A3:B.Q  0.204 -0.102"))

    crrRes <- suppressMessages(factorial.contrasts(factor.levels = c(3, 2, 2), coding.scheme = "poly", intercept = FALSE, verbose = 0))
    crrOut <- suppressMessages(capture.output(factorial.contrasts(factor.levels = c(3, 2, 2), coding.scheme = "poly",
                                                                  intercept = FALSE, verbose = 1)))
    crrMdM <- model.matrix(~ A * B * C, data.frame(A = gl(3, 4), B = gl(2, 2, 12), C = gl(2, 1, 12)),
                           contrasts = list(A = contr.poly, B = contr.poly, C = contr.poly))
    expect_equal(class(crrRes), "list")
    expect_equal(names(crrRes), c("factor.levels", "factor.data", "model.matrix", "contrast.matrix"))
    expect_equal(crrRes$factor.levels, c(3, 2, 2))
    expect_equal(crrRes$factor.data, data.frame(A = gl(3, 4), B = gl(2, 2, 12), C = gl(2, 1, 12)))
    expect_equal(crrRes$model.matrix, crrMdM)
    expect_equal(unname(crrRes$contrast.matrix), unname(solve(crrMdM)[-1, ]))
    expect_equal(dimnames(crrRes$contrast.matrix),
                 list(c("A.L", "A.Q", "B.L", "C.L", "A.L:B.L", "A.Q:B.L", "A.L:C.L", "A.Q:C.L", "B.L:C.L", "A.L:B.L:C.L", "A.Q:B.L:C.L"),
                      c("A1:B1:C1", "A1:B1:C2", "A1:B2:C1", "A1:B2:C2", "A2:B1:C1", "A2:B1:C2",
                        "A2:B2:C1", "A2:B2:C2", "A3:B1:C1", "A3:B1:C2", "A3:B2:C1", "A3:B2:C2")))
    expect_equal(crrOut, c("            A1:B1:C1 A1:B1:C2 A1:B2:C1 A1:B2:C2 A2:B1:C1 A2:B1:C2 A2:B2:C1",
                           "A.L           -0.177   -0.177   -0.177   -0.177    0.000    0.000    0.000",
                           "A.Q            0.102    0.102    0.102    0.102   -0.204   -0.204   -0.204",
                           "B.L           -0.118   -0.118    0.118    0.118   -0.118   -0.118    0.118",
                           "C.L           -0.118    0.118   -0.118    0.118   -0.118    0.118   -0.118",
                           "A.L:B.L        0.250    0.250   -0.250   -0.250    0.000    0.000    0.000",
                           "A.Q:B.L       -0.144   -0.144    0.144    0.144    0.289    0.289   -0.289",
                           "A.L:C.L        0.250   -0.250    0.250   -0.250    0.000    0.000    0.000",
                           "A.Q:C.L       -0.144    0.144   -0.144    0.144    0.289   -0.289    0.289",
                           "B.L:C.L        0.167   -0.167   -0.167    0.167    0.167   -0.167   -0.167",
                           "A.L:B.L:C.L   -0.354    0.354    0.354   -0.354    0.000    0.000    0.000",
                           "A.Q:B.L:C.L    0.204   -0.204   -0.204    0.204   -0.408    0.408    0.408",
                           "            A2:B2:C2 A3:B1:C1 A3:B1:C2 A3:B2:C1 A3:B2:C2",
                           "A.L            0.000    0.177    0.177    0.177    0.177",
                           "A.Q           -0.204    0.102    0.102    0.102    0.102",
                           "B.L            0.118   -0.118   -0.118    0.118    0.118",
                           "C.L            0.118   -0.118    0.118   -0.118    0.118",
                           "A.L:B.L        0.000   -0.250   -0.250    0.250    0.250",
                           "A.Q:B.L       -0.289   -0.144   -0.144    0.144    0.144",
                           "A.L:C.L        0.000   -0.250    0.250   -0.250    0.250",
                           "A.Q:C.L       -0.289   -0.144    0.144   -0.144    0.144",
                           "B.L:C.L        0.167    0.167   -0.167   -0.167    0.167",
                           "A.L:B.L:C.L    0.000    0.354   -0.354   -0.354    0.354",
                           "A.Q:B.L:C.L   -0.408    0.204   -0.204   -0.204    0.204"))

    expect_equal(factorial.contrasts(factor.levels = c(3, 3), coding = rep("deviation", 2), verbose = -1),
                 factorial.contrasts(factor.levels = c(3, 3), coding = rep("deviation", 2), base = 3, verbose = -1))
    expect_equal(factorial.contrasts(factor.levels = c(3, 3), coding = rep("deviation", 2), verbose = -1),
                 factorial.contrasts(factor.levels = c(3, 3), coding = rep("deviation", 2), base = rep(3, 3), verbose = -1))
    expect_message(factorial.contrasts(factor.levels = 3, coding = c("deviation", "poly", "helmert", "sum"), verbose = 0),
                   "Provide as many coding schemes as number of factors. Using the first 1.")
    crrMsg <- capture_messages(factorial.contrasts(factor.levels = c(2, 2, 2), coding = c("deviation"), verbose = 0))
    expect_equal(crrMsg, c("Assuming the same coding scheme applies to the other factor(s)\n",
                           paste0("Elements of `mu.vector`, `sd.vector`, `n.vector` or `p.vector` should follow this specific order:\n",
                                  "A1:B1:C1  A1:B1:C2  A1:B2:C1  A1:B2:C2  A2:B1:C1  A2:B1:C2  A2:B2:C1  A2:B2:C2  \n\n")))
    expect_error(factorial.contrasts(factor.levels = 3, coding = "invalid", verbose = 0),
                 "Contrast type \"invalid\" not supported at the moment.")
    expect_error(factorial.contrasts(factor.levels = rep(2, 4), coding = rep("poly", 4), verbose = 0),
                 "This version supports only up to three-way interactions.")

    # power.f.ancova.shieh / power.t.contrasts / power.t.contrast ------------------------------------------------------
    crrRes <- power.f.ancova.shieh(mu.vector = c(0.20, 0), sd.vector = rep(1, 2), n.vector = rep(150, 2), r.squared = 0.50,
                                   k.covariates = 1, alpha = 0.05, verbose = 0)
    crrOut <- capture.output(power.f.ancova.shieh(mu.vector = c(0.20, 0), sd.vector = rep(1, 2), n.vector = rep(150, 2),
                                                  r.squared = 0.50, k.covariates = 1, alpha = 0.05))
    crrDtl <- capture.output(power.f.ancova.shieh(mu.vector = c(0.20, 0), sd.vector = rep(1, 2), n.vector = rep(150, 2),
                                                  r.squared = 0.50, k.covariates = 1, alpha = 0.05, verbose = 2))
    crrPty <- capture.output(power.f.ancova.shieh(mu.vector = c(0.20, 0), sd.vector = rep(1, 2), n.vector = rep(150, 2),
                                                  r.squared = 0.50, k.covariates = 1, alpha = 0.05, pretty = TRUE))
    expect_equal(class(crrRes), c("pwrss", "f", "ancova", "shieh"))
    expect_equal(names(crrRes),
                 c("parms", "test", "effect", "eta.squared", "f", "df1", "df2", "ncp", "null.ncp", "f.alpha", "power",
                   "n.vector", "n.total"))
    expect_equal(crrRes[["parms"]],
                 list(mu.vector = c(0.2, 0.0), sd.vector = rep(1, 2), p.vector = NULL, factor.levels = NULL,
                      r.squared = 0.5, k.covariates = 1, contrast.matrix = NULL, alpha = 0.05, ceiling = TRUE,
                      verbose = 0, pretty = FALSE))
    expect_equal(crrRes[c("test", "effect", "eta.squared", "f", "df1", "df2", "ncp", "null.ncp", "f.alpha", "power", "n.vector", "n.total")],
                 list(test = "F", effect = "A(2)", eta.squared = 0.019543331, f = 0.141183873, df1 = 1, df2 = 297, ncp = 5.97986577,
                      null.ncp = 0, f.alpha = 3.87295916, power = 0.683501582, n.vector = c(150, 150), n.total = 300))
    expect_equal(crrOut, c("+--------------------------------------------------+",
                           "|                POWER CALCULATION                 |",
                           "+--------------------------------------------------+", "",
                           "One-way Analysis of Covariance (F-Test)", "",
                           "----------------------------------------------------",
                           "Hypotheses",
                           "----------------------------------------------------",
                           "  H0 (Null Claim) : eta.squared = 0 ",
                           "  H1 (Alt. Claim) : eta.squared > 0 ", "",
                           "----------------------------------------------------",
                           "Results",
                           "----------------------------------------------------",
                           "  Total Sample Size    = 300",
                           "  Type 1 Error (alpha) = 0.050",
                           "  Type 2 Error (beta)  = 0.316",
                           "  Statistical Power    = 0.684  <<", ""))
    expect_equal(crrDtl, c("+--------------------------------------------------+",
                           "|                POWER CALCULATION                 |",
                           "+--------------------------------------------------+", "",
                           "One-way Analysis of Covariance (F-Test)", "",
                           "----------------------------------------------------",
                           "Hypotheses",
                           "----------------------------------------------------",
                           "  H0 (Null Claim) : eta.squared = 0 ",
                           "  H1 (Alt. Claim) : eta.squared > 0 ", "",
                           "----------------------------------------------------",
                           "Key Parameters",
                           "----------------------------------------------------",
                           "  Design                 = A(2) ",
                           "  Num. Deg. of Freedom   = 1 ",
                           "  Denom. Deg. of Freedom = 297 ",
                           "  Non-centrality of Alt. = 5.980 ",
                           "  Non-centrality of Null = 0.000 ",
                           "  Critical Value         = 3.873 ", "",
                           "----------------------------------------------------",
                           "Results",
                           "----------------------------------------------------",
                           "  Total Sample Size    = 300",
                           "  Type 1 Error (alpha) = 0.050",
                           "  Type 2 Error (beta)  = 0.316",
                           "  Statistical Power    = 0.684  <<", "",
                           "----------------------------------------------------",
                           "Definitions",
                           "----------------------------------------------------",
                           "  eta.squared      : (Partial) Eta-squared under alt. ", ""))
    expect_equal(crrPty, c("╔══════════════════════════════════════════════════╗",
                           "║               \033[34m POWER CALCULATION \033[0m                ║",
                           "╚══════════════════════════════════════════════════╝", "",
                           "One-way Analysis of Covariance (F-Test)", "",
                           "────────────────────────────────────────────────────",
                           "Hypotheses",
                           "────────────────────────────────────────────────────",
                           "  H₀ (Null)             : η² = 0 ",
                           "  H₁ (Alternative)      : η² > 0 ", "",
                           "────────────────────────────────────────────────────",
                           "Results",
                           "────────────────────────────────────────────────────",
                           "  Total Sample Size = 300",
                           "  Type 1 Error (α)  = 0.050",
                           "  Type 2 Error (β)  = 0.316",
                           "  \033[34mStatistical Power = 0.684\033[0m  \033[1;35m◄◄\033[0m", ""))
    
    crrRes <- power.f.ancova.shieh(mu.vector = c(0.20, 0), sd.vector = rep(1, 2), p.vector = rep(0.50, 2), r.squared = 0.50,
                                   k.covariates = 1, alpha = 0.05, power = 0.80, verbose = 0)
    expect_equal(class(crrRes), c("pwrss", "f", "ancova", "shieh"))
    expect_equal(names(crrRes),
                 c("parms", "test", "effect", "eta.squared", "f", "df1", "df2", "ncp", "null.ncp", "f.alpha", "power",
                   "n.vector", "n.total"))
    expect_equal(crrRes[["parms"]],
                 list(mu.vector = c(0.2, 0.0), sd.vector = rep(1, 2), p.vector = rep(0.5, 2), factor.levels = NULL,
                      r.squared = 0.5, k.covariates = 1, contrast.matrix = NULL, alpha = 0.05, ceiling = TRUE,
                      verbose = 0, pretty = FALSE))
    expect_equal(crrRes[c("test", "effect", "eta.squared", "f", "df1", "df2", "ncp", "null.ncp", "f.alpha", "power", "n.vector", "n.total")],
                 list(test = "F", effect = "A(2)", eta.squared = 0.01955905041, f = 0.141241773, df1 = 1, df2 = 393, ncp = 7.89989848,
                      null.ncp = 0, f.alpha = 3.86522919, power = 0.800619012, n.vector = c(198, 198), n.total = 396))

    crrRes <- power.f.ancova.shieh(mu.vector = c(0.20, 0), sd.vector = rep(1, 2), p.vector = rep(0.50, 2), r.squared = 0.50,
                                   k.covariates = 2, alpha = 0.05, power = 0.80, verbose = 0)
    expect_equal(class(crrRes), c("pwrss", "f", "ancova", "shieh"))
    expect_equal(names(crrRes),
                 c("parms", "test", "effect", "eta.squared", "f", "df1", "df2", "ncp", "null.ncp", "f.alpha", "power",
                   "n.vector", "n.total"))
    expect_equal(crrRes[["parms"]],
                 list(mu.vector = c(0.2, 0.0), sd.vector = rep(1, 2), p.vector = rep(0.5, 2), factor.levels = NULL,
                      r.squared = 0.5, k.covariates = 2, contrast.matrix = NULL, alpha = 0.05, ceiling = TRUE,
                      verbose = 0, pretty = FALSE))
    expect_equal(crrRes[c("test", "effect", "eta.squared", "f", "df1", "df2", "ncp", "null.ncp", "f.alpha", "power", "n.vector", "n.total")],
                 list(test = "F", effect = "A(2)", eta.squared = 0.0195109904, f = 0.141064681, df1 = 1, df2 = 394, ncp = 7.91989924,
                      null.ncp = 0, f.alpha = 3.86516859, power = 0.801596275, n.vector = c(199, 199), n.total = 398))
 
    crrRes <- power.f.ancova.shieh(mu.vector = c(0.20, 0.25, 0.15, 0.05), sd.vector = rep(1, 4), p.vector = rep(0.25, 4),
                                   factor.levels = c(2, 2), r.squared = 0.50, k.covariates = 1, alpha = 0.05,
                                   power = 0.80, verbose = -1)
    expect_equal(class(crrRes), c("pwrss", "f", "ancova", "shieh"))
    expect_equal(names(crrRes),
                 c("parms", "test", "effect", "eta.squared", "f", "df1", "df2", "ncp", "null.ncp", "f.alpha", "power",
                   "n.vector", "n.total"))
    expect_equal(crrRes[["parms"]],
                 list(mu.vector = c(0.20, 0.25, 0.15, 0.05), sd.vector = rep(1, 4), p.vector = rep(0.25, 4),
                      factor.levels = c(2, 2), r.squared = 0.5, k.covariates = 1, contrast.matrix = NULL, alpha = 0.05,
                      ceiling = TRUE, verbose = -1, pretty = FALSE))
    expect_equal(crrRes[c("test", "effect", "eta.squared", "f", "df1", "df2", "ncp", "null.ncp", "f.alpha", "power", "n.vector", "n.total")],
                 list(test = "F", effect = "A(2):B(2)", eta.squared = 0.0028036103, f = 0.05302351, df1 = 1, df2 = 2791, ncp = 7.86093347,
                      null.ncp = 0, f.alpha = 3.84479279, power = 0.800332660, n.vector = rep(699, 4), n.total = 2796))

    crrRes <- power.f.ancova.shieh(mu.vector = c(0.20, 0.25, 0.30, 0.15, 0.05, 0.10, 0.00, 0.05), sd.vector = rep(1, 8),
                                   p.vector = rep(1 / 8, 8), factor.levels = c(2, 2, 2), r.squared = 0.50, k.covariates = 1,
                                   alpha = 0.05, power = 0.80, verbose = -1)
    expect_equal(class(crrRes), c("pwrss", "f", "ancova", "shieh"))
    expect_equal(names(crrRes),
                 c("parms", "test", "effect", "eta.squared", "f", "df1", "df2", "ncp", "null.ncp", "f.alpha", "power",
                   "n.vector", "n.total"))
    expect_equal(crrRes[["parms"]],
                 list(mu.vector = c(0.20, 0.25, 0.30, 0.15, 0.05, 0.10, 0.00, 0.05), sd.vector = rep(1, 8),
                      p.vector = rep(1 / 8, 8), factor.levels = c(2, 2, 2), r.squared = 0.5, k.covariates = 1,
                      contrast.matrix = NULL, alpha = 0.05, ceiling = TRUE, verbose = -1, pretty = FALSE))
    expect_equal(crrRes[c("test", "effect", "eta.squared", "f", "df1", "df2", "ncp", "null.ncp", "f.alpha", "power", "n.vector", "n.total")],
                 list(test = "F", effect = "A(2):B(2):C(2)", eta.squared = 0.0012482409, f = 0.035352524, df1 = 1, df2 = 6279,
                      ncp = 7.85874841, null.ncp = 0, f.alpha = 3.84294023, power = 0.800373625, n.vector = rep(786, 8), n.total = 6288))
     
    mtxCnt <- factorial.contrasts(factor.levels = 3, coding = "treatment", verbose = 0)$contrast.matrix
    crrRes <- power.f.ancova.shieh(mu.vector = c(0.15, 0.30, 0.20), sd.vector = rep(1, 3), p.vector = rep(1 / 3, 3),
                                   contrast.matrix = mtxCnt, r.squared = 0.50, k.covariates = 1, alpha = 0.05,
                                   power = 0.80, verbose = 0)
    expect_equal(class(crrRes), c("pwrss", "f", "ancova", "shieh"))
    expect_equal(names(crrRes),
                 c("parms", "test", "effect", "eta.squared", "f", "df1", "df2", "ncp", "null.ncp", "f.alpha", "power",
                   "n.vector", "n.total"))
    expect_equal(crrRes[["parms"]],
                 list(mu.vector = c(0.15, 0.30, 0.20), sd.vector = rep(1, 3), p.vector = rep(1 / 3, 3),
                      factor.levels = NULL, r.squared = 0.5, k.covariates = 1, contrast.matrix = mtxCnt, alpha = 0.05,
                      ceiling = TRUE, verbose = 0, pretty = FALSE))
    expect_equal(crrRes[c("test", "effect", "eta.squared", "f", "df1", "df2", "ncp", "null.ncp", "f.alpha", "power", "n.vector", "n.total")],
                 list(test = "F", effect = "A(3)", eta.squared = 0.0077115847, f = 0.0881562, df1 = 2, df2 = 1241, ncp = 9.67553677,
                      null.ncp = 0, f.alpha = 3.00297552, power = 0.800762614, n.vector = rep(415, 3), n.total = 1245))
    crrCnt <- power.t.contrasts(crrRes, adjust.alpha = "fdr", verbose = 0)
    crrOut <- capture.output(power.t.contrasts(crrRes, adjust.alpha = "fdr"))
    crrDtl <- capture.output(power.t.contrasts(crrRes, adjust.alpha = "fdr", verbose = 2))
    crrPty <- capture.output(power.t.contrasts(crrRes, adjust.alpha = "fdr", pretty = TRUE))
    crrDnP <- capture.output(power.t.contrasts(crrRes, adjust.alpha = "fdr", verbose = 2, pretty = TRUE))
    expect_equal(class(crrCnt), c("pwrss", "t", "contrasts"))
    expect_equal(names(crrCnt), c("parms", "test", "contrast", "comparison", "psi", "d", "ncp", "df", "t.alpha", "n.total", "power"))
    expect_equal(crrCnt[["parms"]],
                 list(mu.vector = c(0.15, 0.30, 0.20), sd.vector = rep(1, 3), p.vector = rep(1 / 3, 3), r.squared = 0.5,
                      k.covariates = 1, contrast.matrix = mtxCnt, alpha = 0.05, adjust.alpha = "fdr", ceiling = TRUE,
                      verbose = 0, pretty = FALSE))
    expect_equal(crrCnt[c("test", "contrast", "comparison", "psi", "d", "ncp", "df", "t.alpha", "n.total", "power")],
                 list(test = "t", contrast = seq(2), comparison = c("A1 <=> A3", "A2 <=> A3"), psi = c(-0.05, 0.10),
                      d = c(-0.070710678, 0.141421356), ncp = c(-1.018167467, 2.036334933), df = rep(1241, 2),
                      t.alpha = rep(2.24412588, 2), n.total = rep(1245, 2), power = c(0.110986043, 0.417964282)))
    expect_equal(crrOut, c("+--------------------------------------------------+",
                           "|                POWER CALCULATION                 |",
                           "+--------------------------------------------------+", "",
                           "Multiple Contrast Analyses (T-Tests)", "",
                           "----------------------------------------------------",
                           "Hypotheses",
                           "----------------------------------------------------",
                           "  H0 (Null Claim) : psi = 0 ",
                           "  H1 (Alt. Claim) : psi != 0 ", "",
                           "----------------------------------------------------",
                           "Results",
                           "----------------------------------------------------",
                           " contr comparison   psi      d    ncp n.total power",
                           "     1  A1 <=> A3 -0.05 -0.071 -1.018    1245 0.111",
                           "     2  A2 <=> A3  0.10  0.141  2.036    1245 0.418", ""))
    expect_equal(crrDtl, c("+--------------------------------------------------+",
                           "|                POWER CALCULATION                 |",
                           "+--------------------------------------------------+", "",
                           "Multiple Contrast Analyses (T-Tests)", "",
                           "----------------------------------------------------",
                           "Hypotheses",
                           "----------------------------------------------------",
                           "  H0 (Null Claim) : psi = 0 ",
                           "  H1 (Alt. Claim) : psi != 0 ", "",
                           "----------------------------------------------------",
                           "Key Parameters",
                           "----------------------------------------------------",
                           "  Alpha Adjustment       = False Discovery Rate ",
                           "  Adjusted Alpha         = 0.025 ",
                           "  Non-centrality of Null = 0.000 ", "",
                           "----------------------------------------------------",
                           "Results",
                           "----------------------------------------------------",
                           " contr comparison   psi      d    ncp n.total power",
                           "     1  A1 <=> A3 -0.05 -0.071 -1.018    1245 0.111",
                           "     2  A2 <=> A3  0.10  0.141  2.036    1245 0.418", "",
                           "----------------------------------------------------",
                           "Definitions",
                           "----------------------------------------------------",
                           "  psi : Contrast estimate, sum(contrast[i] * mu[i])",
                           "  d   : Standardized contrast estimate ",
                           "  ncp : Non-centrality parameter under alt. ", ""))
    expect_equal(crrPty, c("╔══════════════════════════════════════════════════╗",
                           "║               \033[34m POWER CALCULATION \033[0m                ║",
                           "╚══════════════════════════════════════════════════╝", "",
                           "Multiple Contrast Analyses (T-Tests)", "",
                           "────────────────────────────────────────────────────",
                           "Hypotheses",
                           "────────────────────────────────────────────────────",
                           "  H₀ (Null)                 : ψ = 0 ",
                           "  H₁ (Alternative)          : ψ ≠ 0 ", "",
                           "────────────────────────────────────────────────────",
                           "Results",
                           "────────────────────────────────────────────────────",
                           " contr comparison   psi      d    ncp n.total power",
                           "     1  A1 <=> A3 -0.05 -0.071 -1.018    1245 0.111",
                           "     2  A2 <=> A3  0.10  0.141  2.036    1245 0.418", ""))
    expect_equal(crrDnP, c("╔══════════════════════════════════════════════════╗",
                           "║               \033[34m POWER CALCULATION \033[0m                ║",
                           "╚══════════════════════════════════════════════════╝", "",
                           "Multiple Contrast Analyses (T-Tests)", "",
                           "────────────────────────────────────────────────────",
                           "Hypotheses",
                           "────────────────────────────────────────────────────",
                           "  H₀ (Null)                 : ψ = 0 ",
                           "  H₁ (Alternative)          : ψ ≠ 0 ", "",
                           "────────────────────────────────────────────────────",
                           "Key Parameters",
                           "────────────────────────────────────────────────────",
                           "  λ₀                     = 0.000",
                           "  Adjusted Type 1 Error (α) = 0.025 ",
                           "  α Adjustment              = False Discovery Rate ", "",
                           "────────────────────────────────────────────────────",
                           "Results",
                           "────────────────────────────────────────────────────",
                           " contr comparison   psi      d    ncp n.total power",
                           "     1  A1 <=> A3 -0.05 -0.071 -1.018    1245 0.111",
                           "     2  A2 <=> A3  0.10  0.141  2.036    1245 0.418", "",
                           "\033[36m────────────────────────────────────────────────────\033[0m",
                           "\033[36mDefinitions\033[0m",
                           "\033[36m────────────────────────────────────────────────────\033[0m",
                           "\033[36m  ψ (psi)   : Contrast est. defined as ∑(contrastᵢ * μᵢ) ",
                           "\033[0m\033[36m  d         : Standardized contrast estimate ", "", "\033[0m"))

    mtxCnt <- factorial.contrasts(factor.levels = 3, coding = "helmert", verbose = 0)$contrast.matrix
    crrRes <- power.f.ancova.shieh(mu.vector = c(0.15, 0.30, 0.20), sd.vector = rep(1, 3), p.vector = rep(1/3, 3),
                                   contrast.matrix = mtxCnt, r.squared = 0.50, k.covariates = 1, alpha = 0.05,
                                   power = 0.80, verbose = 0)
    crrCnt <- power.t.contrasts(crrRes, verbose = 0)
    expect_equal(class(crrRes), c("pwrss", "f", "ancova", "shieh"))
    expect_equal(names(crrRes),
                 c("parms", "test", "effect", "eta.squared", "f", "df1", "df2", "ncp", "null.ncp", "f.alpha", "power",
                   "n.vector", "n.total"))
    expect_equal(crrRes[["parms"]],
                 list(mu.vector = c(0.15, 0.30, 0.20), sd.vector = rep(1, 3), p.vector = rep(1 / 3, 3),
                      factor.levels = NULL, r.squared = 0.5, k.covariates = 1, contrast.matrix = mtxCnt, alpha = 0.05,
                      ceiling = TRUE, verbose = 0, pretty = FALSE))
    expect_equal(crrRes[c("test", "effect", "eta.squared", "f", "df1", "df2", "ncp", "null.ncp", "f.alpha", "power", "n.vector", "n.total")],
                 list(test = "F", effect = "A(3)", eta.squared = 0.0077115847, f = 0.0881562, df1 = 2, df2 = 1241, ncp = 9.67553677,
                      null.ncp = 0, f.alpha = 3.00297552, power = 0.800762614, n.vector = rep(415, 3), n.total = 1245))
    expect_equal(class(crrCnt), c("pwrss", "t", "contrasts"))
    expect_equal(names(crrCnt), c("parms", "test", "contrast", "comparison", "psi", "d", "ncp", "df", "t.alpha", "n.total", "power"))
    expect_equal(crrCnt[["parms"]],
                 list(mu.vector = c(0.15, 0.30, 0.20), sd.vector = rep(1, 3), p.vector = rep(1 / 3, 3), r.squared = 0.5,
                      k.covariates = 1, contrast.matrix = mtxCnt, alpha = 0.05, adjust.alpha = "none", ceiling = TRUE,
                      verbose = 0, pretty = FALSE))
    expect_equal(crrCnt[c("test", "contrast", "comparison", "psi", "d", "ncp", "df", "t.alpha", "n.total", "power")],
                 list(test = "t", contrast = seq(2), comparison = c("A2 <=> A1", "A3 <=> A1 A2"), psi = c(0.075, -0.0083333333),
                      d = c(0.106066017, -0.011785113), ncp = c(3.0545024, -0.5878393), df = rep(1241, 2),
                      t.alpha = rep(1.9618774, 2), n.total = rep(1245, 2), power = c(0.86262155, 0.09036882)))
    crrCnt <- do.call(power.t.contrast,
                      c(crrRes[["parms"]][c("mu.vector", "sd.vector", "p.vector", "r.squared", "k.covariates", "alpha")],
                        list(contrast.vector = mtxCnt[1, ], power = 0.8, verbose = 0)))
    expect_equal(class(crrCnt), c("pwrss", "t", "contrast"))
    expect_equal(names(crrCnt), c("parms", "test", "psi", "d", "df", "t.alpha", "ncp", "ncp.null", "power", "n.vector", "n.total"))
    expect_equal(crrCnt[["parms"]],
                 list(mu.vector = c(0.15, 0.30, 0.20), sd.vector = rep(1, 3), p.vector = rep(1 / 3, 3), contrast.vector = mtxCnt[1, ],
                      r.squared = 0.5, k.covariates = 1, alpha = 0.05, tukey.kramer = FALSE, ceiling = TRUE, verbose = 0,
                      pretty = FALSE))
    expect_equal(crrCnt[c("test", "psi", "d", "df", "t.alpha", "ncp", "ncp.null", "power", "n.vector", "n.total")],
                 list(test = "t", psi = 0.075, d = 0.106066017, df = 1046, t.alpha = c(-1.9622345, 1.9622345), ncp = 2.8049032,
                      ncp.null = 0, power = 0.800208158, n.vector = rep(350, 3), n.total = 1050))

    mtxCnt <- factorial.contrasts(factor.levels = 3, coding = "poly", verbose = 0)$contrast.matrix
    crrRes <- power.f.ancova.shieh(mu.vector = c(0.15, 0.30, 0.20), sd.vector = rep(1, 3), p.vector = rep(1 / 3, 3),
                                   contrast.matrix = mtxCnt, r.squared = 0.50, k.covariates = 1, alpha = 0.05,
                                   power = 0.80, verbose = 0)
    crrCnt <- power.t.contrasts(crrRes, verbose = 0)
    expect_equal(class(crrRes), c("pwrss", "f", "ancova", "shieh"))
    expect_equal(names(crrRes),
                 c("parms", "test", "effect", "eta.squared", "f", "df1", "df2", "ncp", "null.ncp", "f.alpha", "power",
                   "n.vector", "n.total"))
    expect_equal(crrRes[["parms"]],
                 list(mu.vector = c(0.15, 0.30, 0.20), sd.vector = rep(1, 3), p.vector = rep(1 / 3, 3),
                      factor.levels = NULL, r.squared = 0.5, k.covariates = 1, contrast.matrix = mtxCnt, alpha = 0.05,
                      ceiling = TRUE, verbose = 0, pretty = FALSE))
    expect_equal(crrRes[c("test", "effect", "eta.squared", "f", "df1", "df2", "ncp", "null.ncp", "f.alpha", "power", "n.vector", "n.total")],
                 list(test = "F", effect = "A(3)", eta.squared = 0.0077115847, f = 0.0881562, df1 = 2, df2 = 1241, ncp = 9.67553677,
                      null.ncp = 0, f.alpha = 3.00297552, power = 0.800762614, n.vector = rep(415, 3), n.total = 1245))
    expect_equal(class(crrCnt), c("pwrss", "t", "contrasts"))
    expect_equal(names(crrCnt), c("parms", "test", "contrast", "comparison", "psi", "d", "ncp", "df", "t.alpha", "n.total", "power"))
    expect_equal(crrCnt[["parms"]],
                 list(mu.vector = c(0.15, 0.30, 0.20), sd.vector = rep(1, 3), p.vector = rep(1 / 3, 3), r.squared = 0.5,
                      k.covariates = 1, contrast.matrix = mtxCnt, alpha = 0.05, adjust.alpha = "none", ceiling = TRUE,
                      verbose = 0, pretty = FALSE))
    expect_equal(crrCnt[c("test", "contrast", "comparison", "psi", "d", "ncp", "df", "t.alpha", "n.total", "power")],
                 list(test = "t", contrast = seq(2), comparison = c("A3 <=> A1", "A1 A3 <=> A2"), psi = c(0.035355339, -0.102062073),
                      d = c(0.05, -0.144337567), ncp = c(1.01816747, -2.9391963), df = rep(1241, 2),
                      t.alpha = rep(1.9618774, 2), n.total = rep(1245, 2), power = c(0.174400616, 0.835704937)))
    expect_equal(crrCnt, do.call(power.t.contrasts,
                                 c(crrRes[["parms"]][c("mu.vector", "sd.vector", "p.vector", "r.squared", "k.covariates", "alpha",
                                                       "contrast.matrix")], list(n.vector = crrRes$n.vector, verbose = 0))))
    crrCnt <- do.call(power.t.contrast,
                      c(crrRes[["parms"]][c("mu.vector", "sd.vector", "p.vector", "r.squared", "k.covariates", "alpha")],
                        list(contrast.vector = mtxCnt[1, ], power = 0.8, verbose = 0)))
    expect_equal(class(crrCnt), c("pwrss", "t", "contrast"))
    expect_equal(names(crrCnt), c("parms", "test", "psi", "d", "df", "t.alpha", "ncp", "ncp.null", "power", "n.vector", "n.total"))
    expect_equal(crrCnt[["parms"]],
                 list(mu.vector = c(0.15, 0.30, 0.20), sd.vector = rep(1, 3), p.vector = rep(1 / 3, 3), contrast.vector = mtxCnt[1, ],
                      r.squared = 0.5, k.covariates = 1, alpha = 0.05, tukey.kramer = FALSE, ceiling = TRUE, verbose = 0,
                      pretty = FALSE))
    expect_equal(crrCnt[c("test", "psi", "d", "df", "t.alpha", "ncp", "ncp.null", "power", "n.vector", "n.total")],
                 list(test = "t", psi = 0.035355339, d = 0.05, df = 9419, t.alpha = c(-1.96021588, 1.96021588), ncp = 2.80208252,
                      ncp.null = 0, power = 0.80006019, n.vector = rep(3141, 3), n.total = 9423))

    mtxCnt <- factorial.contrasts(factor.levels = 3, coding = "poly", verbose = 0)$contrast.matrix
    crrRes <- power.f.ancova.shieh(mu.vector = c(0.15, 0.30, 0.20), sd.vector = rep(1, 3), p.vector = rep(1 / 3, 3),
                                   contrast.matrix = mtxCnt, r.squared = 0.50, k.covariates = 2, alpha = 0.05,
                                   power = 0.80, verbose = 0)
    crrCnt <- power.t.contrasts(crrRes, adjust.alpha = "tukey", verbose = 0)
    expect_equal(class(crrRes), c("pwrss", "f", "ancova", "shieh"))
    expect_equal(names(crrRes),
                 c("parms", "test", "effect", "eta.squared", "f", "df1", "df2", "ncp", "null.ncp", "f.alpha", "power",
                   "n.vector", "n.total"))
    expect_equal(crrRes[["parms"]],
                 list(mu.vector = c(0.15, 0.30, 0.20), sd.vector = rep(1, 3), p.vector = rep(1 / 3, 3),
                      factor.levels = NULL, r.squared = 0.5, k.covariates = 2, contrast.matrix = mtxCnt, alpha = 0.05,
                      ceiling = TRUE, verbose = 0, pretty = FALSE))
    expect_equal(crrRes[c("test", "effect", "eta.squared", "f", "df1", "df2", "ncp", "null.ncp", "f.alpha", "power", "n.vector", "n.total")],
                 list(test = "F", effect = "A(3)", eta.squared = 0.0077054286, f = 0.08812073, df1 = 2, df2 = 1240, ncp = 9.66775275,
                      null.ncp = 0, f.alpha = 3.0029814, power = 0.80041063, n.vector = rep(415, 3), n.total = 1245))
    expect_equal(class(crrCnt), c("pwrss", "t", "contrasts"))
    expect_equal(names(crrCnt), c("parms", "test", "contrast", "comparison", "psi", "d", "ncp", "df", "t.alpha", "n.total", "power"))
    expect_equal(crrCnt[["parms"]],
                 list(mu.vector = c(0.15, 0.30, 0.20), sd.vector = rep(1, 3), p.vector = rep(1 / 3, 3), r.squared = 0.5,
                      k.covariates = 2, contrast.matrix = mtxCnt, alpha = 0.05, adjust.alpha = "tukey", ceiling = TRUE,
                      verbose = 0, pretty = FALSE))
    expect_equal(crrCnt[c("test", "contrast", "comparison", "psi", "d", "ncp", "df", "t.alpha", "n.total", "power")],
                 list(test = "t", contrast = seq(2), comparison = c("A3 <=> A1", "A1 A3 <=> A2"), psi = c(0.035355339, -0.102062073),
                      d = c(0.05, -0.144337567), ncp = c(1.017741531, -2.937966734), df = rep(1240, 2), t.alpha = rep(2.34652362, 2),
                      n.total = rep(1245, 2), power = c(0.09267207, 0.72283065)))
    expect_equal(crrCnt, do.call(power.t.contrasts,
                                 c(crrRes[["parms"]][c("mu.vector", "sd.vector", "p.vector", "r.squared", "k.covariates", "alpha",
                                                       "contrast.matrix")],
                                                       list(n.vector = crrRes$n.vector, adjust.alpha = "tukey", verbose = 0))))

    crrCnt <- do.call(power.t.contrast,
                      c(crrRes[["parms"]][c("mu.vector", "sd.vector", "p.vector", "r.squared", "k.covariates", "alpha")],
                        list(contrast.vector = crrRes$parms$contrast.matrix[2, ], power = 0.8, verbose = 0)))
    expect_equal(class(crrCnt), c("pwrss", "t", "contrast"))
    expect_equal(names(crrCnt), c("parms", "test", "psi", "d", "df", "t.alpha", "ncp", "ncp.null", "power", "n.vector", "n.total"))
    expect_equal(crrCnt[["parms"]],
                 list(mu.vector = c(0.15, 0.30, 0.20), sd.vector = rep(1, 3), p.vector = rep(1 / 3, 3), contrast.vector = mtxCnt[2, ],
                      r.squared = 0.5, k.covariates = 2, alpha = 0.05, tukey.kramer = FALSE, ceiling = TRUE, verbose = 0,
                      pretty = FALSE))
    expect_equal(crrCnt[c("test", "psi", "d", "df", "t.alpha", "ncp", "ncp.null", "power", "n.vector", "n.total")],
                 list(test = "t", psi = -0.102062073, d = -0.144337567, df = 1132, t.alpha = c(-1.96206183, 1.96206183),
                      ncp = -2.8074314, ncp.null = 0, power = 0.80096821, n.vector = rep(379, 3), n.total = 1137))

    # custom contrasts
    mtxCnt <- rbind(c(A1 = 1, A2 = -0.50, A3 = -0.50), c(A1 = 0.50, A2 = 0.50, A3 = -1))
    crrRes <- power.f.ancova.shieh(mu.vector = c(0.15, 0.30, 0.20), sd.vector = rep(1, 3), p.vector = rep(1 / 3, 3),
                                   contrast.matrix = mtxCnt, r.squared = 0.50, k.covariates = 1, alpha = 0.05,
                                   power = 0.80, verbose = 0)
    crrCnt <- power.t.contrasts(crrRes, verbose = 0)
    expect_equal(class(crrRes), c("pwrss", "f", "ancova", "shieh"))
    expect_equal(names(crrRes),
                 c("parms", "test", "effect", "eta.squared", "f", "df1", "df2", "ncp", "null.ncp", "f.alpha", "power",
                   "n.vector", "n.total"))
    expect_equal(crrRes[["parms"]],
                 list(mu.vector = c(0.15, 0.30, 0.20), sd.vector = rep(1, 3), p.vector = rep(1 / 3, 3),
                      factor.levels = NULL, r.squared = 0.5, k.covariates = 1, contrast.matrix = mtxCnt, alpha = 0.05,
                      ceiling = TRUE, verbose = 0, pretty = FALSE))
    expect_equal(crrRes[c("test", "effect", "eta.squared", "f", "df1", "df2", "ncp", "null.ncp", "f.alpha", "power", "n.vector", "n.total")],
                 list(test = "F", effect = "A(3)", eta.squared = 0.0077115847, f = 0.0881562, df1 = 2, df2 = 1241, ncp = 9.67553677,
                      null.ncp = 0, f.alpha = 3.00297552, power = 0.800762614, n.vector = rep(415, 3), n.total = 1245))
    expect_equal(class(crrCnt), c("pwrss", "t", "contrasts"))
    expect_equal(names(crrCnt), c("parms", "test", "contrast", "comparison", "psi", "d", "ncp", "df", "t.alpha", "n.total", "power"))
    expect_equal(crrCnt[["parms"]],
                 list(mu.vector = c(0.15, 0.30, 0.20), sd.vector = rep(1, 3), p.vector = rep(1 / 3, 3), r.squared = 0.5,
                      k.covariates = 1, contrast.matrix = mtxCnt, alpha = 0.05, adjust.alpha = "none", ceiling = TRUE,
                      verbose = 0, pretty = FALSE))
    expect_equal(crrCnt[c("test", "contrast", "comparison", "psi", "d", "ncp", "df", "t.alpha", "n.total", "power")],
                 list(test = "t", contrast = seq(2), comparison = c("A1 <=> A2 A3", "A1 A2 <=> A3"), psi = c(-0.1, 0.025),
                      d = c(-0.141421356, 0.035355339), ncp = c(-2.35135704, 0.58783926), df = rep(1241, 2),
                      t.alpha = rep(1.9618774, 2), n.total = rep(1245, 2), power = c(0.651581934, 0.090368815)))
    expect_equal(crrCnt, do.call(power.t.contrasts,
                                 c(crrRes[["parms"]][c("mu.vector", "sd.vector", "p.vector", "r.squared", "k.covariates", "alpha",
                                                       "contrast.matrix")], list(n.vector = crrRes$n.vector, verbose = 0))))

    crrCnt <- do.call(power.t.contrasts,
                      c(crrRes[["parms"]][c("mu.vector", "sd.vector", "p.vector", "r.squared", "k.covariates", "alpha", "contrast.matrix")],
                        list(power = crrRes$power, verbose = 0)))
    expect_equal(class(crrCnt), c("pwrss", "t", "contrasts"))
    expect_equal(names(crrCnt), c("parms", "test", "contrast", "comparison", "psi", "d", "ncp", "df", "t.alpha", "n.total", "power"))
    expect_equal(crrCnt[["parms"]],
                 list(mu.vector = c(0.15, 0.30, 0.20), sd.vector = rep(1, 3), p.vector = rep(1 / 3, 3), r.squared = 0.5,
                      k.covariates = 1, contrast.matrix = mtxCnt, alpha = 0.05, adjust.alpha = "none", ceiling = TRUE,
                      verbose = 0, pretty = FALSE))
    expect_equal(crrCnt[c("test", "contrast", "comparison", "psi", "d", "ncp", "df", "t.alpha", "n.total", "power")],
                 list(test = "t", contrast = seq(2), comparison = c("A1 <=> A2 A3", "A1 A2 <=> A3"), psi = c(-0.1, 0.025),
                      d = c(-0.141421356, 0.035355339), ncp = c(-2.806340906, 2.804411203), df = c(1769, 28310),
                      t.alpha = c(1.961305911, 1.960047784), n.total = c(1773, 28314), power = c( 0.800904361, 0.800764602)))

    crrCnt <- do.call(power.t.contrast,
                      c(crrRes[["parms"]][c("mu.vector", "sd.vector", "p.vector", "r.squared", "k.covariates", "alpha")],
                        list(contrast.vector = crrRes$parms$contrast.matrix[1, ], power = 0.8, verbose = 0)))
    expect_equal(class(crrCnt), c("pwrss", "t", "contrast"))
    expect_equal(names(crrCnt), c("parms", "test", "psi", "d", "df", "t.alpha", "ncp", "ncp.null", "power", "n.vector", "n.total"))
    expect_equal(crrCnt[["parms"]],
                 list(mu.vector = c(0.15, 0.30, 0.20), sd.vector = rep(1, 3), p.vector = rep(1 / 3, 3), contrast.vector = mtxCnt[1, ],
                      r.squared = 0.5, k.covariates = 1, alpha = 0.05, tukey.kramer = FALSE, ceiling = TRUE, verbose = 0,
                      pretty = FALSE))
    expect_equal(crrCnt[c("test", "psi", "d", "df", "t.alpha", "ncp", "ncp.null", "power", "n.vector", "n.total")],
                 list(test = "t", psi = -0.1, d = -0.141421356, df = 1766, t.alpha = c(-1.96130819, 1.96130819), ncp = -2.80396433,
                      ncp.null = 0, power = 0.8002398, n.vector = rep(590, 3), n.total = 1770))

    crrCnt <- power.t.contrast(mu.vector = c(0.20, 0.10), sd.vector = rep(1, 2), n.vector = rep(1500, 2), contrast.vector = c(1, -1),
                               r.squared = 0.50, k.covariates = 1, alpha = 0.05, tukey.kramer = TRUE, verbose = 0)
    expect_equal(class(crrCnt), c("pwrss", "t", "contrast"))
    expect_equal(names(crrCnt), c("parms", "test", "psi", "d", "df", "t.alpha", "ncp", "ncp.null", "power", "n.vector", "n.total"))
    expect_equal(crrCnt[["parms"]],
                 list(mu.vector = c(0.20, 0.10), sd.vector = rep(1, 2), p.vector = NULL, contrast.vector = c(1, -1), r.squared = 0.5,
                      k.covariates = 1, alpha = 0.05, tukey.kramer = TRUE, ceiling = TRUE, verbose = 0, pretty = FALSE))
    expect_equal(crrCnt[c("test", "psi", "d", "df", "t.alpha", "ncp", "ncp.null", "power", "n.vector", "n.total")],
                 list(test = "t", psi = 0.1, d = 0.141421356, df = 2997, t.alpha = c(-1.96075583, 1.96075583), ncp = 3.87233747,
                      ncp.null = 0, power = 0.972006164, n.vector = rep(1500, 2), n.total = 3000))

    crrOut <- capture.output(power.t.contrast(mu.vector = c(0.20, 0.10), sd.vector = rep(1, 2), n.vector = rep(1200, 2),
                                              contrast.vector = c(1, -1), r.squared = 0.50, k.covariates = 1, alpha = 0.05))
    crrDtl <- capture.output(power.t.contrast(mu.vector = c(0.20, 0.10), sd.vector = rep(1, 2), n.vector = rep(1200, 2),
                                              contrast.vector = c(1, -1), r.squared = 0.50, k.covariates = 1, alpha = 0.05, verbose = 2))
    crrPty <- capture.output(power.t.contrast(mu.vector = c(0.20, 0.10), sd.vector = rep(1, 2), n.vector = rep(1200, 2),
                                              contrast.vector = c(1, -1), r.squared = 0.50, k.covariates = 1, alpha = 0.05, pretty = TRUE))
    crrPnD <- capture.output(power.t.contrast(mu.vector = c(0.20, 0.10), sd.vector = rep(1, 2), n.vector = rep(1200, 2),
                                              contrast.vector = c(1, -1), r.squared = 0.50, k.covariates = 1, alpha = 0.05,
                                              verbose = 2, pretty = TRUE))
    expect_equal(crrOut, c("+--------------------------------------------------+",
                           "|                POWER CALCULATION                 |",
                           "+--------------------------------------------------+", "",
                           "Single Contrast Analysis (T-Test)", "",
                           "----------------------------------------------------",
                           "Hypotheses",
                           "----------------------------------------------------",
                           "  H0 (Null Claim) : psi = 0 ",
                           "  H1 (Alt. Claim) : psi != 0 ", "",
                           "----------------------------------------------------",
                           "Results",
                           "----------------------------------------------------",
                           "  Total Sample Size    = 2400",
                           "  Type 1 Error (alpha) = 0.050",
                           "  Type 2 Error (beta)  = 0.067",
                           "  Statistical Power    = 0.933  <<", ""))
    expect_equal(crrDtl, c("+--------------------------------------------------+",
                           "|                POWER CALCULATION                 |",
                           "+--------------------------------------------------+", "",
                           "Single Contrast Analysis (T-Test)", "",
                           "----------------------------------------------------",
                           "Hypotheses",
                           "----------------------------------------------------",
                           "  H0 (Null Claim) : psi = 0 ",
                           "  H1 (Alt. Claim) : psi != 0 ", "",
                           "----------------------------------------------------",
                           "Key Parameters",
                           "----------------------------------------------------",
                           "  Contrast Est. (psi)    = 0.100 ",
                           "  Standardized psi (d)   = 0.141 ",
                           "  Degrees of Freedom     = 2397 ",
                           "  Non-centrality of Alt. = 3.463 ",
                           "  Non-centrality of Null = 0.000 ",
                           "  Critical Value         = -1.961 and 1.961 ", "",
                           "----------------------------------------------------",
                           "Results",
                           "----------------------------------------------------",
                           "  Total Sample Size    = 2400",
                           "  Type 1 Error (alpha) = 0.050",
                           "  Type 2 Error (beta)  = 0.067",
                           "  Statistical Power    = 0.933  <<", "",
                           "----------------------------------------------------",
                           "Definitions",
                           "----------------------------------------------------",
                           "  psi : Contrast estimate, sum(contrast[i] * mu[i]) ",
                           "  d   : Standardized contrast estimate ", ""))
    expect_equal(crrPty, c("╔══════════════════════════════════════════════════╗",
                           "║               \033[34m POWER CALCULATION \033[0m                ║",
                           "╚══════════════════════════════════════════════════╝", "",
                           "Single Contrast Analysis (T-Test)", "",
                           "────────────────────────────────────────────────────",
                           "Hypotheses",
                           "────────────────────────────────────────────────────",
                           "  H₀ (Null)             : ψ = 0 ",
                           "  H₁ (Alternative)      : ψ ≠ 0 ", "",
                           "────────────────────────────────────────────────────",
                           "Results",
                           "────────────────────────────────────────────────────",
                           "  Total Sample Size = 2400",
                           "  Type 1 Error (α)  = 0.050",
                           "  Type 2 Error (β)  = 0.067",
                           "  \033[34mStatistical Power = 0.933\033[0m  \033[1;35m◄◄\033[0m", ""))
    expect_equal(crrPnD, c("╔══════════════════════════════════════════════════╗",
                           "║               \033[34m POWER CALCULATION \033[0m                ║",
                           "╚══════════════════════════════════════════════════╝", "",
                           "Single Contrast Analysis (T-Test)", "",
                           "────────────────────────────────────────────────────",
                           "Hypotheses",
                           "────────────────────────────────────────────────────",
                           "  H₀ (Null)             : ψ = 0 ",
                           "  H₁ (Alternative)      : ψ ≠ 0 ", "",
                           "────────────────────────────────────────────────────",
                           "Key Parameters",
                           "────────────────────────────────────────────────────",
                           "  ψ                     = 0.100",
                           "  d                     = 0.141",
                           "  df                    = 2397",
                           "  λ                     = 3.463",
                           "  λ₀                     = 0.000",
                           "  T⁻¹(α, λ₀)              = -1.961 and 1.961 ", "",
                           "────────────────────────────────────────────────────",
                           "Results",
                           "────────────────────────────────────────────────────",
                           "  Total Sample Size = 2400",
                           "  Type 1 Error (α)  = 0.050",
                           "  Type 2 Error (β)  = 0.067",
                           "  \033[34mStatistical Power = 0.933\033[0m  \033[1;35m◄◄\033[0m", "",
                           "\033[36m────────────────────────────────────────────────────\033[0m",
                           "\033[36mDefinitions\033[0m",
                           "\033[36m────────────────────────────────────────────────────\033[0m",
                           "\033[36m  ψ    : Contrast est. defined as ∑(contrastᵢ * μᵢ) ",
                           "\033[0m\033[36m  d    : Standardized contrast estimate ",
                           "\033[0m\033[36m  λ    : Non-centrality parameter under alternative ",
                           "\033[0m\033[36m  λ₀  : Non-centrality parameter under null ","", "\033[0m"))

    mtxCnt <- c(A1 = 1, A2 = -0.50, A3 = -0.50)
    expect_equal(power.f.ancova.shieh(mu.vector = c(0.15, 0.30, 0.20), sd.vector = rep(1, 3), n.vector = rep(400, 3), alpha = 0.05,
                                      contrast.matrix = mtxCnt, r.squared = 0.50, k.covariates = 1, verbose = 0)[-1], # [-1] excludes parms
                 power.f.ancova.shieh(mu.vector = c(0.15, 0.30, 0.20), sd.vector = rep(1, 3), n.vector = rep(400, 3), alpha = 0.05,
                                      contrast.matrix = t(as.matrix(mtxCnt)), r.squared = 0.50, k.covariates = 1, verbose = 0)[-1])
    expect_equal(power.t.contrast(mu.vector = c(0.20, 0), sd.vector = rep(1, 2), p.vector = rep(0.5, 2), contrast.vector = c(1, -1),
                                  r.squared = 0.50, k.covariates = 1, alpha = 0.05, power = 0.8, verbose = 0)[-1],
                 power.t.contrast(mu.vector = c(0.20, 0), sd.vector = rep(1, 2), p.vector = rep(0.5, 2),
                                  contrast.vector = matrix(c(1, -1), ncol = 2), r.squared = 0.50, k.covariates = 1,
                                  alpha = 0.05, power = 0.8, verbose = 0)[-1])                                  

    expect_error(power.f.ancova.shieh(mu.vector = NULL, r.squared = 0.50, k.covariates = 1, alpha = 0.05, verbose = 0),
                 "Provide a vector of means \\(`mu.vector`\\) with its length equal to the number of groups.")
    expect_error(power.f.ancova.shieh(mu.vector = c(0.20, 0), sd.vector = rep(1, 2), n.vector = rep(150, 2), factor.levels = 3,
                                      r.squared = 0.50, k.covariates = 1, alpha = 0.05, verbose = 0),
                 "Provide a vector of means \\(`mu.vector`\\) with its length equal to the the product of `factor.levels`.")
    expect_error(power.f.ancova.shieh(mu.vector = c(0.20, 0), sd.vector = rep(1, 2), n.vector = rep(150, 2),
                                      r.squared = -0.01, k.covariates = 1, alpha = 0.05, verbose = 0),
                 "R-squared \\(explanatory power of covariates\\) takes a value between 0 and 1.")
    expect_error(power.f.ancova.shieh(mu.vector = c(0.20, 0), sd.vector = rep(1, 2), n.vector = rep(150, 2),
                                      r.squared = 0.50, k.covariates = 1, alpha = -0.01, verbose = 0),
                 "Type 1 error rate \\(alpha\\) takes a value between 0 and 1.")
    expect_error(power.f.ancova.shieh(mu.vector = c(0.20, 0), sd.vector = rep(1, 2), n.vector = rep(150, 2), contrast.matrix = data.frame,
                                      r.squared = 0.50, k.covariates = 1, alpha = 0.05, verbose = 0),
                 "Contrast coefficients are not provided in the form of a matrix.")
    expect_error(power.f.ancova.shieh(mu.vector = c(0.20, 0), sd.vector = rep(1, 2), n.vector = rep(150, 2), contrast.matrix = 1,
                                      r.squared = 0.50, k.covariates = 1, alpha = 0.05, verbose = 0),
                 "The number of columns in the contrast matrix should match number of groups.")
    expect_error(power.f.ancova.shieh(mu.vector = c(0.20, 0), sd.vector = rep(1, 2), n.vector = rep(150, 2), contrast.matrix = diag(2),
                                      r.squared = 0.50, k.covariates = 1, alpha = 0.05, verbose = 0),
                 "The number of rows in the contrast matrix should be less than or equal to number of groups minus one.")
    expect_error(power.f.ancova.shieh(mu.vector = c(0.20, 0), sd.vector = rep(1, 2),
                                      r.squared = 0.50, k.covariates = 1, alpha = 0.05, power = 0.8, verbose = 0),
                 "`p.vector` cannot be NULL when sample size is requested.")
    expect_error(power.f.ancova.shieh(mu.vector = c(0.20, 0), sd.vector = rep(1, 2), p.vector = rep(0.5 - 1e-5, 2),
                                      r.squared = 0.50, k.covariates = 1, alpha = 0.05, power = 0.8, verbose = 0),
                 "The elements of the `p.vector` should sum to 1.")
    expect_error(power.f.ancova.shieh(mu.vector = runif(16, 0, 1), sd.vector = rep(1, 16), p.vector = rep(1 / 16, 16),
                                      factor.levels = c(2, 2, 2, 2), contrast.matrix = matrix(runif(240), ncol = 16),
                                      r.squared = 0.50, k.covariates = 1, alpha = 0.05, power = 0.8, verbose = 0),
                 "More than three-way ANCOVA is not allowed at the moment.")
    expect_error(power.t.contrast(mu.vector = c(0.20, 0), sd.vector = rep(1, 2), p.vector = rep(0.5, 2), contrast.vector = c(1, -1),
                                  r.squared = -0.01, k.covariates = 1, alpha = 0.05, power = 0.8, verbose = 0),
                 "R-squared \\(explanatory power of covariates\\) takes a value between 0 and 1.")
    expect_error(power.t.contrast(mu.vector = c(0.20, 0), sd.vector = rep(1, 2), p.vector = rep(0.5, 2), contrast.vector = data.frame,
                                  r.squared = 0.50, k.covariates = 1, alpha = 0.05, power = 0.8, verbose = 0),
                 "`contrast.vector` must be either a vector or a matrix.")
    expect_error(power.t.contrast(mu.vector = c(0.20, 0), sd.vector = rep(1, 2), p.vector = rep(0.5, 2), contrast.vector = c(0.5, 0.5, 1),
                                  r.squared = 0.50, k.covariates = 1, alpha = 0.05, power = 0.8, verbose = 0),
                 "The number of columns in the contrast matrix should match number of groups.")
    expect_error(power.t.contrast(mu.vector = c(0.20, 0), sd.vector = rep(1, 2), p.vector = rep(0.5, 2), contrast.vector = diag(2),
                                  r.squared = 0.50, k.covariates = 1, alpha = 0.05, power = 0.8, verbose = 0),
                 "The number of rows in the contrast matrix should be one.")
    expect_error(power.t.contrast(mu.vector = c(0.20, 0), sd.vector = rep(1, 2), p.vector = rep(0.5, 2), contrast.vector = c(1, -1),
                                  k.covariates = 0, alpha = 0.05, power = 0.8, verbose = 0),
                 "The number of covariates should be 1 or greater in the analysis of covariance.")
    expect_warning(power.t.contrast(mu.vector = c(0.20, 0.20), sd.vector = rep(1, 2), p.vector = rep(0.5, 2), contrast.vector = c(1, -1),
                                    r.squared = 0.5, k.covariates = 1, alpha = 0.05, power = 0.8, verbose = 0),
                   "Using infinity \\(maximum integer number as defined in R\\) for `n.total` because `psi` = 0.")
    expect_error(power.t.contrast(mu.vector = c(0.20, 0), sd.vector = rep(1, 2), contrast.vector = c(1, -1),
                                  r.squared = 0.5, k.covariates = 1, alpha = 0.05, power = 0.8, verbose = 0),
                   "The `p.vector` cannot be NULL when the sample size is requested.")
    expect_error(power.t.contrast(mu.vector = c(0.20, 0), sd.vector = rep(1, 2), p.vector = rep(0.5 - 1e-5, 2), contrast.vector = c(1, -1),
                                  r.squared = 0.5, k.covariates = 1, alpha = 0.05, power = 0.8, verbose = 0),
                   "The elements of the `p.vector` should sum to 1.")
    expect_error(power.t.contrasts(data.frame),
                 "This function only works with an object of type `pwrss`, `ancova`, and `shieh`.")
})
