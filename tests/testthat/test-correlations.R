test_that("correlations.R works", {
    # power.z.steiger (= power.z.twocors.steiger [the latter is exported]) ---------------------------------------------
    crrRes <- power.z.twocors.steiger(rho12 = 0.35, rho13 = 0.45, rho23 = 0.05, power = 0.8, alpha = 0.05,
                                      alternative = "two.sided", common.index = TRUE, verbose = 0)
    expect_equal(class(crrRes), c("pwrss", "z", "twocors", "paired"))
    expect_equal(names(crrRes), c("parms", "test", "design", "delta", "q", "mean", "sd", "null.mean", "null.sd", "alternative",
                                  "z.alpha", "n", "power"))
    expect_equal(crrRes[["parms"]], list(rho12 = 0.35, rho13 = 0.45, rho23 = 0.05, rho14 = NULL, rho24 = NULL, rho34 = NULL, alpha = 0.05,
                                         alternative = "two.sided", pooled = TRUE, common.index = TRUE, ceiling = TRUE, verbose = 0, pretty = FALSE))
    expect_equal(crrRes[c("test", "design", "delta", "q", "mean", "sd", "null.mean", "null.sd", "alternative", "z.alpha", "n", "power")],
                 list(test = "z", design = "paired", delta = -0.1, q = -0.119256524, mean = -2.8011469, sd = 0.999426958, null.mean = 0,
                      null.sd = 1, alternative = "two.sided", z.alpha = c(-1.959964, 1.959964), n = 1138, power = 0.80001327))

    crrRes <- power.z.twocors.steiger(rho12 = 0.35, rho13 = 0.45, rho23 = 0.05, n = 1000, alpha = 0.05,
                                      alternative = "two.sided", common.index = TRUE, verbose = 0)
    expect_equal(class(crrRes), c("pwrss", "z", "twocors", "paired"))
    expect_equal(names(crrRes), c("parms", "test", "design", "delta", "q", "mean", "sd", "null.mean", "null.sd", "alternative",
                                  "z.alpha", "n", "power"))
    expect_equal(crrRes[["parms"]], list(rho12 = 0.35, rho13 = 0.45, rho23 = 0.05, rho14 = NULL, rho24 = NULL, rho34 = NULL, alpha = 0.05,
                                         alternative = "two.sided", pooled = TRUE, common.index = TRUE, ceiling = TRUE, verbose = 0, pretty = FALSE))
    expect_equal(crrRes[c("test", "design", "delta", "q", "mean", "sd", "null.mean", "null.sd", "alternative", "z.alpha", "n", "power")],
                 list(test = "z", design = "paired", delta = -0.1, q = -0.119256524, mean = -2.62533987, sd = 0.999426958, null.mean = 0,
                      null.sd = 1, alternative = "two.sided", z.alpha = c(-1.959964, 1.959964), n = 1000, power = 0.747219145))

    crrAsc <- capture.output(power.z.twocors.steiger(rho12 = 0.35, rho13 = 0.45, rho23 = 0.05, n = 1000, power = NULL,
                                                     alpha = 0.05, alternative = "two.sided", common.index = TRUE, verbose = 2))
    expect_equal(crrAsc, c("+--------------------------------------------------+",
                           "|                POWER CALCULATION                 |",
                           "+--------------------------------------------------+", "",
                           "Dependent Correlations", "",
                           "  Common Index    : TRUE", "",
                           "----------------------------------------------------",
                           "Hypotheses",
                           "----------------------------------------------------",
                           "  H0 (Null Claim) : rho12 - rho13  = 0",
                           "  H1 (Alt. Claim) : rho12 - rho13 != 0", "",
                           "----------------------------------------------------",
                           "Key Parameters",
                           "----------------------------------------------------",
                           "  rho12 - rho13        = -0.100",
                           "  Cohen's q            = -0.119",
                           "  Mean of Alt.         = -2.625",
                           "  SD of Alt.           = 0.999",
                           "  Mean of Null         = 0.000",
                           "  SD of Null           = 1.000",
                           "  Critical Value       = -1.96 and 1.96", "",
                           "----------------------------------------------------",
                           "Results",
                           "----------------------------------------------------",
                           "  Sample Size          = 1000",
                           "  Type 1 Error (alpha) = 0.050",
                           "  Type 2 Error (beta)  = 0.253",
                           "  Statistical Power    = 0.747  <<", "",
                           "----------------------------------------------------",
                           "Definitions",
                           "----------------------------------------------------",
                           "  rho12 : Correlation between variable V1 and V2 ",
                           "  rho13 : Correlation between variable V1 and V3 ", ""))

    crrPty <- capture.output(power.z.twocors.steiger(rho12 = 0.35, rho13 = 0.45, rho23 = 0.05, n = 1000, power = NULL,
                                                     alpha = 0.05, alternative = "two.sided", common.index = TRUE,
                                                     verbose = 2, pretty = TRUE))
    expect_equal(crrPty, c("╔══════════════════════════════════════════════════╗",
                           "║               \033[34m POWER CALCULATION \033[0m                ║",
                           "╚══════════════════════════════════════════════════╝", "",
                           "Dependent Correlations", "",
                           "  Common Index        : TRUE", "",
                           "────────────────────────────────────────────────────",
                           "Hypotheses",
                           "────────────────────────────────────────────────────",
                           "  H₀ (Null)         : ρ₁₂ - ρ₁₃ = 0 ",
                           "  H₁ (Alternative)  : ρ₁₂ - ρ₁₃ ≠ 0 ", "",
                           "────────────────────────────────────────────────────",
                           "Key Parameters",
                           "────────────────────────────────────────────────────",
                           "  ρ₁₂ - ρ₁₃           = -0.100 ",
                           "  Cohen's q         = -0.119 ",
                           "  μ (Alternative)   = -2.625",
                           "  σ (Alternative)   = 0.999",
                           "  μ₀ (Null)          = 0.000 ",
                           "  σ₀ (Null)          = 1.000 ",
                           "  Z⁻¹(α, μ₀, σ₀)     = -1.96 and 1.96 ", "",
                           "────────────────────────────────────────────────────",
                           "Results",
                           "────────────────────────────────────────────────────",
                           "  Sample Size        = 1000",
                           "  Type 1 Error (α)   = 0.050",
                           "  Type 2 Error (β)   = 0.253",
                           "  \033[34mStatistical Power  = 0.747\033[0m  \033[1;35m◄◄\033[0m", "",
                           "\033[36m────────────────────────────────────────────────────\033[0m",
                           "\033[36mDefinitions\033[0m",
                           "\033[36m────────────────────────────────────────────────────\033[0m",
                           "\033[36m  ρ₁₂ : Correlation between variable V1 and V2 ",
                           "\033[0m\033[36m  ρ₁₃ : Correlation between variable V1 and V3 ",
                           "\033[0m\033[36m  μ   : Mean ",
                           "\033[0m\033[36m  σ   : Standard deviation ", "", "\033[0m"))

    crrRes <- power.z.twocors.steiger(rho12 = 0.45, rho13 = 0.45, rho23 = 0.50, rho14 = 0.50, rho24 = 0.80, rho34 = 0.55, power = 0.8,
                                      alpha = 0.05, alternative = "two.sided", common.index = FALSE, verbose = 0)
    expect_equal(class(crrRes), c("pwrss", "z", "twocors", "paired"))
    expect_equal(names(crrRes), c("parms", "test", "design", "delta", "q", "mean", "sd", "null.mean", "null.sd", "alternative",
                                  "z.alpha", "n", "power"))
    expect_equal(crrRes[["parms"]], list(rho12 = 0.45, rho13 = 0.45, rho23 = 0.50, rho14 = 0.50, rho24 = 0.80, rho34 = 0.55,
                                         alpha = 0.05, alternative = "two.sided", pooled = TRUE, common.index = FALSE,
                                         ceiling = TRUE, verbose = 0, pretty = FALSE))
    expect_equal(crrRes[c("test", "design", "delta", "q", "mean", "sd", "null.mean", "null.sd", "alternative", "z.alpha", "n", "power")],
                 list(test = "z", design = "paired", delta = -0.1, q = -0.133681035, mean = -2.80207461, sd = 0.99997767, null.mean = 0,
                      null.sd = 1, alternative = "two.sided", z.alpha = c(-1.959964, 1.959964), n = 643, power = 0.8001432))

    crrAsc <- capture.output(power.z.twocors.steiger(rho12 = 0.45, rho13 = 0.45, rho23 = 0.50, rho14 = 0.50, rho24 = 0.80, rho34 = 0.55,
                                                     power = 0.8, alpha = 0.05, alternative = "two.sided", common.index = FALSE))
    expect_equal(crrAsc[c(2)], c("|             SAMPLE SIZE CALCULATION              |"))

    crrRes <- power.z.twocors.steiger(rho12 = 0.45, rho13 = 0.45, rho23 = 0.50, rho14 = 0.50, rho24 = 0.80, rho34 = 0.55, n = 643,
                                      alpha = 0.05, alternative = "two.sided", common.index = FALSE, verbose = 0)
    expect_equal(class(crrRes), c("pwrss", "z", "twocors", "paired"))
    expect_equal(names(crrRes), c("parms", "test", "design", "delta", "q", "mean", "sd", "null.mean", "null.sd", "alternative",
                                  "z.alpha", "n", "power"))
    expect_equal(crrRes[["parms"]], list(rho12 = 0.45, rho13 = 0.45, rho23 = 0.50, rho14 = 0.50, rho24 = 0.80, rho34 = 0.55,
                                         alpha = 0.05, alternative = "two.sided", pooled = TRUE, common.index = FALSE,
                                         ceiling = TRUE, verbose = 0, pretty = FALSE))
    expect_equal(crrRes[c("test", "design", "delta", "q", "mean", "sd", "null.mean", "null.sd", "alternative", "z.alpha", "n", "power")],
                 list(test = "z", design = "paired", delta = -0.1, q = -0.133681035, mean = -2.80207461, sd = 0.99997767, null.mean = 0,
                      null.sd = 1, alternative = "two.sided", z.alpha = c(-1.959964, 1.959964), n = 643, power = 0.8001432))

    crrRes <- power.z.twocors.steiger(rho12 = 0.45, rho13 = 0.45, rho23 = 0.50, rho14 = 0.50, rho24 = 0.80, rho34 = 0.55, power = 0.8,
                                      alpha = 0.05, alternative = "two.sided", pooled = FALSE, common.index = FALSE, verbose = 0)
    expect_equal(class(crrRes), c("pwrss", "z", "twocors", "paired"))
    expect_equal(names(crrRes), c("parms", "test", "design", "delta", "q", "mean", "sd", "null.mean", "null.sd", "alternative",
                                  "z.alpha", "n", "power"))
    expect_equal(crrRes[["parms"]], list(rho12 = 0.45, rho13 = 0.45, rho23 = 0.50, rho14 = 0.50, rho24 = 0.80, rho34 = 0.55,
                                         alpha = 0.05, alternative = "two.sided", pooled = FALSE, common.index = FALSE,
                                         ceiling = TRUE, verbose = 0, pretty = FALSE))
    expect_equal(crrRes[c("test", "design", "delta", "q", "mean", "sd", "null.mean", "null.sd", "alternative", "z.alpha", "n", "power")],
                 list(test = "z", design = "paired", delta = -0.1, q = -0.133681035, mean = -2.81260658, sd = 1.011671015, null.mean = 0,
                      null.sd = 1, alternative = "two.sided", z.alpha = c(-1.959964, 1.959964), n = 633, power = 0.800332771))

    crrRes <- power.z.twocors.steiger(rho12 = 0.45, rho13 = 0.45, rho23 = 0.50, rho14 = 0.50, rho24 = 0.80, rho34 = 0.55, n = 633,
                                      alpha = 0.05, alternative = "two.sided", pooled = FALSE, common.index = FALSE, verbose = 0)
    expect_equal(class(crrRes), c("pwrss", "z", "twocors", "paired"))
    expect_equal(names(crrRes), c("parms", "test", "design", "delta", "q", "mean", "sd", "null.mean", "null.sd", "alternative",
                                  "z.alpha", "n", "power"))
    expect_equal(crrRes[["parms"]], list(rho12 = 0.45, rho13 = 0.45, rho23 = 0.50, rho14 = 0.50, rho24 = 0.80, rho34 = 0.55,
                                         alpha = 0.05, alternative = "two.sided", pooled = FALSE, common.index = FALSE,
                                         ceiling = TRUE, verbose = 0, pretty = FALSE))
    expect_equal(crrRes[c("test", "design", "delta", "q", "mean", "sd", "null.mean", "null.sd", "alternative", "z.alpha", "n", "power")],
                 list(test = "z", design = "paired", delta = -0.1, q = -0.133681035, mean = -2.81260658, sd = 1.011671015, null.mean = 0,
                      null.sd = 1, alternative = "two.sided", z.alpha = c(-1.959964, 1.959964), n = 633, power = 0.800332771))

    crrRes <- power.z.twocors.steiger(rho12 = 0.35, rho13 = 0.45, rho23 = 0.05, power = 0.8, alpha = 0.05,
                                      alternative = "two.sided", pooled = FALSE, common.index = TRUE, verbose = 0)
    expect_equal(class(crrRes), c("pwrss", "z", "twocors", "paired"))
    expect_equal(names(crrRes), c("parms", "test", "design", "delta", "q", "mean", "sd", "null.mean", "null.sd", "alternative",
                                  "z.alpha", "n", "power"))
    expect_equal(crrRes[["parms"]], list(rho12 = 0.35, rho13 = 0.45, rho23 = 0.05, rho14 = NULL, rho24 = NULL, rho34 = NULL, alpha = 0.05,
                                         alternative = "two.sided", pooled = FALSE, common.index = TRUE, ceiling = TRUE, verbose = 0, pretty = FALSE))
    expect_equal(crrRes[c("test", "design", "delta", "q", "mean", "sd", "null.mean", "null.sd", "alternative", "z.alpha", "n", "power")],
                 list(test = "z", design = "paired", delta = -0.1, q = -0.119256524, mean = -2.80946813, sd = 1.00818629, null.mean = 0,
                      null.sd = 1, alternative = "two.sided", z.alpha = c(-1.959964, 1.959964), n = 1125, power = 0.8002768))

    crrRes <- power.z.twocors.steiger(rho12 = 0.35, rho13 = 0.45, rho23 = 0.05, n = 1125, alpha = 0.05,
                                      alternative = "two.sided", pooled = FALSE, common.index = TRUE, verbose = 0)
    expect_equal(class(crrRes), c("pwrss", "z", "twocors", "paired"))
    expect_equal(names(crrRes), c("parms", "test", "design", "delta", "q", "mean", "sd", "null.mean", "null.sd", "alternative",
                                  "z.alpha", "n", "power"))
    expect_equal(crrRes[["parms"]], list(rho12 = 0.35, rho13 = 0.45, rho23 = 0.05, rho14 = NULL, rho24 = NULL, rho34 = NULL, alpha = 0.05,
                                         alternative = "two.sided", pooled = FALSE, common.index = TRUE, ceiling = TRUE, verbose = 0, pretty = FALSE))
    expect_equal(crrRes[c("test", "design", "delta", "q", "mean", "sd", "null.mean", "null.sd", "alternative", "z.alpha", "n", "power")],
                 list(test = "z", design = "paired", delta = -0.1, q = -0.119256524, mean = -2.80946813, sd = 1.00818629, null.mean = 0,
                      null.sd = 1, alternative = "two.sided", z.alpha = c(-1.959964, 1.959964), n = 1125, power = 0.8002768))

    expect_error(power.z.twocors.steiger(rho12 = 2e-4, rho13 = 1e-4, rho23 = 0.01, power = 1 - 1e-8, alpha = 1e-8,
                                         alternative = "two.sided", common.index = TRUE),
                 "Design is not feasible.")
    expect_error(power.z.twocors.steiger(rho12 = 0.01, rho13 = 0.01, rho23 = 0.02, power = 0.8, alpha = 0.05,
                                         alternative = "two.sided", common.index = TRUE),
                 "`common.index` is TRUE and `alternative` is \"two.sided\" but `rho12` = `rho13`.")
    expect_error(power.z.twocors.steiger(rho12 = 0.45, rho13 = 0.45, rho23 = 0.50, rho14 = 0.50, rho24 = 0.80, rho34 = 0.45, power = 0.8,
                                         alpha = 0.05, alternative = "two.sided", common.index = FALSE, verbose = 0),
                 "`common.index` is FALSE and `alternative` = \"two.sided\" but `rho12` = `rho34`.")

    expect_warning(power.z.twocors.steiger(rho12 = 0.1, rho13 = 0.2, rho23 = 0.3, rho14 = 0.4, power = 0.8, alpha = 0.05,
                                         alternative = "two.sided", common.index = TRUE, verbose = 0),
                   "Ignoring `rho14` `rho24`, or `rho34` because `common.index` is TRUE.")

    # power.z.twocors (= pwrss.z.2cors) --------------------------------------------------------------------------------
    crrRes <- power.z.twocors(rho1 = 0.20, rho2 = 0.30, power = 0.80, alpha = 0.05, alternative = "two.sided", verbose = 0)
    expect_equal(class(crrRes), c("pwrss", "z", "twocors", "independent"))
    expect_equal(names(crrRes), c("parms", "test", "design", "delta", "q", "mean", "sd", "null.mean", "null.sd", "alternative",
                                  "z.alpha", "n", "power"))
    expect_equal(crrRes[["parms"]], list(rho1 = 0.20, rho2 = 0.30, n.ratio = 1, alpha = 0.05, alternative = "two.sided",
                                         ceiling = TRUE, verbose = 0, pretty = FALSE))
    expect_equal(crrRes[c("test", "design", "delta", "q", "mean", "sd", "null.mean", "null.sd", "alternative", "z.alpha", "n", "power")],
                 list(test = "z", design = "independent", delta = -0.1, q = -0.10678705, mean = -2.80201569, sd = 1,
                      null.mean = 0, null.sd = 1, alternative = "two.sided", z.alpha = c(-1.959964, 1.959964),
                      n = c(n1 = 1380, n2 = 1380), power = 0.800121451))
    expect_equal(crrRes, pwrss.z.2corrs(r1 = 0.20, r2 = 0.30, power = 0.80, alpha = 0.05, alternative = "not equal", verbose = FALSE))

    crrAsc <- capture.output(power.z.twocors(rho1 = 0.20, rho2 = 0.30, power = 0.80, alpha = 0.05, alternative = "two.sided", verbose = 2))
    expect_equal(crrAsc[c(5, 10, 11, 16:22, 35:36)],
                 c("Independent Correlations ",
                   "  H0 (Null Claim) : rho1 - rho2  = 0",
                   "  H1 (Alt. Claim) : rho1 - rho2 != 0",
                   "  rho1 - rho2          = -0.100",
                   "  Cohen's q            = -0.107",
                   "  Mean of Alt.         = -2.802",
                   "  SD of Alt.           = 1.000",
                   "  Mean of Null         = 0.000",
                   "  SD of Null           = 1.000",
                   "  Critical Value       = -1.96 and 1.96",
                   "  rho1 : Correlation in group 1 ",
                   "  rho2 : Correlation in group 2 "))
    expect_equal(length(crrAsc), 37)

    crrPty <- capture.output(power.z.twocors(rho1 = 0.20, rho2 = 0.30, power = 0.80, alpha = 0.05, alternative = "two.sided",
                                             verbose = 2, pretty = TRUE))
    expect_equal(crrPty[c(5, 10, 11, 16:22, 35:38)],
                 c("Independent Correlations",
                   "  H₀ (Null)         : ρ₁ - ρ₂ = 0 ",
                   "  H₁ (Alternative)  : ρ₁ - ρ₂ ≠ 0 ",
                   "  ρ₁ - ρ₂             = -0.100 ",
                   "  Cohen's q         = -0.107 ",
                   "  μ (Alternative)   = -2.802",
                   "  σ (Alternative)   = 1.000",
                   "  μ₀ (Null)          = 0.000 ",
                   "  σ₀ (Null)          = 1.000 ",
                   "  Z⁻¹(α, μ₀, σ₀)     = -1.96 and 1.96 ",
                   "\033[36m  ρ₁ : Correlation (for some V1 ~ V2) in the first group ",
                   "\033[0m\033[36m  ρ₂ : Correlation (for some V1 ~ V2) in the second group ",
                   "\033[0m\033[36m  μ   : Mean ",
                   "\033[0m\033[36m  σ   : Standard deviation "))
    expect_equal(length(crrPty), 40)

    crrRes <- power.z.twocors(rho1 = 0.20, rho2 = 0.30, n2 = 1380, alpha = 0.05, alternative = "two.sided", verbose = 0)
    expect_equal(class(crrRes), c("pwrss", "z", "twocors", "independent"))
    expect_equal(names(crrRes), c("parms", "test", "design", "delta", "q", "mean", "sd", "null.mean", "null.sd", "alternative",
                                  "z.alpha", "n", "power"))
    expect_equal(crrRes[["parms"]], list(rho1 = 0.20, rho2 = 0.30, n.ratio = 1, alpha = 0.05, alternative = "two.sided",
                                         ceiling = TRUE, verbose = 0, pretty = FALSE))
    expect_equal(crrRes[c("test", "design", "delta", "q", "mean", "sd", "null.mean", "null.sd", "alternative", "z.alpha", "n", "power")],
                 list(test = "z", design = "independent", delta = -0.1, q = -0.10678705, mean = -2.80201569, sd = 1,
                      null.mean = 0, null.sd = 1, alternative = "two.sided", z.alpha = c(-1.959964, 1.959964),
                      n = c(n1 = 1380, n2 = 1380), power = 0.800121451))
    expect_equal(crrRes, pwrss.z.2corrs(r1 = 0.20, r2 = 0.30, n2 = 1380, alpha = 0.05, alternative = "not equal", verbose = FALSE))

    crrRes <- power.z.twocors(rho1 = 0.30, rho2 = 0.20, power = 0.80, alpha = 0.05, alternative = "one.sided", verbose = 0)
    expect_equal(class(crrRes), c("pwrss", "z", "twocors", "independent"))
    expect_equal(names(crrRes), c("parms", "test", "design", "delta", "q", "mean", "sd", "null.mean", "null.sd", "alternative",
                                  "z.alpha", "n", "power"))
    expect_equal(crrRes[["parms"]], list(rho1 = 0.30, rho2 = 0.20, n.ratio = 1, alpha = 0.05, alternative = "one.sided",
                                         ceiling = TRUE, verbose = 0, pretty = FALSE))
    expect_equal(crrRes[c("test", "design", "delta", "q", "mean", "sd", "null.mean", "null.sd", "alternative", "z.alpha", "n", "power")],
                 list(test = "z", design = "independent", delta = 0.1, q = 0.10678705, mean = 2.4872444, sd = 1,
                      null.mean = 0, null.sd = 1, alternative = "one.sided", z.alpha = 1.64485363,
                      n = c(n1 = 1088, n2 = 1088), power = 0.80021537))
    expect_equal(crrRes, pwrss.z.2corrs(r1 = 0.30, r2 = 0.20, power = 0.80, alpha = 0.05, alternative = "less", verbose = FALSE))

    crrRes <- power.z.twocors(rho1 = 0.30, rho2 = 0.20, n2 = 1088, alpha = 0.05, alternative = "one.sided", verbose = 0)
    expect_equal(class(crrRes), c("pwrss", "z", "twocors", "independent"))
    expect_equal(names(crrRes), c("parms", "test", "design", "delta", "q", "mean", "sd", "null.mean", "null.sd", "alternative",
                                  "z.alpha", "n", "power"))
    expect_equal(crrRes[["parms"]], list(rho1 = 0.30, rho2 = 0.20, n.ratio = 1, alpha = 0.05, alternative = "one.sided",
                                         ceiling = TRUE, verbose = 0, pretty = FALSE))
    expect_equal(crrRes[c("test", "design", "delta", "q", "mean", "sd", "null.mean", "null.sd", "alternative", "z.alpha", "n", "power")],
                 list(test = "z", design = "independent", delta = 0.1, q = 0.10678705, mean = 2.4872444, sd = 1,
                      null.mean = 0, null.sd = 1, alternative = "one.sided", z.alpha = 1.64485363,
                      n = c(n1 = 1088, n2 = 1088), power = 0.80021537))

    crrRes <- power.z.twocors(rho1 = 0.30, rho2 = 0.20, n.ratio = 2, power = 0.80, alpha = 0.05, alternative = "two.sided", verbose = 0)
    expect_equal(class(crrRes), c("pwrss", "z", "twocors", "independent"))
    expect_equal(names(crrRes), c("parms", "test", "design", "delta", "q", "mean", "sd", "null.mean", "null.sd", "alternative",
                                  "z.alpha", "n", "power"))
    expect_equal(crrRes[["parms"]], list(rho1 = 0.30, rho2 = 0.20, n.ratio = 2, alpha = 0.05, alternative = "two.sided",
                                         ceiling = TRUE, verbose = 0, pretty = FALSE))
    expect_equal(crrRes[c("test", "design", "delta", "q", "mean", "sd", "null.mean", "null.sd", "alternative", "z.alpha", "n", "power")],
                 list(test = "z", design = "independent", delta = 0.1, q = 0.10678705, mean = 2.80167586, sd = 1,
                      null.mean = 0, null.sd = 1, alternative = "two.sided", z.alpha = c(-1.959964, 1.959964),
                      n = c(n1 = 2070, n2 = 1035), power = 0.800026336))

    crrRes <- power.z.twocors(rho1 = 0.30, rho2 = 0.20, n.ratio = 2, n2 = 1035, alpha = 0.05, alternative = "two.sided", verbose = 0)
    expect_equal(class(crrRes), c("pwrss", "z", "twocors", "independent"))
    expect_equal(names(crrRes), c("parms", "test", "design", "delta", "q", "mean", "sd", "null.mean", "null.sd", "alternative",
                                  "z.alpha", "n", "power"))
    expect_equal(crrRes[["parms"]], list(rho1 = 0.30, rho2 = 0.20, n.ratio = 2, alpha = 0.05, alternative = "two.sided",
                                         ceiling = TRUE, verbose = 0, pretty = FALSE))
    expect_equal(crrRes[c("test", "design", "delta", "q", "mean", "sd", "null.mean", "null.sd", "alternative", "z.alpha", "n", "power")],
                 list(test = "z", design = "independent", delta = 0.1, q = 0.10678705, mean = 2.80167586, sd = 1,
                      null.mean = 0, null.sd = 1, alternative = "two.sided", z.alpha = c(-1.959964, 1.959964),
                      n = c(n1 = 2070, n2 = 1035), power = 0.800026336))

    crrRes <- power.z.twocors(rho1 = 0.20, rho2 = 0.30, n.ratio = 1 / 2, power = 0.80, alpha = 0.05, alternative = "one.sided", verbose = 0)
    expect_equal(class(crrRes), c("pwrss", "z", "twocors", "independent"))
    expect_equal(names(crrRes), c("parms", "test", "design", "delta", "q", "mean", "sd", "null.mean", "null.sd", "alternative",
                                  "z.alpha", "n", "power"))
    expect_equal(crrRes[["parms"]], list(rho1 = 0.20, rho2 = 0.30, n.ratio = 1 / 2, alpha = 0.05, alternative = "one.sided",
                                         ceiling = TRUE, verbose = 0, pretty = FALSE))
    expect_equal(crrRes[c("test", "design", "delta", "q", "mean", "sd", "null.mean", "null.sd", "alternative", "z.alpha", "n", "power")],
                 list(test = "z", design = "independent", delta = -0.1, q = -0.10678705, mean = -2.4868614, sd = 1,
                      null.mean = 0, null.sd = 1, alternative = "one.sided", z.alpha = -1.64485363,
                      n = c(n1 = 816, n2 = 1632), power = 0.8001082))

    crrRes <- power.z.twocors(rho1 = 0.20, rho2 = 0.30, n.ratio = 1 / 2, n2 = 1632, alpha = 0.05, alternative = "one.sided", verbose = 0)
    expect_equal(class(crrRes), c("pwrss", "z", "twocors", "independent"))
    expect_equal(names(crrRes), c("parms", "test", "design", "delta", "q", "mean", "sd", "null.mean", "null.sd", "alternative",
                                  "z.alpha", "n", "power"))
    expect_equal(crrRes[["parms"]], list(rho1 = 0.20, rho2 = 0.30, n.ratio = 1 / 2, alpha = 0.05, alternative = "one.sided",
                                         ceiling = TRUE, verbose = 0, pretty = FALSE))
    expect_equal(crrRes[c("test", "design", "delta", "q", "mean", "sd", "null.mean", "null.sd", "alternative", "z.alpha", "n", "power")],
                 list(test = "z", design = "independent", delta = -0.1, q = -0.10678705, mean = -2.4868614, sd = 1,
                      null.mean = 0, null.sd = 1, alternative = "one.sided", z.alpha = -1.64485363,
                      n = c(n1 = 816, n2 = 1632), power = 0.8001082))

    # power.z.onecor (= pwrss.z.cor) -----------------------------------------------------------------------------------
    crrRes <- power.z.onecor(rho = 0.20, power = 0.80, alpha = 0.05, alternative = "two.sided", verbose = 0)
    expect_equal(class(crrRes), c("pwrss", "z", "onecor"))
    expect_equal(names(crrRes), c("parms", "test", "design", "delta", "q", "mean", "sd", "null.mean", "null.sd", "alternative",
                                  "z.alpha", "n", "power"))
    expect_equal(crrRes[["parms"]], list(rho = 0.20, null.rho = 0, alpha = 0.05, alternative = "two.sided", ceiling = TRUE,
                                         verbose = 0, pretty = FALSE))
    expect_equal(crrRes[c("test", "design", "delta", "q", "mean", "sd", "null.mean", "null.sd", "alternative", "z.alpha", "n", "power")],
                 list(test = "z", design = "one.sample", delta = 0.2, q = 0.202732554, mean = 2.80181964, sd = 1, null.mean = 0, null.sd = 1,
                      alternative = "two.sided", z.alpha = c(-1.959964, 1.959964), n = 194, power = 0.80006658))
    expect_equal(crrRes, pwrss.z.corr(r = 0.20, power = 0.80, alpha = 0.05, alternative = "not equal", verbose = FALSE))

    crrAsc <- capture.output(power.z.onecor(rho = 0.20, power = 0.80, alpha = 0.05, alternative = "two.sided", verbose = 2))
    expect_equal(crrAsc[c(5, 10:11, 16:22, 35:36)],
                 c("One-Sample Correlation ",
                   "  H0 (Null Claim) : rho - null.rho  = 0",
                   "  H1 (Alt. Claim) : rho - null.rho != 0",
                   "  rho - null.rho       = 0.200",
                   "  Cohen's q            = 0.203",
                   "  Mean of Alt.         = 2.802",
                   "  SD of Alt.           = 1.000",
                   "  Mean of Null         = 0.000",
                   "  SD of Null           = 1.000",
                   "  Critical Value       = -1.96 and 1.96",
                   "  rho      : Correlation under alt. ",
                   "  null.rho : Correlation under null "))
    expect_equal(length(crrAsc), 37)

    crrPty <- capture.output(power.z.onecor(rho = 0.20, power = 0.80, alpha = 0.05, alternative = "two.sided", verbose = 2, pretty = TRUE))
    expect_equal(crrPty[c(5, 10:11, 16:22, 35:38)],
                 c("One-Sample Correlation",
                   "  H₀ (Null)         : ρ₁ - ρ₀ = 0 ",
                   "  H₁ (Alternative)  : ρ₁ - ρ₀ ≠ 0 ",
                   "  ρ₁ - ρ₀             = 0.200 ",
                   "  Cohen's q         = 0.203 ",
                   "  μ (Alternative)   = 2.802",
                   "  σ (Alternative)   = 1.000",
                   "  μ₀ (Null)          = 0.000 ",
                   "  σ₀ (Null)          = 1.000 ",
                   "  Z⁻¹(α, μ₀, σ₀)     = -1.96 and 1.96 ",
                   "\033[36m  ρ₁ : Correlation (for some V1 ~ V2) under alternative ",
                   "\033[0m\033[36m  ρ₀ : Correlation (for some V1 ~ V2) under null ",
                   "\033[0m\033[36m  μ   : Mean ",
                   "\033[0m\033[36m  σ   : Standard deviation "))
    expect_equal(length(crrPty), 40)

    crrRes <- power.z.onecor(rho = 0.20, n = 194, alpha = 0.05, alternative = "two.sided", verbose = 0)
    expect_equal(class(crrRes), c("pwrss", "z", "onecor"))
    expect_equal(names(crrRes), c("parms", "test", "design", "delta", "q", "mean", "sd", "null.mean", "null.sd", "alternative",
                                  "z.alpha", "n", "power"))
    expect_equal(crrRes[["parms"]], list(rho = 0.20, null.rho = 0, alpha = 0.05, alternative = "two.sided", ceiling = TRUE,
                                         verbose = 0, pretty = FALSE))
    expect_equal(crrRes[c("test", "design", "delta", "q", "mean", "sd", "null.mean", "null.sd", "alternative", "z.alpha", "n", "power")],
                 list(test = "z", design = "one.sample", delta = 0.2, q = 0.202732554, mean = 2.80181964, sd = 1, null.mean = 0, null.sd = 1,
                      alternative = "two.sided", z.alpha = c(-1.959964, 1.959964), n = 194, power = 0.80006658))

    crrRes <- power.z.onecor(rho = 0.20, null = 0.10, power = 0.80, alpha = 0.05, alternative = "one.sided", verbose = 0)
    expect_equal(class(crrRes), c("pwrss", "z", "onecor"))
    expect_equal(names(crrRes), c("parms", "test", "design", "delta", "q", "mean", "sd", "null.mean", "null.sd", "alternative",
                                  "z.alpha", "n", "power"))
    expect_equal(crrRes[["parms"]], list(rho = 0.20, null.rho = 0.10, alpha = 0.05, alternative = "one.sided", ceiling = TRUE,
                                         verbose = 0, pretty = FALSE))
    expect_equal(crrRes[c("test", "design", "delta", "q", "mean", "sd", "null.mean", "null.sd", "alternative", "z.alpha", "n", "power")],
                 list(test = "z", design = "one.sample", delta = 0.1, q = 0.102397206, mean = 2.4872195, sd = 1, null.mean = 0, null.sd = 1,
                      alternative = "one.sided", z.alpha = 1.64485363, n = 593, power = 0.8002084))

    crrRes <- power.z.onecor(rho = 0.20, null = 0.10, n = 593, alpha = 0.05, alternative = "one.sided", verbose = 0)
    expect_equal(class(crrRes), c("pwrss", "z", "onecor"))
    expect_equal(names(crrRes), c("parms", "test", "design", "delta", "q", "mean", "sd", "null.mean", "null.sd", "alternative",
                                  "z.alpha", "n", "power"))
    expect_equal(crrRes[["parms"]], list(rho = 0.20, null.rho = 0.10, alpha = 0.05, alternative = "one.sided", ceiling = TRUE,
                                         verbose = 0, pretty = FALSE))
    expect_equal(crrRes[c("test", "design", "delta", "q", "mean", "sd", "null.mean", "null.sd", "alternative", "z.alpha", "n", "power")],
                 list(test = "z", design = "one.sample", delta = 0.1, q = 0.102397206, mean = 2.4872195, sd = 1, null.mean = 0, null.sd = 1,
                      alternative = "one.sided", z.alpha = 1.64485363, n = 593, power = 0.8002084))
})
