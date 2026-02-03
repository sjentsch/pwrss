test_that("correlations.R works", {
    # power.z.steiger (= power.z.twocors.steiger [the latter is exported]) ---------------------------------------------
    crrRes <- power.z.twocors.steiger(rho12 = 0.35, rho13 = 0.45, rho23 = 0.05, n = 1000, power = NULL, alpha = 0.05,
                                      alternative = "two.sided", common.index = TRUE, verbose = 0)
    crrOut <- capture.output(power.z.twocors.steiger(rho12 = 0.35, rho13 = 0.45, rho23 = 0.05, n = 1000, power = NULL,
                                                     alpha = 0.05, alternative = "two.sided", common.index = TRUE))
    crrPty <- capture.output(power.z.twocors.steiger(rho12 = 0.35, rho13 = 0.45, rho23 = 0.05, n = 1000, power = NULL,
                                                     alpha = 0.05, alternative = "two.sided", common.index = TRUE, pretty = TRUE))
    expect_equal(class(crrRes), c("pwrss", "z", "twocors", "paired"))
    expect_equal(crrRes$parms, list(rho12 = 0.35, rho13 = 0.45, rho23 = 0.05, rho14 = NULL, rho24 = NULL, rho34 = NULL, alpha = 0.05,
                                    alternative = "two.sided", pooled = TRUE, common.index = TRUE, ceiling = TRUE, verbose = 0, pretty = FALSE))
    expect_equal(crrRes[c("test", "design", "delta", "q", "mean", "sd", "null.mean", "null.sd", "alternative", "z.alpha", "power", "n")],
                 list(test = "z", design = "paired", delta = -0.1, q = -0.119256524, mean = -2.62533987, sd = 0.999426958, null.mean = 0,
                      null.sd = 1, alternative = "two.sided", z.alpha = c(-1.959964, 1.959964), power = 0.747219145, n = 1000))
    expect_equal(crrOut, c("+--------------------------------------------------+",
                           "|                POWER CALCULATION                 |",
                           "+--------------------------------------------------+", "",
                           "Dependent Correlations", "",
                           "  Common Index    : TRUE", "",
                           "----------------------------------------------------",
                           "Hypotheses",
                           "----------------------------------------------------",
                           "  H0 (Null Claim) : rho12 - rho13 = 0",
                           "  H1 (Alt. Claim) : rho12 - rho13 != 0", "",
                           "----------------------------------------------------",
                           "Results",
                           "----------------------------------------------------",
                           "  Sample Size          = 1000",
                           "  Type 1 Error (alpha) = 0.050",
                           "  Type 2 Error (beta)  = 0.253",
                           "  Statistical Power    = 0.747  <<", ""))
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
                           "Results",
                           "────────────────────────────────────────────────────",
                           "  Sample Size       = 1000",
                           "  Type 1 Error (α)  = 0.050",
                           "  Type 2 Error (β)  = 0.253",
                           "  \033[34mStatistical Power = 0.747\033[0m  \033[1;35m◄◄\033[0m", ""))

    crrRes <- power.z.twocors.steiger(rho12 = 0.45, rho13 = 0.45, rho23 = 0.50, rho14 = 0.50, rho24 = 0.80, rho34 = 0.55, n = 1000,
                                      power = NULL, alpha = 0.05, alternative = "two.sided", common.index = FALSE, verbose = 0)
    crrOut <- capture.output(power.z.twocors.steiger(rho12 = 0.45, rho13 = 0.45, rho23 = 0.50, rho14 = 0.50, rho24 = 0.80, rho34 = 0.55,
                                                     n = 1000, power = NULL, alpha = 0.05, alternative = "two.sided", common.index = FALSE))
    expect_equal(class(crrRes), c("pwrss", "z", "twocors", "paired"))
    expect_equal(crrRes$parms, list(rho12 = 0.45, rho13 = 0.45, rho23 = 0.50, rho14 = 0.50, rho24 = 0.80, rho34 = 0.55, alpha = 0.05,
                                    alternative = "two.sided", pooled = TRUE, common.index = FALSE, ceiling = TRUE, verbose = 0, pretty = FALSE))
    expect_equal(crrRes[c("test", "design", "delta", "q", "mean", "sd", "null.mean", "null.sd", "alternative", "z.alpha", "power", "n")],
                 list(test = "z", design = "paired", delta = -0.1, q = -0.133681035, mean = -3.49733543, sd = 0.99997767, null.mean = 0,
                      null.sd = 1, alternative = "two.sided", z.alpha = c(-1.959964, 1.959964), power = 0.937903038, n = 1000))
    expect_equal(crrOut, c("+--------------------------------------------------+",
                           "|                POWER CALCULATION                 |",
                           "+--------------------------------------------------+", "",
                           "Dependent Correlations", "",
                           "  Common Index    : FALSE", "",
                           "----------------------------------------------------",
                           "Hypotheses",
                           "----------------------------------------------------",
                           "  H0 (Null Claim) : rho12 - rho34 = 0",
                           "  H1 (Alt. Claim) : rho12 - rho34 != 0", "",
                           "----------------------------------------------------",
                           "Results",
                           "----------------------------------------------------",
                           "  Sample Size          = 1000",
                           "  Type 1 Error (alpha) = 0.050",
                           "  Type 2 Error (beta)  = 0.062",
                           "  Statistical Power    = 0.938  <<", ""))

    # power.z.twocors (= pwrss.z.2cors) --------------------------------------------------------------------------------

    # power.z.onecor (= pwrss.z.cor) -----------------------------------------------------------------------------------

})
