test_that("proportions.mcnemar.R works", {
    # NB: the code is partially tested through power.exact.twoprops which serves as a wrapper to power.exact.fisher
    # (for independent tests) and power.exact.mcnemar (for paired tests)

    # power.exact.mcnemar ----------------------------------------------------------------------------------------------
    crrRes <- power.exact.mcnemar(prob10 = 0.20, prob01 = 0.10, n.paired = 100, alpha = 0.05, alternative = "two.sided",
                                  method = "exact", verbose = 0)
    crrOut <- capture.output(power.exact.mcnemar(prob10 = 0.20, prob01 = 0.10, n.paired = 100, alpha = 0.05, alternative = "two.sided",
                             method = "exact"))
    crrDtl <- capture.output(power.exact.mcnemar(prob10 = 0.20, prob01 = 0.10, n.paired = 100, alpha = 0.05, alternative = "two.sided",
                             method = "exact", verbose = 2))
    crrPty <- capture.output(power.exact.mcnemar(prob10 = 0.20, prob01 = 0.10, n.paired = 100, alpha = 0.05, alternative = "two.sided",
                             method = "exact", pretty = TRUE))
    crrPnD <- capture.output(power.exact.mcnemar(prob10 = 0.20, prob01 = 0.10, n.paired = 100, alpha = 0.05, alternative = "two.sided",
                             method = "exact", verbose = 2, pretty = TRUE))
    expect_equal(class(crrRes), c("pwrss", "exact", "mcnemar"))
    expect_equal(names(crrRes), c("parms", "test", "delta", "odds.ratio", "size", "prob", "null.prob", "binom.alpha", "mean", "sd",
                                  "null.mean", "null.sd", "z.alpha", "alpha", "power", "n.paired"))
    expect_equal(crrRes[["parms"]],
                 list(prob10 = 0.20, prob01 = 0.10, alpha = 0.05, alternative = "two.sided", method = "exact", ceiling = TRUE,
                      verbose = 0, pretty = FALSE))
    expect_equal(crrRes[c("test", "delta", "odds.ratio", "size", "prob", "null.prob", "binom.alpha", "mean", "sd", "null.mean",
                          "null.sd", "z.alpha", "alpha", "power", "n.paired")],
                 list(test = "exact", delta = 0.1, odds.ratio = 2, size = 30, prob = 2 / 3, null.prob = 0.5,
                      binom.alpha = c(9, 20), mean = NA, sd = NA, null.mean = NA, null.sd = NA, z.alpha = NA,
                      alpha = 0.042773945, power = 0.373077084, n.paired = 100))
    expect_equal(crrOut, c("+--------------------------------------------------+",
                           "|                POWER CALCULATION                 |",
                           "+--------------------------------------------------+", "",
                           "Paired Proportions", "",
                           "  Method          : McNemar's Exact", "",
                           "----------------------------------------------------",
                           "Hypotheses",
                           "----------------------------------------------------",
                           "  H0 (Null Claim) : prob10 - prob01 = 0",
                           "  H1 (Alt. Claim) : prob10 - prob01 != 0", "",
                           "----------------------------------------------------",
                           "Results",
                           "----------------------------------------------------",
                           "  Paired Sample Size   = 100",
                           "  Type 1 Error (alpha) = 0.043",
                           "  Type 2 Error (beta)  = 0.627",
                           "  Statistical Power    = 0.373  <<", ""))
    expect_equal(crrDtl, c("+--------------------------------------------------+",
                           "|                POWER CALCULATION                 |",
                           "+--------------------------------------------------+", "",
                           "Paired Proportions", "",
                           "  Method          : McNemar's Exact", "",
                           "----------------------------------------------------",
                           "Hypotheses",
                           "----------------------------------------------------",
                           "  H0 (Null Claim) : prob10 - prob01 = 0",
                           "  H1 (Alt. Claim) : prob10 - prob01 != 0", "",
                           "----------------------------------------------------",
                           "Key Parameters",
                           "----------------------------------------------------",
                           "  Odds Ratio           = 2.000",
                           "  prob10 - prob01      = 0.100",
                           "  Size of Disc. Pairs  = 30 ",
                           "  prob10 || DP for Alt. = 0.667 ",
                           "  prob10 || DP for Null = 0.5 ",
                           "  Critical Value       = 9 and 20 ", "",
                           "----------------------------------------------------",
                           "Results",
                           "----------------------------------------------------",
                           "  Paired Sample Size   = 100",
                           "  Type 1 Error (alpha) = 0.043",
                           "  Type 2 Error (beta)  = 0.627",
                           "  Statistical Power    = 0.373  <<", "",
                           "----------------------------------------------------",
                           "Definitions",
                           "----------------------------------------------------",
                           "  prob10      : Joint prob. of observing {1,0} ",
                           "  prob01      : Joint prob. of observing {0,1} ",
                           "  Odds Ratio  : prob10 / prob01 ",
                           "  prob10 | DP : Conditional prob. of observing {1,0} ",
                           "                among DP, prob10 / (prob10 + prob01) ",
                           "  DP          : Discordant pairs ", ""))
    expect_equal(crrPty, c("╔══════════════════════════════════════════════════╗",
                           "║               \033[34m POWER CALCULATION \033[0m                ║",
                           "╚══════════════════════════════════════════════════╝", "",
                           "Paired Proportions", "",
                           "  Method              : McNemar's Exact", "",
                           "────────────────────────────────────────────────────",
                           "Hypotheses",
                           "────────────────────────────────────────────────────",
                           "  H₀ (Null)          : P₁₀ - P₀₁ = 0 ",
                           "  H₁ (Alternative)   : P₁₀ - P₀₁ ≠ 0 ", "",
                           "────────────────────────────────────────────────────",
                           "Results",
                           "────────────────────────────────────────────────────",
                           "  Paired Sample Size = 100",
                           "  Type 1 Error (α)   = 0.043",
                           "  Type 2 Error (β)   = 0.627",
                           "  \033[34mStatistical Power  = 0.373\033[0m  \033[1;35m◄◄\033[0m", ""))
    expect_equal(crrPnD, c("╔══════════════════════════════════════════════════╗",
                           "║               \033[34m POWER CALCULATION \033[0m                ║",
                           "╚══════════════════════════════════════════════════╝", "",
                           "Paired Proportions", "",
                           "  Method              : McNemar's Exact", "",
                           "────────────────────────────────────────────────────",
                           "Hypotheses",
                           "────────────────────────────────────────────────────",
                           "  H₀ (Null)          : P₁₀ - P₀₁ = 0 ",
                           "  H₁ (Alternative)   : P₁₀ - P₀₁ ≠ 0 ", "",
                           "────────────────────────────────────────────────────",
                           "Key Parameters",
                           "────────────────────────────────────────────────────",
                           "  P₁₀ - P₀₁            = 0.100 ",
                           "  Odds Ratio (OR)    = 2.000 ",
                           "  Size (Discordant)  = 30 ",
                           "  P₁                  = 0.667 ",
                           "  P₀                  = 0.5 ",
                           "  Bin⁻¹(α, P₀)         = 9 and 20 ", "",
                           "────────────────────────────────────────────────────",
                           "Results",
                           "────────────────────────────────────────────────────",
                           "  Paired Sample Size = 100",
                           "  Type 1 Error (α)   = 0.043",
                           "  Type 2 Error (β)   = 0.627",
                           "  \033[34mStatistical Power  = 0.373\033[0m  \033[1;35m◄◄\033[0m", "",
                           "\033[36m────────────────────────────────────────────────────\033[0m",
                           "\033[36mDefinitions\033[0m",
                           "\033[36m────────────────────────────────────────────────────\033[0m",
                           "\033[36m  OR : P₁₀ / P₀₁ ",
                           "\033[0m\033[36m  P₁  : Prob. of {1,0} among discordant pairs under alt. ",
                           "\033[0m\033[36m  P₀  : Prob. of {1,0} among discordant pairs under null ", "", "\033[0m"))

    crrRes <- power.exact.mcnemar(prob10 = 0.10, prob01 = 0.20, n.paired = 100, alpha = 0.05, alternative = "one.sided",
                                  method = "exact", verbose = 0)
    expect_equal(class(crrRes), c("pwrss", "exact", "mcnemar"))
    expect_equal(names(crrRes), c("parms", "test", "delta", "odds.ratio", "size", "prob", "null.prob", "binom.alpha", "mean", "sd",
                                  "null.mean", "null.sd", "z.alpha", "alpha", "power", "n.paired"))
    expect_equal(crrRes[["parms"]],
                 list(prob10 = 0.10, prob01 = 0.20, alpha = 0.05, alternative = "one.sided", method = "exact", ceiling = TRUE,
                      verbose = 0, pretty = FALSE))
    expect_equal(crrRes[c("test", "delta", "odds.ratio", "size", "prob", "null.prob", "binom.alpha", "mean", "sd", "null.mean",
                          "null.sd", "z.alpha", "alpha", "power", "n.paired")],
                 list(test = "exact", delta = -0.1, odds.ratio = 0.5, size = 30, prob = 1 / 3, null.prob = 0.5,
                      binom.alpha = 10, mean = NA, sd = NA, null.mean = NA, null.sd = NA, z.alpha = NA,
                      alpha = 0.049368573, power = 0.514606143, n.paired = 100))

    crrRes <- power.exact.mcnemar(prob10 = 0.20, prob01 = 0.10, n.paired = 100, alpha = 0.05, alternative = "one.sided",
                                  method = "exact", verbose = 0)
    expect_equal(class(crrRes), c("pwrss", "exact", "mcnemar"))
    expect_equal(names(crrRes), c("parms", "test", "delta", "odds.ratio", "size", "prob", "null.prob", "binom.alpha", "mean", "sd",
                                  "null.mean", "null.sd", "z.alpha", "alpha", "power", "n.paired"))
    expect_equal(crrRes[["parms"]],
                 list(prob10 = 0.20, prob01 = 0.10, alpha = 0.05, alternative = "one.sided", method = "exact", ceiling = TRUE,
                      verbose = 0, pretty = FALSE))
    expect_equal(crrRes[c("test", "delta", "odds.ratio", "size", "prob", "null.prob", "binom.alpha", "mean", "sd", "null.mean",
                          "null.sd", "z.alpha", "alpha", "power", "n.paired")],
                 list(test = "exact", delta = 0.1, odds.ratio = 2, size = 30, prob = 2 / 3, null.prob = 0.5,
                      binom.alpha = 19, mean = NA, sd = NA, null.mean = NA, null.sd = NA, z.alpha = NA,
                      alpha = 0.049368573, power = 0.514606143, n.paired = 100))

    crrRes <- power.exact.mcnemar(prob10 = 0.10, prob01 = 0.20, n = 500, alpha = 0.05, alternative = "two.sided",
                                  method = "approximate", verbose = 0)
    expect_equal(class(crrRes), c("pwrss", "z", "twoprops"))
    expect_equal(names(crrRes), c("parms", "test", "delta", "odds.ratio", "size", "prob", "null.prob", "binom.alpha", "mean", "sd",
                                  "null.mean", "null.sd", "z.alpha", "alpha", "power", "n.paired"))
    expect_equal(crrRes[["parms"]],
                 list(prob10 = 0.10, prob01 = 0.20, alpha = 0.05, alternative = "two.sided", method = "approximate", ceiling = TRUE,
                      verbose = 0, pretty = FALSE))
    expect_equal(crrRes[c("test", "delta", "odds.ratio", "size", "prob", "null.prob", "binom.alpha", "mean", "sd", "null.mean",
                          "null.sd", "z.alpha", "alpha", "power", "n.paired")],
                 list(test = "z", delta = -0.1, odds.ratio = 0.5, size = 150, prob = 1 / 3, null.prob = 0.5,
                      binom.alpha = c(62, 87), mean = -4.118767908, sd = 1, null.mean = 0, null.sd = 1, z.alpha = 1.95996398 * c(-1, 1),
                      alpha = 0.05, power = 0.98456731, n.paired = 500))

    crrRes <- power.exact.mcnemar(prob10 = 0.20, prob01 = 0.10, n = 500, alpha = 0.05, alternative = "one.sided",
                                  method = "approximate", verbose = 0)
    expect_equal(class(crrRes), c("pwrss", "z", "twoprops"))
    expect_equal(names(crrRes), c("parms", "test", "delta", "odds.ratio", "size", "prob", "null.prob", "binom.alpha", "mean", "sd",
                                  "null.mean", "null.sd", "z.alpha", "alpha", "power", "n.paired"))
    expect_equal(crrRes[["parms"]],
                 list(prob10 = 0.20, prob01 = 0.10, alpha = 0.05, alternative = "one.sided", method = "approximate", ceiling = TRUE,
                      verbose = 0, pretty = FALSE))
    expect_equal(crrRes[c("test", "delta", "odds.ratio", "size", "prob", "null.prob", "binom.alpha", "mean", "sd", "null.mean",
                          "null.sd", "z.alpha", "alpha", "power", "n.paired")],
                 list(test = "z", delta = 0.1, odds.ratio = 2, size = 150, prob = 2 / 3, null.prob = 0.5,
                      binom.alpha = 85, mean = 4.1241548, sd = 1, null.mean = 0, null.sd = 1, z.alpha = 1.64485363,
                      alpha = 0.05, power = 0.993418, n.paired = 500))

    crrRes <- power.exact.mcnemar(prob10 = 0.10, prob01 = 0.20, n = 500, alpha = 0.05, alternative = "one.sided",
                                  method = "approximate", verbose = 0)
    expect_equal(class(crrRes), c("pwrss", "z", "twoprops"))
    expect_equal(names(crrRes), c("parms", "test", "delta", "odds.ratio", "size", "prob", "null.prob", "binom.alpha", "mean", "sd",
                                  "null.mean", "null.sd", "z.alpha", "alpha", "power", "n.paired"))
    expect_equal(crrRes[["parms"]],
                 list(prob10 = 0.10, prob01 = 0.20, alpha = 0.05, alternative = "one.sided", method = "approximate", ceiling = TRUE,
                      verbose = 0, pretty = FALSE))
    expect_equal(crrRes[c("test", "delta", "odds.ratio", "size", "prob", "null.prob", "binom.alpha", "mean", "sd", "null.mean",
                          "null.sd", "z.alpha", "alpha", "power", "n.paired")],
                 list(test = "z", delta = -0.1, odds.ratio = 0.5, size = 150, prob = 1 / 3, null.prob = 0.5,
                      binom.alpha = 64, mean = -4.1241548, sd = 1, null.mean = 0, null.sd = 1, z.alpha = -1.64485363,
                      alpha = 0.05, power = 0.993418, n.paired = 500))

    crrRes <- power.exact.mcnemar(prob10 = 0.20, prob01 = 0.10, power = 0.80, alpha = 0.05, alternative = "two.sided",
                                  method = "exact", verbose = 0)
    crrOut <- capture.output(power.exact.mcnemar(prob10 = 0.20, prob01 = 0.10, power = 0.80, alpha = 0.05, alternative = "two.sided",
                             method = "exact"))
    crrDtl <- capture.output(power.exact.mcnemar(prob10 = 0.20, prob01 = 0.10, power = 0.80, alpha = 0.05, alternative = "two.sided",
                             method = "exact", verbose = 2))
    crrPty <- capture.output(power.exact.mcnemar(prob10 = 0.20, prob01 = 0.10, power = 0.80, alpha = 0.05, alternative = "two.sided",
                             method = "exact", pretty = TRUE))
    crrPnD <- capture.output(power.exact.mcnemar(prob10 = 0.20, prob01 = 0.10, power = 0.80, alpha = 0.05, alternative = "two.sided",
                             method = "exact", verbose = 2, pretty = TRUE))
    expect_equal(class(crrRes), c("pwrss", "exact", "mcnemar"))
    expect_equal(names(crrRes), c("parms", "test", "delta", "odds.ratio", "size", "prob", "null.prob", "binom.alpha", "mean", "sd",
                                  "null.mean", "null.sd", "z.alpha", "alpha", "power", "n.paired"))
    expect_equal(crrRes[["parms"]],
                 list(prob10 = 0.20, prob01 = 0.10, alpha = 0.05, alternative = "two.sided", method = "exact", ceiling = TRUE,
                      verbose = 0, pretty = FALSE))
    expect_equal(crrRes[c("test", "delta", "odds.ratio", "size", "prob", "null.prob", "binom.alpha", "mean", "sd", "null.mean",
                          "null.sd", "z.alpha", "alpha", "power", "n.paired")],
                 list(test = "exact", delta = 0.1, odds.ratio = 2, size = 75, prob = 2 / 3, null.prob = 0.5,
                      binom.alpha = c(28, 46), mean = NA, sd = NA, null.mean = NA, null.sd = NA, z.alpha = NA,
                      alpha = 0.036954904, power = 0.801446921, n.paired = 249))
    expect_equal(crrOut, c("+--------------------------------------------------+",
                           "|             SAMPLE SIZE CALCULATION              |",
                           "+--------------------------------------------------+", "",
                           "Paired Proportions", "",
                           "  Method          : McNemar's Exact", "",
                           "----------------------------------------------------",
                           "Hypotheses",
                           "----------------------------------------------------",
                           "  H0 (Null Claim) : prob10 - prob01 = 0",
                           "  H1 (Alt. Claim) : prob10 - prob01 != 0", "",
                           "----------------------------------------------------",
                           "Results",
                           "----------------------------------------------------",
                           "  Paired Sample Size   = 249  <<",
                           "  Type 1 Error (alpha) = 0.037",
                           "  Type 2 Error (beta)  = 0.199",
                           "  Statistical Power    = 0.801", ""))
    expect_equal(crrDtl, c("+--------------------------------------------------+",
                           "|             SAMPLE SIZE CALCULATION              |",
                           "+--------------------------------------------------+", "",
                           "Paired Proportions", "",
                           "  Method          : McNemar's Exact", "",
                           "----------------------------------------------------",
                           "Hypotheses",
                           "----------------------------------------------------",
                           "  H0 (Null Claim) : prob10 - prob01 = 0",
                           "  H1 (Alt. Claim) : prob10 - prob01 != 0", "",
                           "----------------------------------------------------",
                           "Key Parameters",
                           "----------------------------------------------------",
                           "  Odds Ratio           = 2.000",
                           "  prob10 - prob01      = 0.100",
                           "  Size of Disc. Pairs  = 75 ",
                           "  prob10 || DP for Alt. = 0.667 ",
                           "  prob10 || DP for Null = 0.5 ",
                           "  Critical Value       = 28 and 46 ", "",
                           "----------------------------------------------------",
                           "Results",
                           "----------------------------------------------------",
                           "  Paired Sample Size   = 249  <<",
                           "  Type 1 Error (alpha) = 0.037",
                           "  Type 2 Error (beta)  = 0.199",
                           "  Statistical Power    = 0.801", "",
                           "----------------------------------------------------",
                           "Definitions",
                           "----------------------------------------------------",
                           "  prob10      : Joint prob. of observing {1,0} ",
                           "  prob01      : Joint prob. of observing {0,1} ",
                           "  Odds Ratio  : prob10 / prob01 ",
                           "  prob10 | DP : Conditional prob. of observing {1,0} ",
                           "                among DP, prob10 / (prob10 + prob01) ",
                           "  DP          : Discordant pairs ", ""))
    expect_equal(crrPty, c("╔══════════════════════════════════════════════════╗",
                           "║           \033[34m SAMPLE SIZE CALCULATION \033[0m              ║",
                           "╚══════════════════════════════════════════════════╝", "",
                           "Paired Proportions", "",
                           "  Method              : McNemar's Exact", "",
                           "────────────────────────────────────────────────────",
                           "Hypotheses",
                           "────────────────────────────────────────────────────",
                           "  H₀ (Null)          : P₁₀ - P₀₁ = 0 ",
                           "  H₁ (Alternative)   : P₁₀ - P₀₁ ≠ 0 ", "",
                           "────────────────────────────────────────────────────",
                           "Results",
                           "────────────────────────────────────────────────────",
                           "  \033[34mPaired Sample Size = 249\033[0m  \033[1;35m◄◄\033[0m",
                           "  Type 1 Error (α)   = 0.037",
                           "  Type 2 Error (β)   = 0.199",
                           "  Statistical Power  = 0.801", ""))
    expect_equal(crrPnD, c("╔══════════════════════════════════════════════════╗",
                           "║           \033[34m SAMPLE SIZE CALCULATION \033[0m              ║",
                           "╚══════════════════════════════════════════════════╝", "",
                           "Paired Proportions", "",
                           "  Method              : McNemar's Exact", "",
                           "────────────────────────────────────────────────────",
                           "Hypotheses",
                           "────────────────────────────────────────────────────",
                           "  H₀ (Null)          : P₁₀ - P₀₁ = 0 ",
                           "  H₁ (Alternative)   : P₁₀ - P₀₁ ≠ 0 ", "",
                           "────────────────────────────────────────────────────",
                           "Key Parameters",
                           "────────────────────────────────────────────────────",
                           "  P₁₀ - P₀₁            = 0.100 ",
                           "  Odds Ratio (OR)    = 2.000 ",
                           "  Size (Discordant)  = 75 ",
                           "  P₁                  = 0.667 ",
                           "  P₀                  = 0.5 ",
                           "  Bin⁻¹(α, P₀)         = 28 and 46 ", "",
                           "────────────────────────────────────────────────────",
                           "Results",
                           "────────────────────────────────────────────────────",
                           "  \033[34mPaired Sample Size = 249\033[0m  \033[1;35m◄◄\033[0m",
                           "  Type 1 Error (α)   = 0.037",
                           "  Type 2 Error (β)   = 0.199",
                           "  Statistical Power  = 0.801", "",
                           "\033[36m────────────────────────────────────────────────────\033[0m",
                           "\033[36mDefinitions\033[0m",
                           "\033[36m────────────────────────────────────────────────────\033[0m",
                           "\033[36m  OR : P₁₀ / P₀₁ ",
                           "\033[0m\033[36m  P₁  : Prob. of {1,0} among discordant pairs under alt. ",
                           "\033[0m\033[36m  P₀  : Prob. of {1,0} among discordant pairs under null ", "", "\033[0m"))

    crrRes <- power.exact.mcnemar(prob10 = 0.20, prob01 = 0.10, power = 0.80, alpha = 0.05, alternative = "one.sided",
                                  method = "exact", verbose = 0)
    expect_equal(class(crrRes), c("pwrss", "exact", "mcnemar"))
    expect_equal(names(crrRes), c("parms", "test", "delta", "odds.ratio", "size", "prob", "null.prob", "binom.alpha", "mean", "sd",
                                  "null.mean", "null.sd", "z.alpha", "alpha", "power", "n.paired"))
    expect_equal(crrRes[["parms"]],
                 list(prob10 = 0.20, prob01 = 0.10, alpha = 0.05, alternative = "one.sided", method = "exact", ceiling = TRUE,
                      verbose = 0, pretty = FALSE))
    expect_equal(crrRes[c("test", "delta", "odds.ratio", "size", "prob", "null.prob", "binom.alpha", "mean", "sd", "null.mean",
                          "null.sd", "z.alpha", "alpha", "power", "n.paired")],
                 list(test = "exact", delta = 0.1, odds.ratio = 2, size = 60, prob = 2 / 3, null.prob = 0.5,
                      binom.alpha = 36, mean = NA, sd = NA, null.mean = NA, null.sd = NA, z.alpha = NA,
                      alpha = 0.04623049, power = 0.80298386, n.paired = 200))

    crrRes <- power.exact.mcnemar(prob10 = 0.10, prob01 = 0.20, power = 0.80, alpha = 0.05, alternative = "one.sided",
                                  method = "exact", verbose = 0)
    expect_equal(class(crrRes), c("pwrss", "exact", "mcnemar"))
    expect_equal(names(crrRes), c("parms", "test", "delta", "odds.ratio", "size", "prob", "null.prob", "binom.alpha", "mean", "sd",
                                  "null.mean", "null.sd", "z.alpha", "alpha", "power", "n.paired"))
    expect_equal(crrRes[["parms"]],
                 list(prob10 = 0.10, prob01 = 0.20, alpha = 0.05, alternative = "one.sided", method = "exact", ceiling = TRUE,
                      verbose = 0, pretty = FALSE))
    expect_equal(crrRes[c("test", "delta", "odds.ratio", "size", "prob", "null.prob", "binom.alpha", "mean", "sd", "null.mean",
                          "null.sd", "z.alpha", "alpha", "power", "n.paired")],
                 list(test = "exact", delta = -0.1, odds.ratio = 0.5, size = 60, prob = 1 / 3, null.prob = 0.5,
                      binom.alpha = 23, mean = NA, sd = NA, null.mean = NA, null.sd = NA, z.alpha = NA,
                      alpha = 0.04623049, power = 0.80298386, n.paired = 200))

})
