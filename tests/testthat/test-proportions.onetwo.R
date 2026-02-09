test_that("proportions.onetwo.R works", {
    # power.exact.oneprop ----------------------------------------------------------------------------------------------
    crrRes <- power.exact.oneprop(prob = 0.45, null.prob = 0.50, alpha = 0.05, n = 500, alternative = "one.sided", verbose = 0)
    crrOut <- capture.output(power.exact.oneprop(prob = 0.45, null.prob = 0.50, alpha = 0.05, n = 500, alternative = "one.sided"))
    crrDtl <- capture.output(power.exact.oneprop(prob = 0.45, null.prob = 0.50, alpha = 0.05, n = 500, alternative = "one.sided", verbose = 2))
    crrPty <- capture.output(power.exact.oneprop(prob = 0.45, null.prob = 0.50, alpha = 0.05, n = 500, alternative = "one.sided", pretty = TRUE))
    crrPnD <- capture.output(power.exact.oneprop(prob = 0.45, null.prob = 0.50, alpha = 0.05, n = 500, alternative = "one.sided",
                                                 verbose = 2, pretty = TRUE))
    expect_equal(class(crrRes), c("pwrss", "exact", "oneprop"))
    expect_equal(names(crrRes),
                 c("parms", "test", "delta", "odds.ratio", "prob", "null.prob", "size", "binom.alpha", "power", "n"))
    expect_equal(crrRes[["parms"]],
                 list(prob = 0.45, null.prob = 0.50, alpha = 0.05, alternative = "one.sided", verbose = 0, pretty = FALSE))
    expect_equal(crrRes[c("test", "delta", "odds.ratio", "prob", "null.prob", "size", "binom.alpha", "power", "n")],
                 list(test = "exact", delta = -0.05, odds.ratio = 0.818181818, prob = 0.45, null.prob = 0.50, size = 500,
                      binom.alpha = 231, power = 0.7208035, n = 500))
    expect_equal(crrOut, c("+--------------------------------------------------+",
                           "|                POWER CALCULATION                 |",
                           "+--------------------------------------------------+", "",
                           "One Proportion", "",
                           "  Method                 : Exact", "",
                           "----------------------------------------------------",
                           "Hypotheses",
                           "----------------------------------------------------",
                           "  H0 (Null Claim) : prob - null.prob >= 0",
                           "  H1 (Alt. Claim) : prob - null.prob  < 0", "",
                           "----------------------------------------------------",
                           "Results",
                           "----------------------------------------------------",
                           "  Sample Size          = 500",
                           "  Type 1 Error (alpha) = 0.050",
                           "  Type 2 Error (beta)  = 0.279",
                           "  Statistical Power    = 0.721  <<", ""))
    expect_equal(crrDtl, c("+--------------------------------------------------+",
                           "|                POWER CALCULATION                 |",
                           "+--------------------------------------------------+", "",
                           "One Proportion", "",
                           "  Method                 : Exact", "",
                           "----------------------------------------------------",
                           "Hypotheses",
                           "----------------------------------------------------",
                           "  H0 (Null Claim) : prob - null.prob >= 0",
                           "  H1 (Alt. Claim) : prob - null.prob  < 0", "",
                           "----------------------------------------------------",
                           "Key Parameters",
                           "----------------------------------------------------",
                           "  prob - null.prob      = -0.05",
                           "  Odds Ratio            = 0.818",
                           "  Size                  = 500",
                           "  Prob. Under Alt       = 0.450",
                           "  Prob. Under Null      = 0.5",
                           "  Critical Value        = 231", "",
                           "----------------------------------------------------",
                           "Results",
                           "----------------------------------------------------",
                           "  Sample Size          = 500",
                           "  Type 1 Error (alpha) = 0.050",
                           "  Type 2 Error (beta)  = 0.279",
                           "  Statistical Power    = 0.721  <<", "",
                           "----------------------------------------------------",
                           "Definitions",
                           "----------------------------------------------------",
                           "  Odds Ratio      : Odds(prob) / Odds(null.prob) ",
                           "  Odds(prob)      : prob / (1 - prob) ",
                           "  Odds(null.prob) : null.prob / (1 - null.prob) ", ""))
    expect_equal(crrPty, c("╔══════════════════════════════════════════════════╗",
                           "║               \033[34m POWER CALCULATION \033[0m                ║",
                           "╚══════════════════════════════════════════════════╝", "",
                           "One Proportion", "",
                           "  Method              : Exact", "",
                           "────────────────────────────────────────────────────",
                           "Hypotheses",
                           "────────────────────────────────────────────────────",
                           "  H₀ (Null)          : P - P₀ ≥ 0 ",
                           "  H₁ (Alternative)   : P - P₀ < 0 ", "",
                           "────────────────────────────────────────────────────",
                           "Results",
                           "────────────────────────────────────────────────────",
                           "  Sample Size        = 500",
                           "  Type 1 Error (α)   = 0.050",
                           "  Type 2 Error (β)   = 0.279",
                           "  \033[34mStatistical Power  = 0.721\033[0m  \033[1;35m◄◄\033[0m", ""))
    expect_equal(crrPnD, c("╔══════════════════════════════════════════════════╗",
                           "║               \033[34m POWER CALCULATION \033[0m                ║",
                           "╚══════════════════════════════════════════════════╝", "",
                           "One Proportion", "",
                           "  Method              : Exact", "",
                           "────────────────────────────────────────────────────",
                           "Hypotheses",
                           "────────────────────────────────────────────────────",
                           "  H₀ (Null)          : P - P₀ ≥ 0 ",
                           "  H₁ (Alternative)   : P - P₀ < 0 ", "",
                           "────────────────────────────────────────────────────",
                           "Key Parameters",
                           "────────────────────────────────────────────────────",
                           "  P - P₀              = -0.05 ",
                           "  Odds Ratio (OR)    = 0.818 ",
                           "  Size               = 500 ",
                           "  P                  = 0.450 ",
                           "  P₀                  = 0.5 ",
                           "  Bin⁻¹(α, P₀)         = 231 ", "",
                           "────────────────────────────────────────────────────",
                           "Results",
                           "────────────────────────────────────────────────────",
                           "  Sample Size        = 500",
                           "  Type 1 Error (α)   = 0.050",
                           "  Type 2 Error (β)   = 0.279",
                           "  \033[34mStatistical Power  = 0.721\033[0m  \033[1;35m◄◄\033[0m", "",
                           "\033[36m────────────────────────────────────────────────────\033[0m",
                           "\033[36mDefinitions\033[0m",
                           "\033[36m────────────────────────────────────────────────────\033[0m",
                           "\033[36m  OR       : Odds(P) / Odds(P₀) ",
                           "\033[0m\033[36m  Odds(P)  : P / (1 - P) ",
                           "\033[0m\033[36m  Odds(P₀)  : P₀ / (1 - P₀) ",
                           "\033[0m\033[36m  P        : Probability of success under alternative ",
                           "\033[0m\033[36m  P₀        : Probability of success under null ", "", "\033[0m"))

    crrRes <- power.exact.oneprop(prob = 0.45, null.prob = 0.50, alpha = 0.05, power = 0.80, alternative = "one.sided", verbose = 0)
    crrOut <- capture.output(power.exact.oneprop(prob = 0.45, null.prob = 0.50, alpha = 0.05, power = 0.80, alternative = "one.sided"))
    crrDtl <- capture.output(power.exact.oneprop(prob = 0.45, null.prob = 0.50, alpha = 0.05, power = 0.80, alternative = "one.sided", verbose = 2))
    crrPty <- capture.output(power.exact.oneprop(prob = 0.45, null.prob = 0.50, alpha = 0.05, power = 0.80, alternative = "one.sided", pretty = TRUE))
    crrPnD <- capture.output(power.exact.oneprop(prob = 0.45, null.prob = 0.50, alpha = 0.05, power = 0.80, alternative = "one.sided",
                                                 verbose = 2, pretty = TRUE))
    expect_equal(class(crrRes), c("pwrss", "exact", "oneprop"))
    expect_equal(names(crrRes),
                 c("parms", "test", "delta", "odds.ratio", "prob", "null.prob", "size", "binom.alpha", "power", "n"))
    expect_equal(crrRes[["parms"]],
                 list(prob = 0.45, null.prob = 0.50, alpha = 0.05, alternative = "one.sided", verbose = 0, pretty = FALSE))
    expect_equal(crrRes[c("test", "delta", "odds.ratio", "prob", "null.prob", "size", "binom.alpha", "power", "n")],
                 list(test = "exact", delta = -0.05, odds.ratio = 0.818181818, prob = 0.45, null.prob = 0.50, size = 633,
                      binom.alpha = 295, power = 0.802671532, n = 633))
    expect_equal(crrOut, c("+--------------------------------------------------+",
                           "|             SAMPLE SIZE CALCULATION              |",
                           "+--------------------------------------------------+", "",
                           "One Proportion", "",
                           "  Method                 : Exact", "",
                           "----------------------------------------------------",
                           "Hypotheses",
                           "----------------------------------------------------",
                           "  H0 (Null Claim) : prob - null.prob >= 0",
                           "  H1 (Alt. Claim) : prob - null.prob  < 0", "",
                           "----------------------------------------------------",
                           "Results",
                           "----------------------------------------------------",
                           "  Sample Size          = 633  <<",
                           "  Type 1 Error (alpha) = 0.050",
                           "  Type 2 Error (beta)  = 0.197",
                           "  Statistical Power    = 0.803", ""))
    expect_equal(crrDtl, c("+--------------------------------------------------+",
                           "|             SAMPLE SIZE CALCULATION              |",
                           "+--------------------------------------------------+", "",
                           "One Proportion", "",
                           "  Method                 : Exact", "",
                           "----------------------------------------------------",
                           "Hypotheses",
                           "----------------------------------------------------",
                           "  H0 (Null Claim) : prob - null.prob >= 0",
                           "  H1 (Alt. Claim) : prob - null.prob  < 0", "",
                           "----------------------------------------------------",
                           "Key Parameters",
                           "----------------------------------------------------",
                           "  prob - null.prob      = -0.05",
                           "  Odds Ratio            = 0.818",
                           "  Size                  = 633",
                           "  Prob. Under Alt       = 0.450",
                           "  Prob. Under Null      = 0.5",
                           "  Critical Value        = 295", "",
                           "----------------------------------------------------",
                           "Results",
                           "----------------------------------------------------",
                           "  Sample Size          = 633  <<",
                           "  Type 1 Error (alpha) = 0.050",
                           "  Type 2 Error (beta)  = 0.197",
                           "  Statistical Power    = 0.803", "",
                           "----------------------------------------------------",
                           "Definitions",
                           "----------------------------------------------------",
                           "  Odds Ratio      : Odds(prob) / Odds(null.prob) ",
                           "  Odds(prob)      : prob / (1 - prob) ",
                           "  Odds(null.prob) : null.prob / (1 - null.prob) ", ""))
    expect_equal(crrPty, c("╔══════════════════════════════════════════════════╗",
                           "║           \033[34m SAMPLE SIZE CALCULATION \033[0m              ║",
                           "╚══════════════════════════════════════════════════╝", "",
                           "One Proportion", "",
                           "  Method              : Exact", "",
                           "────────────────────────────────────────────────────",
                           "Hypotheses",
                           "────────────────────────────────────────────────────",
                           "  H₀ (Null)          : P - P₀ ≥ 0 ",
                           "  H₁ (Alternative)   : P - P₀ < 0 ", "",
                           "────────────────────────────────────────────────────",
                           "Results",
                           "────────────────────────────────────────────────────",
                           "  \033[34mSample Size        = 633\033[0m  \033[1;35m◄◄\033[0m",
                           "  Type 1 Error (α)   = 0.050",
                           "  Type 2 Error (β)   = 0.197",
                           "  Statistical Power  = 0.803", ""))
    expect_equal(crrPnD, c("╔══════════════════════════════════════════════════╗",
                           "║           \033[34m SAMPLE SIZE CALCULATION \033[0m              ║",
                           "╚══════════════════════════════════════════════════╝", "",
                           "One Proportion", "",
                           "  Method              : Exact", "",
                           "────────────────────────────────────────────────────",
                           "Hypotheses",
                           "────────────────────────────────────────────────────",
                           "  H₀ (Null)          : P - P₀ ≥ 0 ",
                           "  H₁ (Alternative)   : P - P₀ < 0 ", "",
                           "────────────────────────────────────────────────────",
                           "Key Parameters",
                           "────────────────────────────────────────────────────",
                           "  P - P₀              = -0.05 ",
                           "  Odds Ratio (OR)    = 0.818 ",
                           "  Size               = 633 ",
                           "  P                  = 0.450 ",
                           "  P₀                  = 0.5 ",
                           "  Bin⁻¹(α, P₀)         = 295 ", "",
                           "────────────────────────────────────────────────────",
                           "Results",
                           "────────────────────────────────────────────────────",
                           "  \033[34mSample Size        = 633\033[0m  \033[1;35m◄◄\033[0m",
                           "  Type 1 Error (α)   = 0.050",
                           "  Type 2 Error (β)   = 0.197",
                           "  Statistical Power  = 0.803", "",
                           "\033[36m────────────────────────────────────────────────────\033[0m",
                           "\033[36mDefinitions\033[0m",
                           "\033[36m────────────────────────────────────────────────────\033[0m",
                           "\033[36m  OR       : Odds(P) / Odds(P₀) ",
                           "\033[0m\033[36m  Odds(P)  : P / (1 - P) ",
                           "\033[0m\033[36m  Odds(P₀)  : P₀ / (1 - P₀) ",
                           "\033[0m\033[36m  P        : Probability of success under alternative ",
                           "\033[0m\033[36m  P₀        : Probability of success under null ", "", "\033[0m"))

    expect_error(power.exact.oneprop(prob = 0.45, null.prob = c(0.40, 0.50), alpha = 0.05, n = 500, alternative = "one.sided"),
                 "If `alternative` is \"two.sided\" or \"one.sided\", `null.prob` must be of length one.")
    expect_error(power.exact.oneprop(prob = 0.45, null.prob = 0.50, alpha = 0.05, n = 500, alternative = "two.one.sided"),
                 paste("If `alternative` is \"two.one.sided\", `null.prob` must be of length two \\(lower and upper bounds,",
                       "with the upper bound being larger than the lower bound\\)."))
    expect_error(power.exact.oneprop(prob = 0.45, null.prob = 1.2, alpha = 0.05, n = 500, alternative = "one.sided"),
                 "All elements of `null.prob` need to be valid proportion values \\(numeric, >= 0, and <= 1\\)")

    # power.z.oneprop (= pwrss.z.prop) ---------------------------------------------------------------------------------
    crrRes <- power.z.oneprop(prob = 0.45, null.prob = 0.50, alpha = 0.05, n = 500, alternative = "one.sided", verbose = 0)
    crrOut <- capture.output(power.z.oneprop(prob = 0.45, null.prob = 0.50, alpha = 0.05, n = 500, alternative = "one.sided"))
    crrDtl <- capture.output(power.z.oneprop(prob = 0.45, null.prob = 0.50, alpha = 0.05, n = 500, alternative = "one.sided", verbose = 2))
    crrPty <- capture.output(power.z.oneprop(prob = 0.45, null.prob = 0.50, alpha = 0.05, n = 500, alternative = "one.sided", pretty = TRUE))
    crrPnD <- capture.output(power.z.oneprop(prob = 0.45, null.prob = 0.50, alpha = 0.05, n = 500, alternative = "one.sided",
                                             verbose = 2, pretty = TRUE))
    expect_equal(class(crrRes), c("pwrss", "z", "oneprop"))
    expect_equal(names(crrRes),
                 c("parms", "test", "delta", "odds.ratio", "mean", "sd", "null.mean", "null.sd", "z.alpha", "power", "n"))
    expect_equal(crrRes[["parms"]],
                 list(prob = 0.45, null.prob = 0.50, alpha = 0.05, alternative = "one.sided", std.error = "null",
                      arcsine = FALSE, correct = FALSE, ceiling = TRUE, verbose = 0, pretty = FALSE))
    expect_equal(crrRes[c("test", "delta", "odds.ratio", "mean", "sd", "null.mean", "null.sd", "z.alpha", "power", "n")],
                 list(test = "z", delta = -0.05, odds.ratio = 0.818181818, mean = -2.2473329, sd = 1, null.mean = 0,
                      null.sd = 1.00503782, z.alpha = -1.653140096, power = 0.7238084, n = 500))
    expect_equal(crrOut, c("+--------------------------------------------------+",
                           "|                POWER CALCULATION                 |",
                           "+--------------------------------------------------+", "",
                           "One Proportion", "",
                           "  Method                 : Normal Approximation",
                           "  Continuity Correction  : FALSE",
                           "  Arcsine Transformation : FALSE",
                           "  Standard Error         : Calculated From Null", "",
                           "----------------------------------------------------",
                           "Hypotheses",
                           "----------------------------------------------------",
                           "  H0 (Null Claim) : prob - null.prob >= 0",
                           "  H1 (Alt. Claim) : prob - null.prob  < 0", "",
                           "----------------------------------------------------",
                           "Results",
                           "----------------------------------------------------",
                           "  Sample Size          = 500",
                           "  Type 1 Error (alpha) = 0.050",
                           "  Type 2 Error (beta)  = 0.276",
                           "  Statistical Power    = 0.724  <<", ""))
    expect_equal(crrDtl, c("+--------------------------------------------------+",
                           "|                POWER CALCULATION                 |",
                           "+--------------------------------------------------+", "",
                           "One Proportion", "",
                           "  Method                 : Normal Approximation",
                           "  Continuity Correction  : FALSE",
                           "  Arcsine Transformation : FALSE",
                           "  Standard Error         : Calculated From Null", "",
                           "----------------------------------------------------",
                           "Hypotheses",
                           "----------------------------------------------------",
                           "  H0 (Null Claim) : prob - null.prob >= 0",
                           "  H1 (Alt. Claim) : prob - null.prob  < 0", "",
                           "----------------------------------------------------",
                           "Key Parameters",
                           "----------------------------------------------------",
                           "  prob - null.prob      = -0.05",
                           "  Odds Ratio            = 0.818",
                           "  Mean of Alt.          = -2.247",
                           "  Mean of Null          = 0",
                           "  Critical Value        = -1.653", "",
                           "----------------------------------------------------",
                           "Results",
                           "----------------------------------------------------",
                           "  Sample Size          = 500",
                           "  Type 1 Error (alpha) = 0.050",
                           "  Type 2 Error (beta)  = 0.276",
                           "  Statistical Power    = 0.724  <<", "",
                           "----------------------------------------------------",
                           "Definitions",
                           "----------------------------------------------------",
                           "  Odds Ratio      : Odds(prob) / Odds(null.prob) ",
                           "  Odds(prob)      : prob / (1 - prob) ",
                           "  Odds(null.prob) : null.prob / (1 - null.prob) ", ""))
    expect_equal(crrPty, c("╔══════════════════════════════════════════════════╗",
                           "║               \033[34m POWER CALCULATION \033[0m                ║",
                           "╚══════════════════════════════════════════════════╝", "",
                           "One Proportion", "",
                           "  Method                 : Normal Approximation",
                           "  Continuity Correction  : FALSE",
                           "  Arcsine Transformation : FALSE",
                           "  Standard Error         : Calculated From Null", "",
                           "────────────────────────────────────────────────────",
                           "Hypotheses",
                           "────────────────────────────────────────────────────",
                           "  H₀ (Null)          : P - P₀ ≥ 0 ",
                           "  H₁ (Alternative)   : P - P₀ < 0 ", "",
                           "────────────────────────────────────────────────────",
                           "Results",
                           "────────────────────────────────────────────────────",
                           "  Sample Size        = 500",
                           "  Type 1 Error (α)   = 0.050",
                           "  Type 2 Error (β)   = 0.276",
                           "  \033[34mStatistical Power  = 0.724\033[0m  \033[1;35m◄◄\033[0m", ""))
    expect_equal(crrPnD, c("╔══════════════════════════════════════════════════╗",
                           "║               \033[34m POWER CALCULATION \033[0m                ║",
                           "╚══════════════════════════════════════════════════╝", "",
                           "One Proportion", "",
                           "  Method                 : Normal Approximation",
                           "  Continuity Correction  : FALSE",
                           "  Arcsine Transformation : FALSE",
                           "  Standard Error         : Calculated From Null", "",
                           "────────────────────────────────────────────────────",
                           "Hypotheses",
                           "────────────────────────────────────────────────────",
                           "  H₀ (Null)          : P - P₀ ≥ 0 ",
                           "  H₁ (Alternative)   : P - P₀ < 0 ", "",
                           "────────────────────────────────────────────────────",
                           "Key Parameters",
                           "────────────────────────────────────────────────────",
                           "  P - P₀              = -0.05 ",
                           "  Odds Ratio (OR)    = 0.818 ",
                           "  μ₁                  = -2.247 ",
                           "  μ₀                  = 0 ",
                           "  Z⁻¹(α, μ₀)           = -1.653 ", "",
                           "────────────────────────────────────────────────────",
                           "Results",
                           "────────────────────────────────────────────────────",
                           "  Sample Size        = 500",
                           "  Type 1 Error (α)   = 0.050",
                           "  Type 2 Error (β)   = 0.276",
                           "  \033[34mStatistical Power  = 0.724\033[0m  \033[1;35m◄◄\033[0m", "",
                           "\033[36m────────────────────────────────────────────────────\033[0m",
                           "\033[36mDefinitions\033[0m",
                           "\033[36m────────────────────────────────────────────────────\033[0m",
                           "\033[36m  OR       : Odds(P) / Odds(P₀) ",
                           "\033[0m\033[36m  Odds(P)  : P / (1 - P) ",
                           "\033[0m\033[36m  Odds(P₀)  : P₀ / (1 - P₀) ",
                           "\033[0m\033[36m  μ₁        : Mean of the alternative distribution ",
                           "\033[0m\033[36m  μ₀        : Mean of the null distribution ", "", "\033[0m"))
    crrRes <- power.z.oneprop(prob = 0.45, null.prob = 0.50, alpha = 0.05, n = 500, alternative = "one.sided", arcsine = TRUE, verbose = 0)
    expect_equal(class(crrRes), c("pwrss", "z", "oneprop"))
    expect_equal(names(crrRes),
                 c("parms", "test", "delta", "odds.ratio", "mean", "sd", "null.mean", "null.sd", "z.alpha", "power", "n"))
    expect_equal(crrRes[["parms"]],
                 list(prob = 0.45, null.prob = 0.50, alpha = 0.05, alternative = "one.sided", std.error = "null",
                      arcsine = TRUE, correct = FALSE, ceiling = TRUE, verbose = 0, pretty = FALSE))
    expect_equal(crrRes[c("test", "delta", "odds.ratio", "mean", "sd", "null.mean", "null.sd", "z.alpha", "power", "n")],
                 list(test = "z", delta = -0.05, odds.ratio = 0.818181818, mean = -2.23981163, sd = 1, null.mean = 0,
                      null.sd = 1.00503782, z.alpha = -1.653140096, power = 0.72128783, n = 500))

    crrRes <- power.z.oneprop(prob = 0.45, null.prob = 0.50, alpha = 0.05, n = 500, alternative = "one.sided", correct = TRUE, verbose = 0)
    expect_equal(class(crrRes), c("pwrss", "z", "oneprop"))
    expect_equal(names(crrRes),
                 c("parms", "test", "delta", "odds.ratio", "mean", "sd", "null.mean", "null.sd", "z.alpha", "power", "n"))
    expect_equal(crrRes[["parms"]],
                 list(prob = 0.45, null.prob = 0.50, alpha = 0.05, alternative = "one.sided", std.error = "null",
                      arcsine = FALSE, correct = TRUE, ceiling = TRUE, verbose = 0, pretty = FALSE))
    expect_equal(crrRes[c("test", "delta", "odds.ratio", "mean", "sd", "null.mean", "null.sd", "z.alpha", "power", "n")],
                 list(test = "z", delta = -0.05, odds.ratio = 0.818181818, mean = -2.2023862, sd = 1, null.mean = 0,
                      null.sd = 1.00503782, z.alpha = -1.653140096, power = 0.708581722, n = 500))

    crrRes <- power.z.oneprop(prob = 0.45, null.prob = 0.50, alpha = 0.05, power = 0.80, alternative = "one.sided", verbose = 0)
    crrOut <- capture.output(power.z.oneprop(prob = 0.45, null.prob = 0.50, alpha = 0.05, power = 0.80, alternative = "one.sided"))
    crrDtl <- capture.output(power.z.oneprop(prob = 0.45, null.prob = 0.50, alpha = 0.05, power = 0.80, alternative = "one.sided", verbose = 2))
    crrPty <- capture.output(power.z.oneprop(prob = 0.45, null.prob = 0.50, alpha = 0.05, power = 0.80, alternative = "one.sided", pretty = TRUE))
    crrPnD <- capture.output(power.z.oneprop(prob = 0.45, null.prob = 0.50, alpha = 0.05, power = 0.80, alternative = "one.sided",
                                             verbose = 2, pretty = TRUE))
    expect_equal(class(crrRes), c("pwrss", "z", "oneprop"))
    expect_equal(names(crrRes),
                 c("parms", "test", "delta", "odds.ratio", "mean", "sd", "null.mean", "null.sd", "z.alpha", "power", "n"))
    expect_equal(crrRes[["parms"]],
                 list(prob = 0.45, null.prob = 0.50, alpha = 0.05, alternative = "one.sided", std.error = "null",
                      arcsine = FALSE, correct = FALSE, ceiling = TRUE, verbose = 0, pretty = FALSE))
    expect_equal(crrRes[c("test", "delta", "odds.ratio", "mean", "sd", "null.mean", "null.sd", "z.alpha", "power", "n")],
                 list(test = "z", delta = -0.05, odds.ratio = 0.818181818, mean = -2.49646214, sd = 1, null.mean = 0,
                      null.sd = 1.00503782, z.alpha = -1.653140096, power = 0.800475822, n = 617))
    expect_equal(crrOut, c("+--------------------------------------------------+",
                           "|             SAMPLE SIZE CALCULATION              |",
                           "+--------------------------------------------------+", "",
                           "One Proportion", "",
                           "  Method                 : Normal Approximation",
                           "  Continuity Correction  : FALSE",
                           "  Arcsine Transformation : FALSE",
                           "  Standard Error         : Calculated From Null", "",
                           "----------------------------------------------------",
                           "Hypotheses",
                           "----------------------------------------------------",
                           "  H0 (Null Claim) : prob - null.prob >= 0",
                           "  H1 (Alt. Claim) : prob - null.prob  < 0", "",
                           "----------------------------------------------------",
                           "Results",
                           "----------------------------------------------------",
                           "  Sample Size          = 617  <<",
                           "  Type 1 Error (alpha) = 0.050",
                           "  Type 2 Error (beta)  = 0.200",
                           "  Statistical Power    = 0.800", ""))
    expect_equal(crrDtl, c("+--------------------------------------------------+",
                           "|             SAMPLE SIZE CALCULATION              |",
                           "+--------------------------------------------------+", "",
                           "One Proportion", "",
                           "  Method                 : Normal Approximation",
                           "  Continuity Correction  : FALSE",
                           "  Arcsine Transformation : FALSE",
                           "  Standard Error         : Calculated From Null", "",
                           "----------------------------------------------------",
                           "Hypotheses",
                           "----------------------------------------------------",
                           "  H0 (Null Claim) : prob - null.prob >= 0",
                           "  H1 (Alt. Claim) : prob - null.prob  < 0", "",
                           "----------------------------------------------------",
                           "Key Parameters",
                           "----------------------------------------------------",
                           "  prob - null.prob      = -0.05",
                           "  Odds Ratio            = 0.818",
                           "  Mean of Alt.          = -2.496",
                           "  Mean of Null          = 0",
                           "  Critical Value        = -1.653", "",
                           "----------------------------------------------------",
                           "Results",
                           "----------------------------------------------------",
                           "  Sample Size          = 617  <<",
                           "  Type 1 Error (alpha) = 0.050",
                           "  Type 2 Error (beta)  = 0.200",
                           "  Statistical Power    = 0.800", "",
                           "----------------------------------------------------",
                           "Definitions",
                           "----------------------------------------------------",
                           "  Odds Ratio      : Odds(prob) / Odds(null.prob) ",
                           "  Odds(prob)      : prob / (1 - prob) ",
                           "  Odds(null.prob) : null.prob / (1 - null.prob) ", ""))
    expect_equal(crrRes, pwrss.z.prop(p = 0.45, p0 = 0.50, alpha = 0.05, power = 0.80, alternative = "less", verbose = FALSE))
    expect_equal(crrOut, capture.output(pwrss.z.prop(p = 0.45, p0 = 0.50, alpha = 0.05, power = 0.80, alternative = "less")))

    crrRes <- power.z.oneprop(prob = 0.45, null.prob = 0.50, alpha = 0.05, power = 0.80, alternative = "one.sided", arcsine = TRUE, verbose = 0)
    expect_equal(class(crrRes), c("pwrss", "z", "oneprop"))
    expect_equal(names(crrRes),
                 c("parms", "test", "delta", "odds.ratio", "mean", "sd", "null.mean", "null.sd", "z.alpha", "power", "n"))
    expect_equal(crrRes[["parms"]],
                 list(prob = 0.45, null.prob = 0.50, alpha = 0.05, alternative = "one.sided", std.error = "null",
                      arcsine = TRUE, correct = FALSE, ceiling = TRUE, verbose = 0, pretty = FALSE))
    expect_equal(crrRes[c("test", "delta", "odds.ratio", "mean", "sd", "null.mean", "null.sd", "z.alpha", "power", "n")],
                 list(test = "z", delta = -0.05, odds.ratio = 0.818181818, mean = -2.49615927, sd = 1, null.mean = 0,
                      null.sd = 1.00503782, z.alpha = -1.653140096, power = 0.80039114, n = 621))

    crrRes <- power.z.oneprop(prob = 0.45, null.prob = 0.50, alpha = 0.05, power = 0.80, alternative = "one.sided", correct = TRUE, verbose = 0)
    expect_equal(class(crrRes), c("pwrss", "z", "oneprop"))
    expect_equal(names(crrRes),
                 c("parms", "test", "delta", "odds.ratio", "mean", "sd", "null.mean", "null.sd", "z.alpha", "power", "n"))
    expect_equal(crrRes[["parms"]],
                 list(prob = 0.45, null.prob = 0.50, alpha = 0.05, alternative = "one.sided", std.error = "null",
                      arcsine = FALSE, correct = TRUE, ceiling = TRUE, verbose = 0, pretty = FALSE))
    expect_equal(crrRes[c("test", "delta", "odds.ratio", "mean", "sd", "null.mean", "null.sd", "z.alpha", "power", "n")],
                 list(test = "z", delta = -0.05, odds.ratio = 0.818181818, mean = -2.49677972, sd = 1, null.mean = 0,
                      null.sd = 1.00503782, z.alpha = -1.653140096, power = 0.8005646, n = 637))

    crrRes <- power.z.oneprop(prob = 0.45, null.prob = 0.50, alpha = 0.05, power = 0.80, alternative = "two.sided", verbose = 0)
    expect_equal(class(crrRes), c("pwrss", "z", "oneprop"))
    expect_equal(names(crrRes),
                 c("parms", "test", "delta", "odds.ratio", "mean", "sd", "null.mean", "null.sd", "z.alpha", "power", "n"))
    expect_equal(crrRes[["parms"]],
                 list(prob = 0.45, null.prob = 0.50, alpha = 0.05, alternative = "two.sided", std.error = "null",
                      arcsine = FALSE, correct = FALSE, ceiling = TRUE, verbose = 0, pretty = FALSE))
    expect_equal(crrRes[c("test", "delta", "odds.ratio", "mean", "sd", "null.mean", "null.sd", "z.alpha", "power", "n")],
                 list(test = "z", delta = -0.05, odds.ratio = 0.818181818, mean = 2.8123106, sd = 1, null.mean = 0,
                      null.sd = 1.00503782, z.alpha = 1.96983792100888 * c(-1, 1), power = 0.80023915, n = 783))
    expect_equal(crrRes, pwrss.z.prop(p = 0.45, p0 = 0.50, alpha = 0.05, power = 0.80, alternative = "not equal", verbose = FALSE))

    crrRes <- power.z.oneprop(prob = 0.45, null.prob = c(0.40, 0.50), alpha = 0.05, power = 0.80, alternative = "two.one.sided",
                              std.error = "alternative", verbose = 0)
    expect_equal(class(crrRes), c("pwrss", "z", "oneprop"))
    expect_equal(names(crrRes),
                 c("parms", "test", "delta", "odds.ratio", "mean", "sd", "null.mean", "null.sd", "z.alpha", "power", "n"))
    expect_equal(crrRes[["parms"]],
                 list(prob = 0.45, null.prob = c(0.40, 0.50), alpha = 0.05, alternative = "two.one.sided",
                      std.error = "alternative", arcsine = FALSE, correct = FALSE, ceiling = TRUE, verbose = 0, pretty = FALSE))
    expect_equal(crrRes[c("test", "delta", "odds.ratio", "mean", "sd", "null.mean", "null.sd", "z.alpha", "power", "n")],
                 list(test = "z", delta = 0.05 * c(1, -1), odds.ratio = c(1.2272727, 0.818181818), mean = 0, sd = 1,
                      null.mean = 2.9267143 * c(1, -1), null.sd = 1, z.alpha = c(-1.28186067, 1.28186067),
                      power = 0.800108473, n = 848))

    crrRes <- power.z.oneprop(prob = 0.45, null.prob = c(0.35, 0.40), alpha = 0.05, power = 0.80, alternative = "two.one.sided",
                              std.error = "alternative", verbose = 0)
    expect_equal(class(crrRes), c("pwrss", "z", "oneprop"))
    expect_equal(names(crrRes),
                 c("parms", "test", "delta", "odds.ratio", "mean", "sd", "null.mean", "null.sd", "z.alpha", "power", "n"))
    expect_equal(crrRes[["parms"]],
                 list(prob = 0.45, null.prob = c(0.35, 0.40), alpha = 0.05, alternative = "two.one.sided",
                      std.error = "alternative", arcsine = FALSE, correct = FALSE, ceiling = TRUE, verbose = 0, pretty = FALSE))
    expect_equal(crrRes[c("test", "delta", "odds.ratio", "mean", "sd", "null.mean", "null.sd", "z.alpha", "power", "n")],
                 list(test = "z", delta = 0.05 * c(2, 1), odds.ratio = c(1.519480519, 1.227272727), mean = 0, sd = 1,
                      null.mean = 2.8033169385 * c(2, 1), null.sd = 1, z.alpha = c(0.843353, 7.5665979),
                      power = 0.800484462, n = 778))
    expect_equal(crrRes, pwrss.z.prop(p = 0.45, p0 = c(0.35, 0.40), alpha = 0.05, power = 0.80, alternative = "equivalent", verbose = FALSE))

    expect_warning(power.z.oneprop(prob = 0.45, null.prob = 0.50, n = 500, alternative = "one.sided", arcsine = TRUE, correct = TRUE, verbose = 0),
                   "Continuity correction does not apply to arcsine transformation approach.")
    expect_warning(power.z.oneprop(prob = 0.45, null.prob = c(0.40, 0.50), n = 500, alternative = "two.one.sided", verbose = 0),
                   "`std.error` = \"null\" is ignored. Using \"alternative\" for equivalence or minimal effect testing.")

    expect_error(power.z.oneprop(prob = 0.45, null.prob = c(0.40, 0.50), alpha = 0.05, n = 500, alternative = "one.sided"),
                 "If `alternative` is \"two.sided\" or \"one.sided\", `null.prob` must be of length one.")
    expect_error(power.z.oneprop(prob = 0.45, null.prob = 0.50, alpha = 0.05, n = 500, alternative = "two.one.sided"),
                 paste("If `alternative` is \"two.one.sided\", `null.prob` must be of length two \\(lower and upper bounds,",
                       "with the upper bound being larger than the lower bound\\)."))
    expect_error(power.z.oneprop(prob = 0.45, null.prob = 1.2, alpha = 0.05, n = 500, alternative = "one.sided"),
                 "All elements of `null.prob` need to be valid proportion values \\(numeric, >= 0, and <= 1\\)")
    expect_error(pwrss.z.prop(p = 0.45, p0 = 0.50, margin = 1, alpha = 0.05, power = 0.80, alternative = "less", verbose = FALSE),
                 "Provide a reasonable margin consistent with `p` - `p0`.")

    # power.exact.twoprops ---------------------------------------------------------------------------------------------
    crrRes <- power.exact.twoprops(prob1 = 0.65, prob2 = 0.60, alpha = 0.05, n2 = 500, alternative = "one.sided", verbose = 0)
    crrOut <- capture.output(power.exact.twoprops(prob1 = 0.65, prob2 = 0.60, alpha = 0.05, n2 = 500, alternative = "one.sided"))
    crrDtl <- capture.output(power.exact.twoprops(prob1 = 0.65, prob2 = 0.60, alpha = 0.05, n2 = 500, alternative = "one.sided", verbose = 2))
    crrPty <- capture.output(power.exact.twoprops(prob1 = 0.65, prob2 = 0.60, alpha = 0.05, n2 = 500, alternative = "one.sided", pretty = TRUE))
    crrPnD <- capture.output(power.exact.twoprops(prob1 = 0.65, prob2 = 0.60, alpha = 0.05, n2 = 500, alternative = "one.sided",
                                                  pretty = TRUE, verbose = 2))
    expect_equal(class(crrRes), c("pwrss", "exact", "fisher"))
    expect_equal(names(crrRes),
                 c("parms", "test", "delta", "odds.ratio", "mean", "sd", "null.mean", "null.sd", "alternative", "z.alpha",
                   "power", "n", "n.total"))
    expect_equal(crrRes[["parms"]],
                 list(prob1 = 0.65, prob2 = 0.60, n.ratio = 1, alpha = 0.05, alternative = "one.sided",
                      method = "exact", ceiling = TRUE, verbose = 0, pretty = FALSE))
    expect_equal(crrRes[c("test", "delta", "odds.ratio", "mean", "sd", "null.mean", "null.sd", "alternative", "z.alpha", "power", "n", "n.total")],
                 list(test = "exact", delta = 0.05, odds.ratio = 1.23809524, mean = NA, sd = NA, null.mean = NA, null.sd = NA,
                      alternative = "one.sided", z.alpha = NA, power = 0.466794272, n = c(n1 = 500, n2 = 500), n.total = 1000))
    expect_equal(crrOut, c("+--------------------------------------------------+",
                           "|                POWER CALCULATION                 |",
                           "+--------------------------------------------------+", "",
                           "Independent Proportions", "",
                           "  Method          : Fisher's Exact", "",
                           "----------------------------------------------------",
                           "Hypotheses",
                           "----------------------------------------------------",
                           "  H0 (Null Claim) : prob1 - prob2 <= 0",
                           "  H1 (Alt. Claim) : prob1 - prob2  > 0", "",
                           "----------------------------------------------------",
                           "Results",
                           "----------------------------------------------------",
                           "  Sample Size          = 500 and 500",
                           "  Type 1 Error (alpha) = 0.050",
                           "  Type 2 Error (beta)  = 0.533",
                           "  Statistical Power    = 0.467  <<", ""))
    expect_equal(crrDtl, c("+--------------------------------------------------+",
                           "|                POWER CALCULATION                 |",
                           "+--------------------------------------------------+", "",
                           "Independent Proportions", "",
                           "  Method          : Fisher's Exact", "",
                           "----------------------------------------------------",
                           "Hypotheses",
                           "----------------------------------------------------",
                           "  H0 (Null Claim) : prob1 - prob2 <= 0",
                           "  H1 (Alt. Claim) : prob1 - prob2  > 0", "",
                           "----------------------------------------------------",
                           "Key Parameters",
                           "----------------------------------------------------",
                           "  prob1 - prob2        = 0.050",
                           "  Odds Ratio           = 1.238", "",
                           "----------------------------------------------------",
                           "Results",
                           "----------------------------------------------------",
                           "  Sample Size          = 500 and 500",
                           "  Type 1 Error (alpha) = 0.050",
                           "  Type 2 Error (beta)  = 0.533",
                           "  Statistical Power    = 0.467  <<", "",
                           "----------------------------------------------------",
                           "Definitions",
                           "----------------------------------------------------",
                           "  prob1       : Probability of success in the first group ",
                           "  prob2       : Probability of success in the second group ",
                           "  Odds Ratio  : Odds(prob1) / Odds(prob2) ",
                           "  Odds(prob1) : prob1 / (1 - prob1) ",
                           "  Odds(prob2) : prob2 / (1 - prob2) ", ""))
    expect_equal(crrPty, c("╔══════════════════════════════════════════════════╗",
                           "║               \033[34m POWER CALCULATION \033[0m                ║",
                           "╚══════════════════════════════════════════════════╝", "",
                           "Independent Proportions", "",
                           "  Method            : Fisher's Exact", "",
                           "────────────────────────────────────────────────────",
                           "Hypotheses",
                           "────────────────────────────────────────────────────",
                           "  H₀ (Null)         : P₁ - P₂ ≤ 0 ",
                           "  H₁ (Alternative)  : P₁ - P₂ > 0 ", "",
                           "────────────────────────────────────────────────────",
                           "Results",
                           "────────────────────────────────────────────────────",
                           "  Sample Size        = 500 and 500",
                           "  Type 1 Error (α)   = 0.050",
                           "  Type 2 Error (β)   = 0.533",
                           "  \033[34mStatistical Power  = 0.467\033[0m  \033[1;35m◄◄\033[0m", ""))
    expect_equal(crrPnD, c("╔══════════════════════════════════════════════════╗",
                           "║               \033[34m POWER CALCULATION \033[0m                ║",
                           "╚══════════════════════════════════════════════════╝", "",
                           "Independent Proportions", "",
                           "  Method            : Fisher's Exact", "",
                           "────────────────────────────────────────────────────",
                           "Hypotheses",
                           "────────────────────────────────────────────────────",
                           "  H₀ (Null)         : P₁ - P₂ ≤ 0 ",
                           "  H₁ (Alternative)  : P₁ - P₂ > 0 ", "",
                           "────────────────────────────────────────────────────",
                           "Key Parameters",
                           "────────────────────────────────────────────────────",
                           "  P₁ - P₂             = 0.050 ",
                           "  Odds Ratio (OR)   = 1.238 ", "",
                           "────────────────────────────────────────────────────",
                           "Results",
                           "────────────────────────────────────────────────────",
                           "  Sample Size        = 500 and 500",
                           "  Type 1 Error (α)   = 0.050",
                           "  Type 2 Error (β)   = 0.533",
                           "  \033[34mStatistical Power  = 0.467\033[0m  \033[1;35m◄◄\033[0m", "",
                           "\033[36m────────────────────────────────────────────────────\033[0m",
                           "\033[36mDefinitions\033[0m",
                           "\033[36m────────────────────────────────────────────────────\033[0m",
                           "\033[36m  P₁       : Probability of success in the first group ",
                           "\033[0m\033[36m  P₂       : Probability of success in the second group ",
                           "\033[0m\033[36m  δ         : Margin - ignorable P₁ - P₂ difference ",
                           "\033[0m\033[36m  OR        : Odds(P₁) / Odds(P₂) ",
                           "\033[0m\033[36m  Odds(P₁) : P₁ / (1 - P₁) ",
                           "\033[0m\033[36m  Odds(P₂) : P₂ / (1 - P₂) ", "", "\033[0m"))

    crrRes <- power.exact.twoprops(prob1 = 0.70, prob2 = 0.60, alpha = 0.05, n2 = 500, paired = TRUE, rho.paired = 0.7, verbose = 0)
    expect_equal(class(crrRes), c("pwrss", "exact", "mcnemar"))
    expect_equal(names(crrRes),
                 c("parms", "test", "delta", "odds.ratio", "size", "prob", "null.prob", "binom.alpha", "mean", "sd",
                   "null.mean", "null.sd", "z.alpha", "alpha", "power", "n.paired"))
    expect_equal(crrRes[["parms"]],
                 list(prob10 = 0.12285039, prob01 = 0.02285039, alpha = 0.05, alternative = "two.sided", method = "exact",
                      ceiling = TRUE, verbose = 0, pretty = FALSE))
    expect_equal(crrRes[c("test", "delta", "odds.ratio", "size", "prob", "null.prob", "binom.alpha", "mean", "sd",
                          "null.mean", "null.sd", "z.alpha", "alpha", "power", "n.paired")],
                 list(test = "exact", delta = 0.10, odds.ratio = 5.376293, size = 74, prob = 0.843169063, null.prob = 0.5,
                      binom.alpha = c(28, 45), mean = NA, sd = NA, null.mean = NA, null.sd = NA,
                      z.alpha = NA, alpha = 0.047392976, power = 0.999990832, n.paired = 500))

    crrRes <- power.exact.twoprops(prob1 = 0.70, prob2 = 0.60, alpha = 0.05, power = 0.80, alternative = "one.sided", verbose = 0)
    crrOut <- capture.output(power.exact.twoprops(prob1 = 0.70, prob2 = 0.60, alpha = 0.05, power = 0.80, alternative = "one.sided"))
    crrDtl <- capture.output(power.exact.twoprops(prob1 = 0.70, prob2 = 0.60, alpha = 0.05, power = 0.80, alternative = "one.sided", verbose = 2))
    crrPty <- capture.output(power.exact.twoprops(prob1 = 0.70, prob2 = 0.60, alpha = 0.05, power = 0.80, alternative = "one.sided", pretty = TRUE))
    crrPnD <- capture.output(power.exact.twoprops(prob1 = 0.70, prob2 = 0.60, alpha = 0.05, power = 0.80, alternative = "one.sided",
                                              verbose = 2, pretty = TRUE))
    expect_equal(class(crrRes), c("pwrss", "exact", "fisher"))
    expect_equal(names(crrRes),
                 c("parms", "test", "delta", "odds.ratio", "mean", "sd", "null.mean", "null.sd", "alternative", "z.alpha",
                   "power", "n", "n.total"))
    expect_equal(crrRes[["parms"]],
                 list(prob1 = 0.70, prob2 = 0.60, n.ratio = 1, alpha = 0.05, alternative = "one.sided",
                      method = "exact", ceiling = TRUE, verbose = 0, pretty = FALSE))
    expect_equal(crrRes[c("test", "delta", "odds.ratio", "mean", "sd", "null.mean", "null.sd", "alternative", "z.alpha", "power", "n", "n.total")],
                 list(test = "exact", delta = 0.10, odds.ratio = 1.55555556, mean = NA, sd = NA, null.mean = NA, null.sd = NA,
                      alternative = "one.sided", z.alpha = NA, power = 0.801117239, n = c(n1 = 302, n2 = 302), n.total = 604))
    expect_equal(crrOut, c("+--------------------------------------------------+",
                           "|             SAMPLE SIZE CALCULATION              |",
                           "+--------------------------------------------------+", "",
                           "Independent Proportions", "",
                           "  Method          : Fisher's Exact", "",
                           "----------------------------------------------------",
                           "Hypotheses",
                           "----------------------------------------------------",
                           "  H0 (Null Claim) : prob1 - prob2 <= 0",
                           "  H1 (Alt. Claim) : prob1 - prob2  > 0", "",
                           "----------------------------------------------------",
                           "Results",
                           "----------------------------------------------------",
                           "  Sample Size          = 302 and 302  <<",
                           "  Type 1 Error (alpha) = 0.050",
                           "  Type 2 Error (beta)  = 0.199",
                           "  Statistical Power    = 0.801", ""))
    expect_equal(crrDtl, c("+--------------------------------------------------+",
                           "|             SAMPLE SIZE CALCULATION              |",
                           "+--------------------------------------------------+", "",
                           "Independent Proportions", "",
                           "  Method          : Fisher's Exact", "",
                           "----------------------------------------------------",
                           "Hypotheses",
                           "----------------------------------------------------",
                           "  H0 (Null Claim) : prob1 - prob2 <= 0",
                           "  H1 (Alt. Claim) : prob1 - prob2  > 0", "",
                           "----------------------------------------------------",
                           "Key Parameters",
                           "----------------------------------------------------",
                           "  prob1 - prob2        = 0.100",
                           "  Odds Ratio           = 1.556", "",
                           "----------------------------------------------------",
                           "Results",
                           "----------------------------------------------------",
                           "  Sample Size          = 302 and 302  <<",
                           "  Type 1 Error (alpha) = 0.050",
                           "  Type 2 Error (beta)  = 0.199",
                           "  Statistical Power    = 0.801", "",
                           "----------------------------------------------------",
                           "Definitions",
                           "----------------------------------------------------",
                           "  prob1       : Probability of success in the first group ",
                           "  prob2       : Probability of success in the second group ",
                           "  Odds Ratio  : Odds(prob1) / Odds(prob2) ",
                           "  Odds(prob1) : prob1 / (1 - prob1) ",
                           "  Odds(prob2) : prob2 / (1 - prob2) ", ""))
    expect_equal(crrPty, c("╔══════════════════════════════════════════════════╗",
                           "║           \033[34m SAMPLE SIZE CALCULATION \033[0m              ║",
                           "╚══════════════════════════════════════════════════╝", "",
                           "Independent Proportions", "",
                           "  Method            : Fisher's Exact", "",
                           "────────────────────────────────────────────────────",
                           "Hypotheses",
                           "────────────────────────────────────────────────────",
                           "  H₀ (Null)         : P₁ - P₂ ≤ 0 ",
                           "  H₁ (Alternative)  : P₁ - P₂ > 0 ", "",
                           "────────────────────────────────────────────────────",
                           "Results",
                           "────────────────────────────────────────────────────",
                           "  \033[34mSample Size        = 302 and 302\033[0m  \033[1;35m◄◄\033[0m",
                           "  Type 1 Error (α)   = 0.050",
                           "  Type 2 Error (β)   = 0.199",
                           "  Statistical Power  = 0.801", ""))
    expect_equal(crrPnD, c("╔══════════════════════════════════════════════════╗",
                           "║           \033[34m SAMPLE SIZE CALCULATION \033[0m              ║",
                           "╚══════════════════════════════════════════════════╝", "",
                           "Independent Proportions", "",
                           "  Method            : Fisher's Exact", "",
                           "────────────────────────────────────────────────────",
                           "Hypotheses",
                           "────────────────────────────────────────────────────",
                           "  H₀ (Null)         : P₁ - P₂ ≤ 0 ",
                           "  H₁ (Alternative)  : P₁ - P₂ > 0 ", "",
                           "────────────────────────────────────────────────────",
                           "Key Parameters",
                           "────────────────────────────────────────────────────",
                           "  P₁ - P₂             = 0.100 ",
                           "  Odds Ratio (OR)   = 1.556 ", "",
                           "────────────────────────────────────────────────────",
                           "Results",
                           "────────────────────────────────────────────────────",
                           "  \033[34mSample Size        = 302 and 302\033[0m  \033[1;35m◄◄\033[0m",
                           "  Type 1 Error (α)   = 0.050",
                           "  Type 2 Error (β)   = 0.199",
                           "  Statistical Power  = 0.801", "",
                           "\033[36m────────────────────────────────────────────────────\033[0m",
                           "\033[36mDefinitions\033[0m",
                           "\033[36m────────────────────────────────────────────────────\033[0m",
                           "\033[36m  P₁       : Probability of success in the first group ",
                           "\033[0m\033[36m  P₂       : Probability of success in the second group ",
                           "\033[0m\033[36m  δ         : Margin - ignorable P₁ - P₂ difference ",
                           "\033[0m\033[36m  OR        : Odds(P₁) / Odds(P₂) ",
                           "\033[0m\033[36m  Odds(P₁) : P₁ / (1 - P₁) ",
                           "\033[0m\033[36m  Odds(P₂) : P₂ / (1 - P₂) ", "", "\033[0m"))

    crrRes <- power.exact.twoprops(prob1 = 0.70, prob2 = 0.60, alpha = 0.05, power = 0.80, paired = TRUE, rho.paired = 0.7, verbose = 0)
    expect_equal(class(crrRes), c("pwrss", "exact", "mcnemar"))
    expect_equal(names(crrRes),
                 c("parms", "test", "delta", "odds.ratio", "size", "prob", "null.prob", "binom.alpha", "mean", "sd",
                   "null.mean", "null.sd", "z.alpha", "alpha", "power", "n.paired"))
    expect_equal(crrRes[["parms"]],
                 list(prob10 = 0.12285039, prob01 = 0.02285039, alpha = 0.05, alternative = "two.sided", method = "exact",
                      ceiling = TRUE, verbose = 0, pretty = FALSE))
    expect_equal(crrRes[c("test", "delta", "odds.ratio", "size", "prob", "null.prob", "binom.alpha", "mean", "sd",
                          "null.mean", "null.sd", "z.alpha", "alpha", "power", "n.paired")],
                 list(test = "exact", delta = 0.10, odds.ratio = 5.376293, size = 18, prob = 0.843169063, null.prob = 0.5,
                      binom.alpha = c(4, 13), mean = NA, sd = NA, null.mean = NA, null.sd = NA,
                      z.alpha = NA, alpha = 0.030883789, power = 0.807733751, n.paired = 120))

    # power.z.twoprops (= pwrss.z.2props) ------------------------------------------------------------------------------
    crrRes <- power.z.twoprops(prob1 = 0.65, prob2 = 0.60, alpha = 0.05, n2 = 500, alternative = "one.sided", verbose = 0)
    crrOut <- capture.output(power.z.twoprops(prob1 = 0.65, prob2 = 0.60, alpha = 0.05, n2 = 500, alternative = "one.sided"))
    crrDtl <- capture.output(power.z.twoprops(prob1 = 0.65, prob2 = 0.60, alpha = 0.05, n2 = 500, alternative = "one.sided", verbose = 2))
    crrPty <- capture.output(power.z.twoprops(prob1 = 0.65, prob2 = 0.60, alpha = 0.05, n2 = 500, alternative = "one.sided", pretty = TRUE))
    crrPnD <- capture.output(power.z.twoprops(prob1 = 0.65, prob2 = 0.60, alpha = 0.05, n2 = 500, alternative = "one.sided",
                                              pretty = TRUE, verbose = 2))
    expect_equal(class(crrRes), c("pwrss", "z", "twoprops"))
    expect_equal(names(crrRes),
                 c("parms", "test", "delta", "odds.ratio", "mean", "sd", "null.mean", "null.sd", "z.alpha", "power", "n", "n.total"))
    expect_equal(crrRes[["parms"]],
                 list(prob1 = 0.65, prob2 = 0.6, margin = 0, n.ratio = 1, alpha = 0.05, alternative = "one.sided",
                      arcsine = FALSE, correct = FALSE, paired = FALSE, rho.paired = 0.5, std.error = "pooled",
                      ceiling = TRUE, verbose = 0, pretty = FALSE))
    expect_equal(crrRes[c("test", "delta", "odds.ratio", "mean", "sd", "null.mean", "null.sd", "z.alpha", "power", "n", "n.total")],
                 list(test = "z", delta = 0.05, odds.ratio = 1.23809524, mean = 1.63517485, sd = 1, null.mean = 0, null.sd = 1.001336,
                      z.alpha = 1.64705116, power = 0.495262149, n = c(n1 = 500, n2 = 500), n.total = 1000))
    expect_equal(crrOut, c("+--------------------------------------------------+",
                           "|                POWER CALCULATION                 |",
                           "+--------------------------------------------------+", "",
                           "Independent Proportions", "",
                           "  Method          : Normal Approximation", "",
                           "----------------------------------------------------",
                           "Hypotheses",
                           "----------------------------------------------------",
                           "  H0 (Null Claim) : prob1 - prob2 <= 0",
                           "  H1 (Alt. Claim) : prob1 - prob2  > 0", "",
                           "----------------------------------------------------",
                           "Results",
                           "----------------------------------------------------",
                           "  Sample Size          = 500 and 500",
                           "  Type 1 Error (alpha) = 0.050",
                           "  Type 2 Error (beta)  = 0.505",
                           "  Statistical Power    = 0.495  <<", ""))
    expect_equal(crrDtl, c("+--------------------------------------------------+",
                           "|                POWER CALCULATION                 |",
                           "+--------------------------------------------------+", "",
                           "Independent Proportions", "",
                           "  Method          : Normal Approximation", "",
                           "----------------------------------------------------",
                           "Hypotheses",
                           "----------------------------------------------------",
                           "  H0 (Null Claim) : prob1 - prob2 <= 0",
                           "  H1 (Alt. Claim) : prob1 - prob2  > 0", "",
                           "----------------------------------------------------",
                           "Key Parameters",
                           "----------------------------------------------------",
                           "  prob1 - prob2        = 0.050",
                           "  Odds Ratio           = 1.238",
                           "  Mean of Alt.         = 1.635",
                           "  Mean of Null         = 0",
                           "  Critical Value       = 1.647", "",
                           "----------------------------------------------------",
                           "Results",
                           "----------------------------------------------------",
                           "  Sample Size          = 500 and 500",
                           "  Type 1 Error (alpha) = 0.050",
                           "  Type 2 Error (beta)  = 0.505",
                           "  Statistical Power    = 0.495  <<", "",
                           "----------------------------------------------------",
                           "Definitions",
                           "----------------------------------------------------",
                           "  prob1       : Probability of success in the first group ",
                           "  prob2       : Probability of success in the second group ",
                           "  Odds Ratio  : Odds(prob1) / Odds(prob2) ",
                           "  Odds(prob1) : prob1 / (1 - prob1) ",
                           "  Odds(prob2) : prob2 / (1 - prob2) ", ""))
    expect_equal(crrPty, c("╔══════════════════════════════════════════════════╗",
                           "║               \033[34m POWER CALCULATION \033[0m                ║",
                           "╚══════════════════════════════════════════════════╝", "",
                           "Independent Proportions", "",
                           "  Method            : Normal Approximation", "",
                           "────────────────────────────────────────────────────",
                           "Hypotheses",
                           "────────────────────────────────────────────────────",
                           "  H₀ (Null)         : P₁ - P₂ ≤ 0 ",
                           "  H₁ (Alternative)  : P₁ - P₂ > 0 ", "",
                           "────────────────────────────────────────────────────",
                           "Results",
                           "────────────────────────────────────────────────────",
                           "  Sample Size        = 500 and 500",
                           "  Type 1 Error (α)   = 0.050",
                           "  Type 2 Error (β)   = 0.505",
                           "  \033[34mStatistical Power  = 0.495\033[0m  \033[1;35m◄◄\033[0m", ""))
    expect_equal(crrPnD, c("╔══════════════════════════════════════════════════╗",
                           "║               \033[34m POWER CALCULATION \033[0m                ║",
                           "╚══════════════════════════════════════════════════╝", "",
                           "Independent Proportions", "",
                           "  Method            : Normal Approximation", "",
                           "────────────────────────────────────────────────────",
                           "Hypotheses",
                           "────────────────────────────────────────────────────",
                           "  H₀ (Null)         : P₁ - P₂ ≤ 0 ",
                           "  H₁ (Alternative)  : P₁ - P₂ > 0 ", "",
                           "────────────────────────────────────────────────────",
                           "Key Parameters",
                           "────────────────────────────────────────────────────",
                           "  P₁ - P₂             = 0.050 ",
                           "  Odds Ratio (OR)   = 1.238 ",
                           "  μ                 = 1.635 ",
                           "  μ₀                 = 0 ",
                           "  Z⁻¹(α, μ₀)          = 1.647 ", "",
                           "────────────────────────────────────────────────────",
                           "Results",
                           "────────────────────────────────────────────────────",
                           "  Sample Size        = 500 and 500",
                           "  Type 1 Error (α)   = 0.050",
                           "  Type 2 Error (β)   = 0.505",
                           "  \033[34mStatistical Power  = 0.495\033[0m  \033[1;35m◄◄\033[0m", "",
                           "\033[36m────────────────────────────────────────────────────\033[0m",
                           "\033[36mDefinitions\033[0m",
                           "\033[36m────────────────────────────────────────────────────\033[0m",
                           "\033[36m  P₁       : Probability of success in the first group ",
                           "\033[0m\033[36m  P₂       : Probability of success in the second group ",
                           "\033[0m\033[36m  δ         : Margin - ignorable P₁ - P₂ difference ",
                           "\033[0m\033[36m  OR        : Odds(P₁) / Odds(P₂) ",
                           "\033[0m\033[36m  Odds(P₁) : P₁ / (1 - P₁) ",
                           "\033[0m\033[36m  Odds(P₂) : P₂ / (1 - P₂) ",
                           "\033[0m\033[36m  μ         : Mean of the alternative distribution ",
                           "\033[0m\033[36m  μ₀       : Mean of the null distribution ", "", "\033[0m"))
    expect_equal(crrRes, pwrss.z.2props(p1 = 0.65, p2 = 0.60, alpha = 0.05, n2 = 500, alternative = "less", verbose = FALSE))
    expect_equal(crrRes, pwrss.z.2prop(p1  = 0.65, p2 = 0.60, alpha = 0.05, n2 = 500, alternative = "less", verbose = FALSE))
    expect_equal(crrOut, capture.output(pwrss.z.2props(p1 = 0.65, p2 = 0.60, alpha = 0.05, n2 = 500, alternative = "less")))

    # sample size
    crrRes <- power.z.twoprops(prob1 = 0.65, prob2 = 0.60, alpha = 0.05, power = 0.80, alternative = "one.sided", verbose = 0)
    crrOut <- capture.output(power.z.twoprops(prob1 = 0.65, prob2 = 0.60, alpha = 0.05, power = 0.80, alternative = "one.sided"))
    crrDtl <- capture.output(power.z.twoprops(prob1 = 0.65, prob2 = 0.60, alpha = 0.05, power = 0.80, alternative = "one.sided", verbose = 2))
    crrPty <- capture.output(power.z.twoprops(prob1 = 0.65, prob2 = 0.60, alpha = 0.05, power = 0.80, alternative = "one.sided", pretty = TRUE))
    crrPnD <- capture.output(power.z.twoprops(prob1 = 0.65, prob2 = 0.60, alpha = 0.05, power = 0.80, alternative = "one.sided",
                                              verbose = 2, pretty = TRUE))
    expect_equal(class(crrRes), c("pwrss", "z", "twoprops"))
    expect_equal(names(crrRes),
                 c("parms", "test", "delta", "odds.ratio", "mean", "sd", "null.mean", "null.sd", "z.alpha", "power", "n", "n.total"))
    expect_equal(crrRes[["parms"]],
                 list(prob1 = 0.65, prob2 = 0.6, margin = 0, n.ratio = 1, alpha = 0.05, alternative = "one.sided",
                      arcsine = FALSE, correct = FALSE, paired = FALSE, rho.paired = 0.5, std.error = "pooled",
                      ceiling = TRUE, verbose = 0, pretty = FALSE))
    expect_equal(crrRes[c("test", "delta", "odds.ratio", "mean", "sd", "null.mean", "null.sd", "z.alpha", "power", "n", "n.total")],
                 list(test = "z", delta = 0.05, odds.ratio = 1.23809524, mean = 2.48955035, sd = 1, null.mean = 0, null.sd = 1.001336,
                      z.alpha = 1.64705116, power = 0.8002457, n = c(n1 = 1159, n2 = 1159), n.total = 2318))
    expect_equal(crrOut, c("+--------------------------------------------------+",
                           "|             SAMPLE SIZE CALCULATION              |",
                           "+--------------------------------------------------+", "",
                           "Independent Proportions", "",
                           "  Method          : Normal Approximation", "",
                           "----------------------------------------------------",
                           "Hypotheses",
                           "----------------------------------------------------",
                           "  H0 (Null Claim) : prob1 - prob2 <= 0",
                           "  H1 (Alt. Claim) : prob1 - prob2  > 0", "",
                           "----------------------------------------------------",
                           "Results",
                           "----------------------------------------------------",
                           "  Sample Size          = 1159 and 1159  <<",
                           "  Type 1 Error (alpha) = 0.050",
                           "  Type 2 Error (beta)  = 0.200",
                           "  Statistical Power    = 0.800", ""))
    expect_equal(crrDtl, c("+--------------------------------------------------+",
                           "|             SAMPLE SIZE CALCULATION              |",
                           "+--------------------------------------------------+", "",
                           "Independent Proportions", "",
                           "  Method          : Normal Approximation", "",
                           "----------------------------------------------------",
                           "Hypotheses",
                           "----------------------------------------------------",
                           "  H0 (Null Claim) : prob1 - prob2 <= 0",
                           "  H1 (Alt. Claim) : prob1 - prob2  > 0", "",
                           "----------------------------------------------------",
                           "Key Parameters",
                           "----------------------------------------------------",
                           "  prob1 - prob2        = 0.050",
                           "  Odds Ratio           = 1.238",
                           "  Mean of Alt.         = 2.490",
                           "  Mean of Null         = 0",
                           "  Critical Value       = 1.647", "",
                           "----------------------------------------------------",
                           "Results",
                           "----------------------------------------------------",
                           "  Sample Size          = 1159 and 1159  <<",
                           "  Type 1 Error (alpha) = 0.050",
                           "  Type 2 Error (beta)  = 0.200",
                           "  Statistical Power    = 0.800", "",
                           "----------------------------------------------------",
                           "Definitions",
                           "----------------------------------------------------",
                           "  prob1       : Probability of success in the first group ",
                           "  prob2       : Probability of success in the second group ",
                           "  Odds Ratio  : Odds(prob1) / Odds(prob2) ",
                           "  Odds(prob1) : prob1 / (1 - prob1) ",
                           "  Odds(prob2) : prob2 / (1 - prob2) ", ""))
    expect_equal(crrPty, c("╔══════════════════════════════════════════════════╗",
                           "║           \033[34m SAMPLE SIZE CALCULATION \033[0m              ║",
                           "╚══════════════════════════════════════════════════╝", "",
                           "Independent Proportions", "",
                           "  Method            : Normal Approximation", "",
                           "────────────────────────────────────────────────────",
                           "Hypotheses",
                           "────────────────────────────────────────────────────",
                           "  H₀ (Null)         : P₁ - P₂ ≤ 0 ",
                           "  H₁ (Alternative)  : P₁ - P₂ > 0 ", "",
                           "────────────────────────────────────────────────────",
                           "Results",
                           "────────────────────────────────────────────────────",
                           "  \033[34mSample Size        = 1159 and 1159\033[0m  \033[1;35m◄◄\033[0m",
                           "  Type 1 Error (α)   = 0.050",
                           "  Type 2 Error (β)   = 0.200",
                           "  Statistical Power  = 0.800", ""))
    expect_equal(crrPnD, c("╔══════════════════════════════════════════════════╗",
                           "║           \033[34m SAMPLE SIZE CALCULATION \033[0m              ║",
                           "╚══════════════════════════════════════════════════╝", "",
                           "Independent Proportions", "",
                           "  Method            : Normal Approximation", "",
                           "────────────────────────────────────────────────────",
                           "Hypotheses",
                           "────────────────────────────────────────────────────",
                           "  H₀ (Null)         : P₁ - P₂ ≤ 0 ",
                           "  H₁ (Alternative)  : P₁ - P₂ > 0 ", "",
                           "────────────────────────────────────────────────────",
                           "Key Parameters",
                           "────────────────────────────────────────────────────",
                           "  P₁ - P₂             = 0.050 ",
                           "  Odds Ratio (OR)   = 1.238 ",
                           "  μ                 = 2.490 ",
                           "  μ₀                 = 0 ",
                           "  Z⁻¹(α, μ₀)          = 1.647 ", "",
                           "────────────────────────────────────────────────────",
                           "Results",
                           "────────────────────────────────────────────────────",
                           "  \033[34mSample Size        = 1159 and 1159\033[0m  \033[1;35m◄◄\033[0m",
                           "  Type 1 Error (α)   = 0.050",
                           "  Type 2 Error (β)   = 0.200",
                           "  Statistical Power  = 0.800", "",
                           "\033[36m────────────────────────────────────────────────────\033[0m",
                           "\033[36mDefinitions\033[0m",
                           "\033[36m────────────────────────────────────────────────────\033[0m",
                           "\033[36m  P₁       : Probability of success in the first group ",
                           "\033[0m\033[36m  P₂       : Probability of success in the second group ",
                           "\033[0m\033[36m  δ         : Margin - ignorable P₁ - P₂ difference ",
                           "\033[0m\033[36m  OR        : Odds(P₁) / Odds(P₂) ",
                           "\033[0m\033[36m  Odds(P₁) : P₁ / (1 - P₁) ",
                           "\033[0m\033[36m  Odds(P₂) : P₂ / (1 - P₂) ",
                           "\033[0m\033[36m  μ         : Mean of the alternative distribution ",
                           "\033[0m\033[36m  μ₀       : Mean of the null distribution ", "", "\033[0m"))
    expect_equal(crrRes, pwrss.z.2props(p1 = 0.65, p2 = 0.60, alpha = 0.05, power = 0.80, alternative = "less", verbose = FALSE))
    expect_equal(crrRes, pwrss.z.2prop(p1  = 0.65, p2 = 0.60, alpha = 0.05, power = 0.80, alternative = "less", verbose = FALSE))
    expect_equal(crrOut, capture.output(pwrss.z.2props(p1 = 0.65, p2 = 0.60, alpha = 0.05, power = 0.80, alternative = "less")))

    crrRes <- power.z.twoprops(prob1 = 0.65, prob2 = 0.60, alpha = 0.05, power = 0.80, alternative = "one.sided",
                               arcsine = TRUE, std.error = "unpooled", verbose = 0)
    expect_equal(class(crrRes), c("pwrss", "z", "twoprops"))
    expect_equal(names(crrRes),
                 c("parms", "test", "delta", "odds.ratio", "mean", "sd", "null.mean", "null.sd", "z.alpha", "power", "n", "n.total"))
    expect_equal(crrRes[["parms"]],
                 list(prob1 = 0.65, prob2 = 0.6, margin = 0, n.ratio = 1, alpha = 0.05, alternative = "one.sided",
                      arcsine = TRUE, correct = FALSE, paired = FALSE, rho.paired = 0.5, std.error = "unpooled",
                      ceiling = TRUE, verbose = 0, pretty = FALSE))
    expect_equal(crrRes[c("test", "delta", "odds.ratio", "mean", "sd", "null.mean", "null.sd", "z.alpha", "power", "n", "n.total")],
                 list(test = "z", delta = 0.05, odds.ratio = 1.23809524, mean = 2.48648363, sd = 1, null.mean = 0, null.sd = 1,
                      z.alpha = 1.64485363, power = 0.800002455, n = c(n1 = 1158, n2 = 1158), n.total = 2316))

    crrRes <- power.z.twoprops(prob1 = 0.65, prob2 = 0.60, alpha = 0.05, power = 0.80, alternative = "two.sided", verbose = 0)
    expect_equal(class(crrRes), c("pwrss", "z", "twoprops"))
    expect_equal(names(crrRes),
                 c("parms", "test", "delta", "odds.ratio", "mean", "sd", "null.mean", "null.sd", "z.alpha", "power", "n", "n.total"))
    expect_equal(crrRes[["parms"]],
                 list(prob1 = 0.65, prob2 = 0.6, margin = 0, n.ratio = 1, alpha = 0.05, alternative = "two.sided",
                      arcsine = FALSE, correct = FALSE, paired = FALSE, rho.paired = 0.5, std.error = "pooled",
                      ceiling = TRUE, verbose = 0, pretty = FALSE))
    expect_equal(crrRes[c("test", "delta", "odds.ratio", "mean", "sd", "null.mean", "null.sd", "z.alpha", "power", "n", "n.total")],
                 list(test = "z", delta = 0.05, odds.ratio = 1.23809524, mean = 2.8046943, sd = 1, null.mean = 0, null.sd = 1.001336,
                      z.alpha = 1.96258250806517 * c(-1, 1), power = 0.800138245, n = c(n1 = 1471, n2 = 1471), n.total = 2942))
    expect_equal(crrRes, pwrss.z.2props(p1 = 0.65, p2 = 0.60, alpha = 0.05, power = 0.80, alternative = "not equal", verbose = 0))

    crrRes <- power.z.twoprops(prob1 = 0.65, prob2 = 0.60, alpha = 0.05, power = 0.80, alternative = "two.sided",
                               arcsine = TRUE, verbose = 0)
    expect_equal(class(crrRes), c("pwrss", "z", "twoprops"))
    expect_equal(names(crrRes),
                 c("parms", "test", "delta", "odds.ratio", "mean", "sd", "null.mean", "null.sd", "z.alpha", "power", "n", "n.total"))
    expect_equal(crrRes[["parms"]],
                 list(prob1 = 0.65, prob2 = 0.6, margin = 0, n.ratio = 1, alpha = 0.05, alternative = "two.sided",
                      arcsine = TRUE, correct = FALSE, paired = FALSE, rho.paired = 0.5, std.error = "pooled",
                      ceiling = TRUE, verbose = 0, pretty = FALSE))
    expect_equal(crrRes[c("test", "delta", "odds.ratio", "mean", "sd", "null.mean", "null.sd", "z.alpha", "power", "n", "n.total")],
                 list(test = "z", delta = 0.05, odds.ratio = 1.23809524, mean = 2.80244863, sd = 1, null.mean = 0, null.sd = 1,
                      z.alpha = 1.959964 * c(-1, 1), power = 0.8002426, n = c(n1 = 1471, n2 = 1471), n.total = 2942))

    crrRes <- power.z.twoprops(prob1 = 0.65, prob2 = 0.60, alpha = 0.05, power = 0.80, alternative = "two.sided",
                               correct = TRUE, verbose = 0)
    expect_equal(class(crrRes), c("pwrss", "z", "twoprops"))
    expect_equal(names(crrRes),
                 c("parms", "test", "delta", "odds.ratio", "mean", "sd", "null.mean", "null.sd", "z.alpha", "power", "n", "n.total"))
    expect_equal(crrRes[["parms"]],
                 list(prob1 = 0.65, prob2 = 0.6, margin = 0, n.ratio = 1, alpha = 0.05, alternative = "two.sided",
                      arcsine = FALSE, correct = TRUE, paired = FALSE, rho.paired = 0.5, std.error = "pooled",
                      ceiling = TRUE, verbose = 0, pretty = FALSE))
    expect_equal(crrRes[c("test", "delta", "odds.ratio", "mean", "sd", "null.mean", "null.sd", "z.alpha", "power", "n", "n.total")],
                 list(test = "z", delta = 0.05, odds.ratio = 1.23809524, mean = 2.8049467, sd = 1, null.mean = 0, null.sd = 1.001336,
                      z.alpha = 1.96258250806517 * c(-1, 1), power = 0.80020886, n = c(n1 = 1511, n2 = 1511), n.total = 3022))

    crrRes <- power.z.twoprops(prob1 = 0.65, prob2 = 0.60, alpha = 0.05, power = 0.80, alternative = "two.sided",
                               std.error = "unpooled", verbose = 0)
    expect_equal(class(crrRes), c("pwrss", "z", "twoprops"))
    expect_equal(names(crrRes),
                 c("parms", "test", "delta", "odds.ratio", "mean", "sd", "null.mean", "null.sd", "z.alpha", "power", "n", "n.total"))
    expect_equal(crrRes[["parms"]],
                 list(prob1 = 0.65, prob2 = 0.6, margin = 0, n.ratio = 1, alpha = 0.05, alternative = "two.sided",
                      arcsine = FALSE, correct = FALSE, paired = FALSE, rho.paired = 0.5, std.error = "unpooled",
                      ceiling = TRUE, verbose = 0, pretty = FALSE))
    expect_equal(crrRes[c("test", "delta", "odds.ratio", "mean", "sd", "null.mean", "null.sd", "z.alpha", "power", "n", "n.total")],
                 list(test = "z", delta = 0.05, odds.ratio = 1.23809524, mean = 2.80183286, sd = 1, null.mean = 0, null.sd = 1,
                      z.alpha = 1.959964 * c(-1, 1), power = 0.80007028, n = c(n1 = 1468, n2 = 1468), n.total = 2936))

    crrRes <- power.z.twoprops(prob1 = 0.65, prob2 = 0.60, alpha = 0.05, power = 0.80, alternative = "two.sided",
                               arcsine = TRUE, std.error = "unpooled", verbose = 0)
    expect_equal(class(crrRes), c("pwrss", "z", "twoprops"))
    expect_equal(names(crrRes),
                 c("parms", "test", "delta", "odds.ratio", "mean", "sd", "null.mean", "null.sd", "z.alpha", "power", "n", "n.total"))
    expect_equal(crrRes[["parms"]],
                 list(prob1 = 0.65, prob2 = 0.6, margin = 0, n.ratio = 1, alpha = 0.05, alternative = "two.sided",
                      arcsine = TRUE, correct = FALSE, paired = FALSE, rho.paired = 0.5, std.error = "unpooled",
                      ceiling = TRUE, verbose = 0, pretty = FALSE))
    expect_equal(crrRes[c("test", "delta", "odds.ratio", "mean", "sd", "null.mean", "null.sd", "z.alpha", "power", "n", "n.total")],
                 list(test = "z", delta = 0.05, odds.ratio = 1.23809524, mean = 2.8024486, sd = 1, null.mean = 0, null.sd = 1,
                      z.alpha = 1.959964 * c(-1, 1), power = 0.8002426, n = c(n1 = 1471, n2 = 1471), n.total = 2942))

    crrRes <- power.z.twoprops(prob1 = 0.65, prob2 = 0.60, margin = c(0, 0.10), alpha = 0.05, power = 0.80,
                               alternative = "two.one.sided", verbose = 0)
    expect_equal(class(crrRes), c("pwrss", "z", "twoprops"))
    expect_equal(names(crrRes),
                 c("parms", "test", "delta", "odds.ratio", "mean", "sd", "null.mean", "null.sd", "z.alpha", "power", "n", "n.total"))
    expect_equal(crrRes[["parms"]],
                 list(prob1 = 0.65, prob2 = 0.6, margin = c(0, 0.10), n.ratio = 1, alpha = 0.05, alternative = "two.one.sided",
                      arcsine = FALSE, correct = FALSE, paired = FALSE, rho.paired = 0.5, std.error = "pooled",
                      ceiling = TRUE, verbose = 0, pretty = FALSE))
    expect_equal(crrRes[c("test", "delta", "odds.ratio", "mean", "sd", "null.mean", "null.sd", "z.alpha", "power", "n", "n.total")],
                 list(test = "z", delta = 0.05, odds.ratio = 1.23809524, mean = 2.92874378, sd = 1, null.mean = c(0, 5.85748755251),
                      null.sd = 1.001336, z.alpha = c(1.64705116, 4.21043639), power = 0.80004950, n = c(n1 = 1604, n2 = 1604),
                      n.total = 3208))

    crrRes <- power.z.twoprops(prob1 = 0.65, prob2 = 0.60, margin = c(0.10, 0.20), alpha = 0.05, power = 0.80,
                               alternative = "two.one.sided", verbose = 0)
    expect_equal(class(crrRes), c("pwrss", "z", "twoprops"))
    expect_equal(names(crrRes),
                 c("parms", "test", "delta", "odds.ratio", "mean", "sd", "null.mean", "null.sd", "z.alpha", "power", "n", "n.total"))
    expect_equal(crrRes[["parms"]],
                 list(prob1 = 0.65, prob2 = 0.6, margin = c(0.10, 0.20), n.ratio = 1, alpha = 0.05, alternative = "two.one.sided",
                      arcsine = FALSE, correct = FALSE, paired = FALSE, rho.paired = 0.5, std.error = "pooled",
                      ceiling = TRUE, verbose = 0, pretty = FALSE))
    expect_equal(crrRes[c("test", "delta", "odds.ratio", "mean", "sd", "null.mean", "null.sd", "z.alpha", "power", "n", "n.total")],
                 list(test = "z", delta = 0.05, odds.ratio = 1.23809524, mean = 2.8046943, sd = 1,
                      null.mean = c(5.60938862, 11.21877723), null.sd = 1.001336, z.alpha = c(3.64680611, 13.18135974),
                      power = 0.80013731, n = c(n1 = 1471, n2 = 1471), n.total = 2942))

    crrRes <- pwrss.z.2props(p1 = 0.65, p2 = 0.60, margin = 0.10, alpha = 0.05, power = 0.80, alternative = "equivalent", verbose = 0)
    expect_equal(class(crrRes), c("pwrss", "z", "twoprops"))
    expect_equal(names(crrRes),
                 c("parms", "test", "delta", "odds.ratio", "mean", "sd", "null.mean", "null.sd", "z.alpha", "power", "n", "n.total"))
    expect_equal(crrRes[["parms"]],
                 list(prob1 = 0.65, prob2 = 0.6, margin = c(-0.10, 0.10), n.ratio = 1, alpha = 0.05, alternative = "two.one.sided",
                      arcsine = FALSE, correct = FALSE, paired = FALSE, rho.paired = 0.5, std.error = "pooled",
                      ceiling = TRUE, verbose = 0, pretty = FALSE))
    expect_equal(crrRes[c("test", "delta", "odds.ratio", "mean", "sd", "null.mean", "null.sd", "z.alpha", "power", "n", "n.total")],
                 list(test = "z", delta = 0.05, odds.ratio = 1.23809524, mean = 2.48955035, sd = 1,
                      null.mean = 4.97910071 * c(-1, 1), null.sd = 1.001336, z.alpha = 3.33204955 * c(-1, 1),
                      power = 0.8002457, n = c(n1 = 1159, n2 = 1159), n.total = 2318))

    expect_error(power.z.twoprops(prob1 = 0.65, prob2 = 0.60, margin = 1, n2 = 500, alternative = "one.sided"),
                 "Provide a reasonable `margin` consistent with `prob1` - `prob2`.")
    expect_error(power.z.twoprops(prob1 = 0.65, prob2 = 0.60, margin = 0.05, n2 = 500, alternative = "one.sided"),
                 "The value of margin should be different from the prob1 - prob2 difference.")
    expect_error(power.z.twoprops(prob1 = 0.65, prob2 = 0.60, n2 = 500, alternative = "one.sided", correct = TRUE, paired = TRUE),
                 "Continuity correction is currently not available for paired proportions.")
    expect_error(power.z.twoprops(prob1 = 0.65, prob2 = 0.60, n2 = 500, alternative = "one.sided", arcsine = TRUE, correct = TRUE),
                 "Continuity correction does not apply to arcsine transformation approach.")
    expect_error(power.z.twoprops(prob1 = 0.65, prob2 = 0.60, n2 = 500, alternative = "one.sided", arcsine = TRUE, paired = TRUE),
                 "Arcsine transformation is currently not available for paired proportions.")
    expect_error(power.z.twoprops(prob1 = 0.65, prob2 = 0.60, margin = 0.1, n2 = 500, alternative = "one.sided", arcsine = TRUE),
                 "Arcsine transformation is currently not available for non-zero null.")
    expect_error(power.z.twoprops(prob1 = 0.65, prob2 = 0.60, n2 = 500, alternative = "two.one.sided", paired = TRUE),
                 "Two one-sided tests are currently not available for paired proportions.")
    expect_error(power.z.twoprops(prob1 = 0.65, prob2 = 0.60, n2 = 500, alternative = "two.one.sided", arcsine = TRUE),
                 "Arcsine transformation is currently not available for two one-sided tests.")
    expect_error(power.z.twoprops(prob1 = 0.65, prob2 = 0.60, margin = 0.10, n2 = 500, alternative = "two.one.sided"),
                 "Provide margins in the form of margin = c\\(lower, upper\\).")
    expect_warning(power.z.twoprops(prob1 = 0.65, prob2 = 0.60, margin = 0.10, alpha = 0.05, power = 0.80,
                                    alternative = "one.sided", paired = TRUE, verbose = 0),
                   "`margin` argument is ignored.")
})
