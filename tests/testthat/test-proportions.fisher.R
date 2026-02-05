test_that("proportions.fisher.R works", {
    # power.exact.fisher -----------------------------------------------------------------------------------------------
    crrRes <- power.exact.fisher(prob1 = 0.60, prob2 = 0.40, n2 = 50, verbose = 0)
    crrOut <- capture.output(power.exact.fisher(prob1 = 0.60, prob2 = 0.40, n2 = 50))
    crrDtl <- capture.output(power.exact.fisher(prob1 = 0.60, prob2 = 0.40, n2 = 50, verbose = 2))
    crrPty <- capture.output(power.exact.fisher(prob1 = 0.60, prob2 = 0.40, n2 = 50, pretty = TRUE))
    crrPnD <- capture.output(power.exact.fisher(prob1 = 0.60, prob2 = 0.40, n2 = 50, verbose = 2, pretty = TRUE))
    expect_equal(class(crrRes), c("pwrss", "exact", "fisher"))
    expect_equal(names(crrRes), c("parms", "test", "delta", "odds.ratio", "mean", "sd", "null.mean", "null.sd",
                                  "alternative", "z.alpha", "power", "n", "n.total"))
    expect_equal(crrRes[["parms"]],
                 list(prob1 = 0.6, prob2 = 0.4, n.ratio = 1, alpha = 0.05, alternative = "two.sided", method = "exact",
                      ceiling = TRUE, verbose = 0, pretty = FALSE))
    expect_equal(crrRes[c("test", "delta", "odds.ratio", "mean", "sd", "null.mean", "null.sd", "alternative", "z.alpha",
                          "power", "n", "n.total")],
                 list(test = "exact", delta = 0.2, odds.ratio = 2.25, mean = NA, sd = NA, null.mean = NA, null.sd = NA,
                      alternative = "two.sided", z.alpha = NA, power = 0.462100133, n = c(n1 = 50, n2 = 50), n.total = 100))
    expect_equal(crrOut, c("+--------------------------------------------------+",
                           "|                POWER CALCULATION                 |",
                           "+--------------------------------------------------+", "",
                           "Independent Proportions", "",
                           "  Method          : Fisher's Exact", "",
                           "----------------------------------------------------",
                           "Hypotheses",
                           "----------------------------------------------------",
                           "  H0 (Null Claim) : prob1 - prob2 = 0 ",
                           "  H1 (Alt. Claim) : prob1 - prob2 != 0 ", "",
                           "----------------------------------------------------",
                           "Results",
                           "----------------------------------------------------",
                           "  Sample Size          = 50 and 50",
                           "  Type 1 Error (alpha) = 0.050",
                           "  Type 2 Error (beta)  = 0.538",
                           "  Statistical Power    = 0.462  <<", ""))
    expect_equal(crrDtl, c("+--------------------------------------------------+",
                           "|                POWER CALCULATION                 |",
                           "+--------------------------------------------------+", "",
                           "Independent Proportions", "",
                           "  Method          : Fisher's Exact", "",
                           "----------------------------------------------------",
                           "Hypotheses",
                           "----------------------------------------------------",
                           "  H0 (Null Claim) : prob1 - prob2 = 0 ",
                           "  H1 (Alt. Claim) : prob1 - prob2 != 0 ", "",
                           "----------------------------------------------------",
                           "Key Parameters",
                           "----------------------------------------------------",
                           "  prob1 - prob2        = 0.200",
                           "  Odds Ratio           = 2.250", "",
                           "----------------------------------------------------",
                           "Results",
                           "----------------------------------------------------",
                           "  Sample Size          = 50 and 50",
                           "  Type 1 Error (alpha) = 0.050",
                           "  Type 2 Error (beta)  = 0.538",
                           "  Statistical Power    = 0.462  <<", "",
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
                           "  H₀ (Null)         : P₁ - P₂ = 0 ",
                           "  H₁ (Alternative)  : P₁ - P₂ ≠ 0 ", "",
                           "────────────────────────────────────────────────────",
                           "Results",
                           "────────────────────────────────────────────────────",
                           "  Sample Size       = 50 and 50",
                           "  Type 1 Error (α)  = 0.050",
                           "  Type 2 Error (β)  = 0.538",
                           "  \033[34mStatistical Power = 0.462\033[0m  \033[1;35m◄◄\033[0m", ""))
    expect_equal(crrPnD, c("╔══════════════════════════════════════════════════╗",
                           "║               \033[34m POWER CALCULATION \033[0m                ║",
                           "╚══════════════════════════════════════════════════╝", "",
                           "Independent Proportions", "",
                           "  Method            : Fisher's Exact", "",
                           "────────────────────────────────────────────────────",
                           "Hypotheses",
                           "────────────────────────────────────────────────────",
                           "  H₀ (Null)         : P₁ - P₂ = 0 ",
                           "  H₁ (Alternative)  : P₁ - P₂ ≠ 0 ", "",
                           "────────────────────────────────────────────────────",
                           "Key Parameters",
                           "────────────────────────────────────────────────────",
                           "  P₁ - P₂             = 0.200 ",
                           "  Odds Ratio (OR)   = 2.250 ", "",
                           "────────────────────────────────────────────────────",
                           "Results",
                           "────────────────────────────────────────────────────",
                           "  Sample Size       = 50 and 50",
                           "  Type 1 Error (α)  = 0.050",
                           "  Type 2 Error (β)  = 0.538",
                           "  \033[34mStatistical Power = 0.462\033[0m  \033[1;35m◄◄\033[0m", "",
                           "\033[36m────────────────────────────────────────────────────\033[0m",
                           "\033[36mDefinitions\033[0m",
                           "\033[36m────────────────────────────────────────────────────\033[0m",
                           "\033[36m  P₁       : Probability of success in the first group ",
                           "\033[0m\033[36m  P₂       : Probability of success in the second group ",
                           "\033[0m\033[36m  δ         : Margin - ignorable P₁ - P₂ difference ",
                           "\033[0m\033[36m  OR        : Odds(P₁) / Odds(P₂) ",
                           "\033[0m\033[36m  Odds(P₁) : P₁ / (1 - P₁) ",
                           "\033[0m\033[36m  Odds(P₂) : P₂ / (1 - P₂) ", "", "\033[0m"))
    expect_equal(crrOut, capture.output(power.exact.fisher(prob1 = 0.60, prob2 = 0.40, n2 = 50, verbose = "J")))

    crrRes <- power.exact.fisher(prob1 = 0.60, prob2 = 0.40, power = 0.4621, verbose = 0)
    expect_equal(class(crrRes), c("pwrss", "exact", "fisher"))
    expect_equal(names(crrRes), c("parms", "test", "delta", "odds.ratio", "mean", "sd", "null.mean", "null.sd",
                                  "alternative", "z.alpha", "power", "n", "n.total"))
    expect_equal(crrRes[["parms"]],
                 list(prob1 = 0.6, prob2 = 0.4, n.ratio = 1, alpha = 0.05, alternative = "two.sided", method = "exact",
                      ceiling = TRUE, verbose = 0, pretty = FALSE))
    expect_equal(crrRes[c("test", "delta", "odds.ratio", "mean", "sd", "null.mean", "null.sd", "alternative", "z.alpha",
                          "power", "n", "n.total")],
                 list(test = "exact", delta = 0.2, odds.ratio = 2.25, mean = NA, sd = NA, null.mean = NA, null.sd = NA,
                      alternative = "two.sided", z.alpha = NA, power = 0.462100133, n = c(n1 = 50, n2 = 50), n.total = 100))

    crrRes <- power.exact.fisher(prob1 = 0.60, prob2 = 0.40, power = 0.80, verbose = 0)
    expect_equal(class(crrRes), c("pwrss", "exact", "fisher"))
    expect_equal(names(crrRes), c("parms", "test", "delta", "odds.ratio", "mean", "sd", "null.mean", "null.sd",
                                  "alternative", "z.alpha", "power", "n", "n.total"))
    expect_equal(crrRes[["parms"]],
                 list(prob1 = 0.6, prob2 = 0.4, n.ratio = 1, alpha = 0.05, alternative = "two.sided", method = "exact",
                      ceiling = TRUE, verbose = 0, pretty = FALSE))
    expect_equal(crrRes[c("test", "delta", "odds.ratio", "mean", "sd", "null.mean", "null.sd", "alternative", "z.alpha",
                          "power", "n", "n.total")],
                 list(test = "exact", delta = 0.2, odds.ratio = 2.25, mean = NA, sd = NA, null.mean = NA, null.sd = NA,
                      alternative = "two.sided", z.alpha = NA, power = 0.80194182, n = c(n1 = 108, n2 = 108), n.total = 216))

    crrRes <- power.exact.fisher(prob1 = 0.60, prob2 = 0.40, alternative = "one.sided", power = 0.80, verbose = 0)
    expect_equal(class(crrRes), c("pwrss", "exact", "fisher"))
    expect_equal(names(crrRes), c("parms", "test", "delta", "odds.ratio", "mean", "sd", "null.mean", "null.sd",
                                  "alternative", "z.alpha", "power", "n", "n.total"))
    expect_equal(crrRes[["parms"]],
                 list(prob1 = 0.6, prob2 = 0.4, n.ratio = 1, alpha = 0.05, alternative = "one.sided", method = "exact",
                      ceiling = TRUE, verbose = 0, pretty = FALSE))
    expect_equal(crrRes[c("test", "delta", "odds.ratio", "mean", "sd", "null.mean", "null.sd", "alternative", "z.alpha",
                          "power", "n", "n.total")],
                 list(test = "exact", delta = 0.2, odds.ratio = 2.25, mean = NA, sd = NA, null.mean = NA, null.sd = NA,
                      alternative = "one.sided", z.alpha = NA, power = 0.805746021, n = c(n1 = 85, n2 = 85), n.total = 170))

    crrRes <- power.exact.fisher(prob1 = 0.60, prob2 = 0.40, method = "approximate", power = 0.80, verbose = 0)
    expect_equal(class(crrRes), c("pwrss", "z", "twoprops"))
    expect_equal(names(crrRes), c("parms", "test", "delta", "odds.ratio", "mean", "sd", "null.mean", "null.sd",
                                  "alternative", "z.alpha", "power", "n", "n.total"))
    expect_equal(crrRes[["parms"]],
                 list(prob1 = 0.6, prob2 = 0.4, n.ratio = 1, alpha = 0.05, alternative = "two.sided", method = "approximate",
                      ceiling = TRUE, verbose = 0, pretty = FALSE))
    expect_equal(crrRes[c("test", "delta", "odds.ratio", "mean", "sd", "null.mean", "null.sd", "alternative", "z.alpha",
                          "power", "n", "n.total")],
                 list(test = "z", delta = 0.2, odds.ratio = 2.25, mean = NULL, sd = NULL, null.mean = NULL, null.sd = NULL,
                      alternative = "two.sided", z.alpha = c(-1.95996398454, 1.95996398454), power = 0.8033634,
                      n = c(n1 = 95, n2 = 95), n.total = 190))

    crrRes <- power.exact.fisher(prob1 = 0.60, prob2 = 0.40, method = "approximate", n2 = 95, verbose = 0)
    expect_equal(class(crrRes), c("pwrss", "z", "twoprops"))
    expect_equal(names(crrRes), c("parms", "test", "delta", "odds.ratio", "mean", "sd", "null.mean", "null.sd",
                                  "alternative", "z.alpha", "power", "n", "n.total"))
    expect_equal(crrRes[["parms"]],
                 list(prob1 = 0.6, prob2 = 0.4, n.ratio = 1, alpha = 0.05, alternative = "two.sided", method = "approximate",
                      ceiling = TRUE, verbose = 0, pretty = FALSE))
    expect_equal(crrRes[c("test", "delta", "odds.ratio", "mean", "sd", "null.mean", "null.sd", "alternative", "z.alpha",
                          "power", "n", "n.total")],
                 list(test = "z", delta = 0.2, odds.ratio = 2.25, mean = NULL, sd = NULL, null.mean = NULL, null.sd = NULL,
                      alternative = "two.sided", z.alpha = c(-1.95996398454, 1.95996398454), power = 0.8033634,
                      n = c(n1 = 95, n2 = 95), n.total = 190))

#    crrRes <- power.exact.fisher(prob1 = 0.55, prob2 = 0.45, power = 0.8, verbose = 0)
#    expect_equal(class(crrRes), c("pwrss", "exact", "fisher"))
#    expect_equal(names(crrRes), c("parms", "test", "delta", "odds.ratio", "mean", "sd", "null.mean", "null.sd",
#                                  "alternative", "z.alpha", "power", "n", "n.total"))
#    expect_equal(crrRes[["parms"]],
#                 list(prob1 = 0.55, prob2 = 0.45, n.ratio = 1, alpha = 0.05, alternative = "two.sided", method = "exact",
#                      ceiling = TRUE, verbose = 0, pretty = FALSE))
#    expect_equal(crrRes[c("test", "delta", "odds.ratio", "mean", "sd", "null.mean", "null.sd", "alternative", "z.alpha",
#                          "power", "n", "n.total")],
#                 list(test = "exact", delta = 0.1, odds.ratio = 1.49382716, mean = NA, sd = NA, null.mean = NA, null.sd = NA,
#                      alternative = "two.sided", z.alpha = NA, power = 0.800534276, n = c(n1 = 416, n2 = 416), n.total = 832))

    crrRes <- power.exact.fisher(prob1 = 2 / 3, prob2 = 1 / 3, power = 0.8, verbose = 0)
    expect_equal(class(crrRes), c("pwrss", "exact", "fisher"))
    expect_equal(names(crrRes), c("parms", "test", "delta", "odds.ratio", "mean", "sd", "null.mean", "null.sd",
                                  "alternative", "z.alpha", "power", "n", "n.total"))
    expect_equal(crrRes[["parms"]],
                 list(prob1 = 2 / 3, prob2 = 1 / 3, n.ratio = 1, alpha = 0.05, alternative = "two.sided", method = "exact",
                      ceiling = TRUE, verbose = 0, pretty = FALSE))
    expect_equal(crrRes[c("test", "delta", "odds.ratio", "mean", "sd", "null.mean", "null.sd", "alternative", "z.alpha",
                          "power", "n", "n.total")],
                 list(test = "exact", delta = 1 / 3, odds.ratio = 4, mean = NA, sd = NA, null.mean = NA, null.sd = NA,
                      alternative = "two.sided", z.alpha = NA, power = 0.8008238, n = c(n1 = 39, n2 = 39), n.total = 78))

    crrRes <- power.exact.fisher(prob1 = 0.75, prob2 = 0.25, power = 0.8, verbose = 0)
    expect_equal(class(crrRes), c("pwrss", "exact", "fisher"))
    expect_equal(names(crrRes), c("parms", "test", "delta", "odds.ratio", "mean", "sd", "null.mean", "null.sd",
                                  "alternative", "z.alpha", "power", "n", "n.total"))
    expect_equal(crrRes[["parms"]],
                 list(prob1 = 0.75, prob2 = 0.25, n.ratio = 1, alpha = 0.05, alternative = "two.sided", method = "exact",
                      ceiling = TRUE, verbose = 0, pretty = FALSE))
    expect_equal(crrRes[c("test", "delta", "odds.ratio", "mean", "sd", "null.mean", "null.sd", "alternative", "z.alpha",
                          "power", "n", "n.total")],
                 list(test = "exact", delta = 0.5, odds.ratio = 9, mean = NA, sd = NA, null.mean = NA, null.sd = NA,
                      alternative = "two.sided", z.alpha = NA, power = 0.833005149, n = c(n1 = 18, n2 = 18), n.total = 36))

    expect_error(power.exact.fisher(prob1 = 0.60, prob2 = 0.40, n2 = 2001, verbose = 0),
                 "Consider `method` = 'approximate' for total sample size > 4000")
    expect_error(power.exact.fisher(prob1 = 0.51, prob2 = 0.49, power = 0.80, verbose = 0),
                 "Consider `method` = 'approximate' for total sample size > 1000")
})
