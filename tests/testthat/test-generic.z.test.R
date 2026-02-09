test_that("generic.z.test.R works", {
    # power.z.test -----------------------------------------------------------------------------------------------------
    crrOut <- capture.output(power.z.test(mean = 1.96, alpha = 0.05, alternative = "two.sided", plot = FALSE))
    crrDtl <- capture.output(power.z.test(mean = 1.96, alpha = 0.05, alternative = "two.sided", plot = FALSE, verbose = 2))
    crrPty <- capture.output(power.z.test(mean = 1.96, alpha = 0.05, alternative = "two.sided", plot = FALSE, pretty = TRUE))
    crrPnD <- capture.output(power.z.test(mean = 1.96, alpha = 0.05, alternative = "two.sided", plot = FALSE, verbose = 2, pretty = TRUE))
    expect_equal(power.z.test(mean = 1.96, alpha = 0.05, alternative = "two.sided", plot = FALSE, verbose = 0),
                 list(alternative = "two.sided", mean = 1.96, sd = 1, null.mean = 0, null.sd = 1, alpha = 0.05,
                      z.alpha = 1.959964 * c(-1, 1), power = 0.500058649))
    expect_equal(crrOut, c("+--------------------------------------------------+",
                           "|                POWER CALCULATION                 |",
                           "+--------------------------------------------------+", "",
                           "Generic z-Test", "",
                           "----------------------------------------------------",
                           "Hypotheses",
                           "----------------------------------------------------",
                           "  H0 (Null Claim) : mean  = null.mean",
                           "  H1 (Alt. Claim) : mean != null.mean", "",
                           "----------------------------------------------------",
                           "Results",
                           "----------------------------------------------------",
                           "  Type 1 Error (alpha) = 0.050",
                           "  Type 2 Error (beta)  = 0.500",
                           "  Statistical Power    = 0.500  <<", ""))
    expect_equal(crrDtl, c("+--------------------------------------------------+",
                           "|                POWER CALCULATION                 |",
                           "+--------------------------------------------------+", "",
                           "Generic z-Test", "",
                           "----------------------------------------------------",
                           "Hypotheses",
                           "----------------------------------------------------",
                           "  H0 (Null Claim) : mean  = null.mean",
                           "  H1 (Alt. Claim) : mean != null.mean", "",
                           "----------------------------------------------------",
                           "Key Parameters",
                           "----------------------------------------------------",
                           "  Mean of Alt.         = 1.960",
                           "  Mean of Null         = 0",
                           "  Critical Value       = -1.96 and 1.96", "",
                           "----------------------------------------------------",
                           "Results",
                           "----------------------------------------------------",
                           "  Type 1 Error (alpha) = 0.050",
                           "  Type 2 Error (beta)  = 0.500",
                           "  Statistical Power    = 0.500  <<", "",
                           "----------------------------------------------------",
                           "Definitions",
                           "----------------------------------------------------",
                           "  mean      : Mean of alt. ",
                           "  null.mean : Mean of null ", ""))
    expect_equal(crrPty, c("╔══════════════════════════════════════════════════╗",
                           "║               \033[34m POWER CALCULATION \033[0m                ║",
                           "╚══════════════════════════════════════════════════╝", "",
                           "Generic z-Test", "",
                           "────────────────────────────────────────────────────",
                           "Hypotheses",
                           "────────────────────────────────────────────────────",
                           "  H₀ (Null)         : μ = μ₀ ",
                           "  H₁ (Alternative)  : μ ≠ μ₀ ", "",
                           "────────────────────────────────────────────────────",
                           "Results",
                           "────────────────────────────────────────────────────",
                           "  Type 1 Error (α)   = 0.050",
                           "  Type 2 Error (β)   = 0.500",
                           "  \033[34mStatistical Power  = 0.500\033[0m  \033[1;35m◄◄\033[0m", ""))
    expect_equal(crrPnD, c("╔══════════════════════════════════════════════════╗",
                           "║               \033[34m POWER CALCULATION \033[0m                ║",
                           "╚══════════════════════════════════════════════════╝", "",
                           "Generic z-Test", "",
                           "────────────────────────────────────────────────────",
                           "Hypotheses",
                           "────────────────────────────────────────────────────",
                           "  H₀ (Null)         : μ = μ₀ ",
                           "  H₁ (Alternative)  : μ ≠ μ₀ ", "",
                           "────────────────────────────────────────────────────",
                           "Key Parameters",
                           "────────────────────────────────────────────────────",
                           "  μ                 = 1.960 ",
                           "  μ₀                 = 0 ",
                           "  Z⁻¹(α, μ₀)          = -1.96 and 1.96 ", "",
                           "────────────────────────────────────────────────────",
                           "Results",
                           "────────────────────────────────────────────────────",
                           "  Type 1 Error (α)   = 0.050",
                           "  Type 2 Error (β)   = 0.500",
                           "  \033[34mStatistical Power  = 0.500\033[0m  \033[1;35m◄◄\033[0m", "",
                           "\033[36m────────────────────────────────────────────────────\033[0m",
                           "\033[36mDefinitions\033[0m",
                           "\033[36m────────────────────────────────────────────────────\033[0m",
                           "\033[36m  μ   : Mean under alternative",
                           "\033[0m\033[36m  μ₀ : Mean under null", "", "\033[0m"))
     
    expect_equal(power.z.test(mean = 1.96, alpha = 0.05, alternative = "one.sided", plot = FALSE, verbose = 0),
                 list(alternative = "one.sided", mean = 1.96, sd = 1, null.mean = 0, null.sd = 1, alpha = 0.05,
                      z.alpha = 1.64485363, power = 0.62367474))
     
    expect_equal(power.z.test(mean = 0, null.mean = c(-2, 2), alpha = 0.05, alternative = "two.one.sided", plot = FALSE, verbose = 0),
                 list(alternative = "two.one.sided", mean = 0, sd = 1, null.mean = c(-2, 2), null.sd = 1, alpha = 0.05,
                      z.alpha = 0.355146373 * c(-1, 1), power = 0.277520063))

    expect_equal(power.z.test(mean = 0, null.mean = 2, alpha = 0.05, alternative = "two.one.sided", plot = FALSE, verbose = 0),
                 list(alternative = "two.one.sided", mean = 0, sd = 1, null.mean = c(-2, 2), null.sd = 1, alpha = 0.05,
                      z.alpha = 0.355146373 * c(-1, 1), power = 0.277520063))
     
    expect_equal(power.z.test(mean = 2, null.mean = c(-1, 1), alpha = 0.05, alternative = "two.one.sided", plot = FALSE, verbose = 0),
                 list(alternative = "two.one.sided", mean = 2, sd = 1, null.mean = c(-1, 1), null.sd = 1, alpha = 0.05,
                      z.alpha = 2.959964 * c(-1, 1), power = 0.168537023))

    expect_equal(power.z.test(mean = 2, null.mean = -1, alpha = 0.05, alternative = "two.one.sided", plot = FALSE, verbose = 0),
                 list(alternative = "two.one.sided", mean = 2, sd = 1, null.mean = c(-1, 1), null.sd = 1, alpha = 0.05,
                      z.alpha = 2.959964 * c(-1, 1), power = 0.168537023))
     
    expect_error(power.z.test(mean = 2, null.mean = c(-1, 1), alpha = 0.05, alternative = "one.sided", plot = FALSE, verbose = 0),
                 "If `alternative` is \"two.sided\" or \"one.sided\", `null.mean` must be of length one.")
    expect_error(power.z.test(mean = 2, null.mean = c(-1, 1, 1), alpha = 0.05, alternative = "two.one.sided", plot = FALSE, verbose = 0),
                 paste("If `alternative` is \"two.one.sided\", `null.mean` must be of length one \\(absolute value,",
                       "that must be different from 0\\), or length two \\(lower and upper bounds)."))
    expect_error(power.z.test(mean = 2, alpha = 0.05, alternative = "two.one.sided", plot = FALSE, verbose = 0),
                 paste("If `alternative` is \"two.one.sided\", `null.mean` must be of length one \\(absolute value,",
                       "that must be different from 0\\), or length two \\(lower and upper bounds\\)."))
})
