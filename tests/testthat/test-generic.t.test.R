test_that("generic.t.test.R works", {
    # power.t.test -----------------------------------------------------------------------------------------------------
    crrOut <- capture.output(power.t.test(ncp = 1.96, df = 100, alpha = 0.05, alternative = "two.sided", plot = FALSE))
    crrDtl <- capture.output(power.t.test(ncp = 1.96, df = 100, alpha = 0.05, alternative = "two.sided", plot = FALSE, verbose = 2))
    crrPty <- capture.output(power.t.test(ncp = 1.96, df = 100, alpha = 0.05, alternative = "two.sided", plot = FALSE, pretty = TRUE))
    crrPnD <- capture.output(power.t.test(ncp = 1.96, df = 100, alpha = 0.05, alternative = "two.sided", plot = FALSE, verbose = 2, pretty = TRUE))
    expect_equal(power.t.test(ncp = 1.96, df = 100, alpha = 0.05, alternative = "two.sided", plot = FALSE, verbose = 0),
                 list(alternative = "two.sided", ncp = 1.96, null.ncp = 0, df = 100, alpha = 0.05, t.alpha = 1.98397152 * c(-1, 1),
                      power = 0.49254726))
    expect_equal(crrOut, c("+--------------------------------------------------+",
                           "|                POWER CALCULATION                 |",
                           "+--------------------------------------------------+", "",
                           "Generic t-Test", "",
                           "----------------------------------------------------",
                           "Hypotheses",
                           "----------------------------------------------------",
                           "  H0 (Null Claim) : ncp  = null.ncp",
                           "  H1 (Alt. Claim) : ncp != null.ncp", "",
                           "----------------------------------------------------",
                           "Results",
                           "----------------------------------------------------",
                           "  Type 1 Error (alpha) = 0.050",
                           "  Type 2 Error (beta)  = 0.507",
                           "  Statistical Power    = 0.493  <<", ""))
    expect_equal(crrDtl, c("+--------------------------------------------------+",
                           "|                POWER CALCULATION                 |",
                           "+--------------------------------------------------+", "",
                           "Generic t-Test", "",
                           "----------------------------------------------------",
                           "Hypotheses",
                           "----------------------------------------------------",
                           "  H0 (Null Claim) : ncp  = null.ncp",
                           "  H1 (Alt. Claim) : ncp != null.ncp", "",
                           "----------------------------------------------------",
                           "Key Parameters",
                           "----------------------------------------------------",
                           "  Degrees of Freedom     = 100",
                           "  Non-centrality of Alt. = 1.960",
                           "  Non-centrality of Null = 0",
                           "  Critical Value         = -1.984 and 1.984", "",
                           "----------------------------------------------------",
                           "Results",
                           "----------------------------------------------------",
                           "  Type 1 Error (alpha) = 0.050",
                           "  Type 2 Error (beta)  = 0.507",
                           "  Statistical Power    = 0.493  <<", "",
                           "----------------------------------------------------",
                           "Definitions",
                           "----------------------------------------------------",
                           "  ncp      : Non-centrality parameter of Alt. ",
                           "  null.ncp : Non-centrality parameter of Null ", ""))
    expect_equal(crrPty, c("╔══════════════════════════════════════════════════╗",
                           "║               \033[34m POWER CALCULATION \033[0m                ║",
                           "╚══════════════════════════════════════════════════╝", "",
                           "Generic t-Test", "",
                           "────────────────────────────────────────────────────",
                           "Hypotheses",
                           "────────────────────────────────────────────────────",
                           "  H₀ (Null)         : λ = λ₀ ",
                           "  H₁ (Alternative)  : λ ≠ λ₀ ", "",
                           "────────────────────────────────────────────────────",
                           "Results",
                           "────────────────────────────────────────────────────",
                           "  Type 1 Error (α)   = 0.050",
                           "  Type 2 Error (β)   = 0.507",
                           "  \033[34mStatistical Power  = 0.493\033[0m  \033[1;35m◄◄\033[0m", ""))
    expect_equal(crrPnD, c("╔══════════════════════════════════════════════════╗",
                           "║               \033[34m POWER CALCULATION \033[0m                ║",
                           "╚══════════════════════════════════════════════════╝", "",
                           "Generic t-Test", "",
                           "────────────────────────────────────────────────────",
                           "Hypotheses",
                           "────────────────────────────────────────────────────",
                           "  H₀ (Null)         : λ = λ₀ ",
                           "  H₁ (Alternative)  : λ ≠ λ₀ ", "",
                           "────────────────────────────────────────────────────",
                           "Key Parameters",
                           "────────────────────────────────────────────────────",
                           "  df                = 100 ",
                           "  λ                 = 1.960 ",
                           "  λ₀                 = 0 ",
                           "  T⁻¹(α, λ₀)          = -1.984 and 1.984 ", "",
                           "────────────────────────────────────────────────────",
                           "Results",
                           "────────────────────────────────────────────────────",
                           "  Type 1 Error (α)   = 0.050",
                           "  Type 2 Error (β)   = 0.507",
                           "  \033[34mStatistical Power  = 0.493\033[0m  \033[1;35m◄◄\033[0m", "",
                           "\033[36m────────────────────────────────────────────────────\033[0m",
                           "\033[36mDefinitions\033[0m",
                           "\033[36m────────────────────────────────────────────────────\033[0m",
                           "\033[36m  λ   : Non-centrality parameter under alternative ",
                           "\033[0m\033[36m  λ₀ : Non-centrality parameter under null ", "", "\033[0m"))

    expect_equal(power.t.test(ncp = 1.96, df = 120, alpha = 0.05, alternative = "one.sided", plot = FALSE, verbose = 0),
                 list(alternative = "one.sided", ncp = 1.96, null.ncp = 0, df = 120, alpha = 0.05, t.alpha = 1.6576509,
                      power = 0.619466))
     
    expect_equal(power.t.test(ncp = 0, null.ncp = c(-2, 2), df = 100, alpha = 0.05, alternative = "two.one.sided", plot = FALSE, verbose = 0),
                 list(alternative = "two.one.sided", ncp = 0, null.ncp = c(-2, 2), df = 100, alpha = 0.05,
                      t.alpha = 0.35551537 * c(-1, 1), power = 0.277047649))
     
    expect_equal(power.t.test(ncp = 0, null.ncp = 2, df = 100, alpha = 0.05, alternative = "two.one.sided", plot = FALSE, verbose = 0),
                 list(alternative = "two.one.sided", ncp = 0, null.ncp = c(-2, 2), df = 100, alpha = 0.05,
                      t.alpha = 0.35551537 * c(-1, 1), power = 0.277047649))
     
    expect_equal(power.t.test(ncp = 2, null.ncp = c(-1, 1), df = 400, alpha = 0.05, alternative = "two.one.sided", plot = FALSE, verbose = 0),
                 list(alternative = "two.one.sided", ncp = 2, null.ncp = c(-1, 1), df = 400, alpha = 0.05,
                      t.alpha = 2.97258983 * c(-1, 1), power = 0.167168042))

    expect_equal(power.t.test(ncp = 2, null.ncp = -1, df = 400, alpha = 0.05, alternative = "two.one.sided", plot = FALSE, verbose = 0),
                 list(alternative = "two.one.sided", ncp = 2, null.ncp = c(-1, 1), df = 400, alpha = 0.05,
                      t.alpha = 2.97258983 * c(-1, 1), power = 0.167168042))

    expect_error(power.t.test(ncp = 2, null.ncp = c(-1, 1), df = 100, alpha = 0.05, alternative = "one.sided", plot = FALSE, verbose = 0),
                 "If `alternative` is \"two.sided\" or \"one.sided\", `null.ncp` must be of length one.")
    expect_error(power.t.test(ncp = 2, null.ncp = c(-1, 1, 1), df = 100, alpha = 0.05, alternative = "two.one.sided",
                              plot = FALSE, verbose = 0),
                 paste("If `alternative` is \"two.one.sided\", `null.ncp` must be of length one \\(absolute value,",
                       "that must be different from 0\\), or length two \\(lower and upper bounds\\)."))
    expect_error(power.t.test(ncp = 2, alpha = 0.05, df = 100, alternative = "two.one.sided", plot = FALSE, verbose = 0),
                 paste("If `alternative` is \"two.one.sided\", `null.ncp` must be of length one \\(absolute value,",
                       "that must be different from 0\\), or length two \\(lower and upper bounds\\)."))
})
