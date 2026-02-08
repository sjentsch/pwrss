test_that("generic.binom.test.R works", {
    # power.binom.test

    crrRes <- power.binom.test(size = 200, prob = 0.6, null.prob = 0.5, alpha = 0.05, alternative = "one.sided",
                               plot = FALSE, verbose = 0)
    crrOut <- capture.output(power.binom.test(size = 200, prob = 0.6, null.prob = 0.5, alpha = 0.05, alternative = "one.sided",
                                              plot = FALSE))
    crrPty <- capture.output(power.binom.test(size = 200, prob = 0.6, null.prob = 0.5, alpha = 0.05, alternative = "one.sided",
                                              plot = FALSE, pretty = TRUE))
    expect_equal(crrRes, list(size = 200, alpha = 0.038418816, alternative = "one.sided", prob = 0.6, null.prob = 0.5,
                              binom.alpha = 112, power = 0.860335667))
    expect_equal(crrOut, c("+--------------------------------------------------+",
                           "|                POWER CALCULATION                 |",
                           "+--------------------------------------------------+", "",
                           "Generic Binomial Test", "",
                           "----------------------------------------------------",
                           "Hypotheses",
                           "----------------------------------------------------",
                           "  H0 (Null Claim) : prob <= null.prob ",
                           "  H1 (Alt. Claim) : prob > null.prob ", "",
                           "----------------------------------------------------",
                           "Results",
                           "----------------------------------------------------",
                           "  Type 1 Error (alpha) = 0.038",
                           "  Type 2 Error (beta)  = 0.140",
                           "  Statistical Power    = 0.860  <<", ""))
    expect_equal(crrPty, c("╔══════════════════════════════════════════════════╗",
                           "║               \033[34m POWER CALCULATION \033[0m                ║",
                           "╚══════════════════════════════════════════════════╝", "",
                           "Generic Binomial Test", "",
                           "────────────────────────────────────────────────────",
                           "Hypotheses",
                           "────────────────────────────────────────────────────",
                           "  H₀ (Null)         : P ≤ P₀ ",
                           "  H₁ (Alternative)  : P > P₀ ", "",
                           "────────────────────────────────────────────────────",
                           "Results",
                           "────────────────────────────────────────────────────",
                           "  Type 1 Error (α)   = 0.038",
                           "  Type 2 Error (β)   = 0.140",
                           "  \033[34mStatistical Power  = 0.860\033[0m  \033[1;35m◄◄\033[0m", ""))
     
    crrRes <- power.binom.test(size = 200, prob = 0.4, null.prob = 0.5, alpha = 0.05, alternative = "two.sided",
                               plot = FALSE, verbose = 0)
    expect_equal(crrRes, list(size = 200, alpha = 0.040037192, alternative = "two.sided", prob = 0.4, null.prob = 0.5,
                              binom.alpha = c(85, 114), power = 0.786848265))
     
    crrRes <- power.binom.test(size = 200, prob = 0.5, null.prob = c(0.4, 0.6), alpha = 0.05, alternative = "two.one.sided",
                               plot = FALSE, verbose = 0)
    expect_equal(crrRes, list(size = 200, alpha = 0.049184711, alternative = "two.one.sided", prob = 0.5, null.prob = c(0.4, 0.6),
                              binom.alpha = c(91, 108), power = 0.7707534))

    crrRes <- power.binom.test(size = 200, prob = 0.7, null.prob = c(0.4, 0.6), alpha = 0.05, alternative = "two.one.sided",
                               plot = FALSE, verbose = 0)
    expect_equal(crrRes, list(size = 200, alpha = 0.049432239, alternative = "two.one.sided", prob = 0.7, null.prob = c(0.4, 0.6),
                              binom.alpha = c(66, 133), power = 0.84208844))

    expect_error(power.binom.test(size = -1, prob = 0.6, null.prob = 0.5, alpha = 0.05, alternative = "one.sided", plot = FALSE, verbose = 0),
                 "Argument `size` does not have a valid value \\(integer-like, >= 0, and finite\\).")

    expect_error(power.binom.test(size = 2, prob = 0.6, null.prob = c(0.5, 0.3), alpha = 0.05, alternative = "two.one.sided", plot = FALSE),
                 "Lower margin \\(first value\\) of `null.prob` must not be greater than the upper margin \\(second value\\).")
})
