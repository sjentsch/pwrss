test_that("generic.binom.test.R works", {
    # power.binom.test -------------------------------------------------------------------------------------------------
    expect_equal(power.binom.test(size = 200, prob = 0.6, null.prob = 0.5, alpha = 0.05, alternative = "one.sided", plot = FALSE,
                                  verbose = 0),
                 list(size = 200, alpha = 0.038418816, alternative = "one.sided", prob = 0.6, null.prob = 0.5,
                              binom.alpha = 112, power = 0.860335667))

    expect_equal(power.binom.test(size = 200, prob = 0.4, null.prob = 0.5, alpha = 0.05, alternative = "two.sided",
                                  plot = FALSE, verbose = 0),
                 list(size = 200, alpha = 0.040037192, alternative = "two.sided", prob = 0.4, null.prob = 0.5,
                      binom.alpha = c(85, 114), power = 0.786848265))

    expect_equal(power.binom.test(size = 200, prob = 0.5, null.prob = c(0.4, 0.6), alpha = 0.05, alternative = "two.one.sided",
                                  plot = FALSE, verbose = 0),
                 list(size = 200, alpha = 0.049184711, alternative = "two.one.sided", prob = 0.5, null.prob = c(0.4, 0.6),
                      binom.alpha = c(91, 108), power = 0.7707534))

    expect_equal(power.binom.test(size = 200, prob = 0.7, null.prob = c(0.4, 0.6), alpha = 0.05, alternative = "two.one.sided",
                                  plot = FALSE, verbose = 0),
                 list(size = 200, alpha = 0.049432239, alternative = "two.one.sided", prob = 0.7, null.prob = c(0.4, 0.6),
                      binom.alpha = c(66, 133), power = 0.84208844))

    expect_error(power.binom.test(size = 200, prob = 0.6, null.prob = c(0.5, 0.7), alpha = 0.05, alternative = "one.sided",
                                  plot = FALSE, verbose = 0),
                 "If `alternative` is \"two.sided\" or \"one.sided\", `null.prob` must be of length one.")
    expect_error(power.binom.test(size = -1, prob = 0.6, null.prob = 0.5, alpha = 0.05, alternative = "one.sided", plot = FALSE, verbose = 0),
                 "Argument `size` does not have a valid value \\(integer-like, >= 0, and finite\\).")
    expect_error(power.binom.test(size = 2, prob = 0.6, null.prob = 0.5, alpha = 0.05, alternative = "two.one.sided", plot = FALSE),
                 paste("If `alternative` is \"two.one.sided\", `null.prob` must be of length two \\(lower and upper bounds,",
                       "with the upper bound being larger than the lower bound\\)."))
    expect_error(power.binom.test(size = 2, prob = 0.6, null.prob = c(0.5, 0.3), alpha = 0.05, alternative = "two.one.sided", plot = FALSE),
                 paste("If `alternative` is \"two.one.sided\", `null.prob` must be of length two \\(lower and upper bounds,",
                       "with the upper bound being larger than the lower bound\\)."))
})
