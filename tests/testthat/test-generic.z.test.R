test_that("generic.z.test.R works", {
    # power.z.test -----------------------------------------------------------------------------------------------------
    expect_equal(power.z.test(mean = 1.96, alpha = 0.05, alternative = "two.sided", plot = FALSE, verbose = 0),
                 list(alternative = "two.sided", mean = 1.96, sd = 1, null.mean = 0, null.sd = 1, alpha = 0.05,
                      z.alpha = 1.959964 * c(-1, 1), power = 0.500058649))

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
