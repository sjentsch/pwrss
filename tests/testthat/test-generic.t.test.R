test_that("generic.t.test.R works", {
    # power.t.test -----------------------------------------------------------------------------------------------------
    expect_equal(power.t.test(ncp = 1.96, df = 100, alpha = 0.05, alternative = "two.sided", plot = FALSE, verbose = 0),
                 list(alternative = "two.sided", ncp = 1.96, null.ncp = 0, df = 100, alpha = 0.05, t.alpha = 1.98397152 * c(-1, 1),
                      power = 0.49254726))

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
