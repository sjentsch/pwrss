test_that("generic.chisq.test.R works", {
    # power.chisq.test -------------------------------------------------------------------------------------------------
    expect_equal(power.chisq.test(ncp = 20, df = 100, alpha = 0.05, plot = FALSE, verbose = 0),
                 list(power = 0.381376391, ncp = 20, null.ncp = 0, alpha = 0.05, df = 100, chisq.crit = 124.342113))

    expect_equal(power.chisq.test(ncp = 20, null.ncp = 10, df = 10, alpha = 0.05, plot = FALSE, verbose = 0),
                 list(power = 0.31365895, ncp = 20, null.ncp = 10, alpha = 0.05, df = 10, chisq.crit = 34.0886349))

    expect_equal(power.chisq.test(ncp = 20, df = 17, alpha = 0.05, plot = FALSE, verbose = 0),
                 list(power = 0.80744863, ncp = 20, null.ncp = 0, alpha = 0.05, df = 17, chisq.crit = 27.5871116))

    expect_error(power.chisq.test(ncp = 1, null.ncp = 11, df = 17, alpha = 0.05, plot = FALSE, verbose = 0),
                 "`ncp` should be greater than or equal to `null.ncp`.")
})
