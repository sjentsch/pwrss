test_that("regression.linear.R works", {
    # rsq.to.f ---------------------------------------------------------------------------------------------------------
    expect_equal(rsq.to.f(0.00990099, verbose = FALSE),
                 list(f.squared = 0.0100, f = 0.10, r.squared.full = 0.00990099, r.squared.reduced = 0), tolerance = 1e-6)
    expect_equal(rsq.to.f(0.05882353, verbose = FALSE),
                 list(f.squared = 0.0625, f = 0.25, r.squared.full = 0.05882353, r.squared.reduced = 0), tolerance = 1e-6)
    expect_equal(rsq.to.f(0.13793100, verbose = FALSE),
                 list(f.squared = 0.1600, f = 0.40, r.squared.full = 0.13793100, r.squared.reduced = 0), tolerance = 1e-6)

    # f.to.rsq ---------------------------------------------------------------------------------------------------------
    expect_equal(f.to.rsq(0.10,       verbose = FALSE),
                 list(f.squared = 0.0100, f = 0.10, r.squared.full = 0.00990099, r.squared.reduced = 0), tolerance = 1e-6)
    expect_equal(f.to.rsq(0.25,       verbose = FALSE),
                 list(f.squared = 0.0625, f = 0.25, r.squared.full = 0.05882353, r.squared.reduced = 0), tolerance = 1e-6)
    expect_equal(f.to.rsq(0.40,       verbose = FALSE),
                 list(f.squared = 0.1600, f = 0.40, r.squared.full = 0.13793100, r.squared.reduced = 0), tolerance = 1e-6)

    # power.f.regression (= pwrss.f.regression) ------------------------------------------------------------------------
      
    # power.t.regression (= pwrss.t.regression) ------------------------------------------------------------------------
      
    # pwrss.z.mean, pwrss.z.2means, pwrss.z.regression (not longer supported) ------------------------------------------
    expect_error(pwrss.z.mean(),       "This function is no longer available. Please use `power.t.student\\(\\)`.")
    expect_error(pwrss.z.2means(),     "This function is no longer available. Please use `power.t.student\\(\\)` or `power.t.welch\\(\\)`.")
    expect_error(pwrss.z.regression(), "This function is no longer available. Please use `power.t.regression\\(\\)`.")
})
