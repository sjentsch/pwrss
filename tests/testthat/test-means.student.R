test_that("means.student.R works", {
    # power.t.student (= pwrss.t.mean / pwrss.t.2means)
    # power.t.welch

    # pwrss.z.mean, pwrss.z.2means (not longer supported) --------------------------------------------------------------
    expect_error(pwrss.z.mean(),   "This function is no longer available. Please use `power.t.student\\(\\)`.")
    expect_error(pwrss.z.2means(), "This function is no longer available. Please use `power.t.student\\(\\)` or `power.t.welch\\(\\)`.")
})
