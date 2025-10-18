test_that("means.to.d works", {
    expect_equal(suppressWarnings(means.to.d(mu1 = 20, mu2 = 17.5, sd1 = 5, sd2 = 15, n2 = 30, verbose = FALSE)),
                 list(d = 0.2236068, mu1 = 20, mu2 = 17.5, sd1 = 5, sd2 = 15, pooled.sd = 11.18034, var.ratio = 1 / 9,
                      n1 = 30, n2 = 30, n.ratio = 1, paired = FALSE, rho.paired = 0.5, verbose = FALSE), tolerance = 1e-6)
    expect_warning(means.to.d(mu1 = 20, mu2 = 17.5, sd1 = 5, sd2 = 15, n2 = 30, verbose = FALSE),
                   "Interpretation of Cohen's d may no longer be valid when variances differ beyond sampling error\\.")
    expect_equal(means.to.d(mu1 = 20, mu2 = 17.5, sd1 = 5, sd2 = 6, n2 = 30, verbose = FALSE),
                 list(d = 0.4526787, mu1 = 20, mu2 = 17.5, sd1 = 5, sd2 = 6, pooled.sd = 5.522681, var.ratio = 0.6944444,
                      n1 = 30, n2 = 30, n.ratio = 1, paired = FALSE, rho.paired = 0.5, verbose = FALSE), tolerance = 1e-6)
    expect_equal(suppressWarnings(means.to.d(mu1 = 20, mu2 = 17.5, sd1 = 5, sd2 = 15, n2 = 30, paired = TRUE, verbose = FALSE)),
                 list(d = 0.1889822, mu1 = 20, mu2 = 17.5, sd1 = 5, sd2 = 15, pooled.sd = 13.22876, var.ratio = 1 / 9,
                      n1 = 30, n2 = 30, n.ratio = 1, paired = TRUE, rho.paired = 0.5, verbose = FALSE), tolerance = 1e-6)
    expect_warning(means.to.d(mu1 = 20, mu2 = 17.5, sd1 = 5, sd2 = 15, n2 = 30, paired = TRUE, verbose = FALSE),
                   "Interpretation of Cohen's d may no longer be valid when variances differ beyond sampling error\\.")
    expect_equal(means.to.d(mu1 = 20, mu2 = 17.5, sd1 = 5, sd2 = 6, n2 = 30, paired = TRUE, verbose = FALSE),
                 list(d = 0.4490133, mu1 = 20, mu2 = 17.5, sd1 = 5, sd2 = 6, pooled.sd = 5.567764, var.ratio = 0.6944444,
                      n1 = 30, n2 = 30, n.ratio = 1, paired = TRUE, rho.paired = 0.5, verbose = FALSE), tolerance = 1e-6)
})
