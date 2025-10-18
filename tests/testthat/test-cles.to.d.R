test_that("cles.to.d / d.to.cles works", {
    expect_equal(d.to.cles(0.2,       verbose = FALSE), list(cles = 0.5562315, d = 0.2), tolerance = 1e-6)
    expect_equal(d.to.cles(0.5,       verbose = FALSE), list(cles = 0.6381632, d = 0.5), tolerance = 1e-6)
    expect_equal(d.to.cles(0.8,       verbose = FALSE), list(cles = 0.7141962, d = 0.8), tolerance = 1e-6)
    expect_equal(cles.to.d(0.5562315, verbose = FALSE), list(d = 0.2, cles = 0.5562315), tolerance = 1e-6)
    expect_equal(cles.to.d(0.6381632, verbose = FALSE), list(d = 0.5, cles = 0.6381632), tolerance = 1e-6)
    expect_equal(cles.to.d(0.7141962, verbose = FALSE), list(d = 0.8, cles = 0.7141962), tolerance = 1e-6)
})
