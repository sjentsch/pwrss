test_that("etasq.to.f / f.to.etasq works", {
    expect_equal(etasq.to.f(0.009901, verbose = FALSE), list(f.squared = 0.0100, f = 0.10, eta.squared = 0.009901), tolerance = 1e-5)
    expect_equal(etasq.to.f(0.058824, verbose = FALSE), list(f.squared = 0.0625, f = 0.25, eta.squared = 0.058824), tolerance = 1e-5)
    expect_equal(etasq.to.f(0.137931, verbose = FALSE), list(f.squared = 0.1600, f = 0.40, eta.squared = 0.137931), tolerance = 1e-5)
    expect_equal(f.to.etasq(0.10,     verbose = FALSE), list(eta.squared = 0.009901, f.squared = 0.0100, f = 0.10), tolerance = 1e-5)
    expect_equal(f.to.etasq(0.25,     verbose = FALSE), list(eta.squared = 0.058823, f.squared = 0.0625, f = 0.25), tolerance = 1e-5)
    expect_equal(f.to.etasq(0.40,     verbose = FALSE), list(eta.squared = 0.137931, f.squared = 0.1600, f = 0.40), tolerance = 1e-5)
})
