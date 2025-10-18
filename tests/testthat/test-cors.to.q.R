test_that("cors.to.q / q.to.cors works", {
    expect_equal(q.to.cors(q = 0.10,         rho1 = 0.5, verbose = FALSE),
                 list(q =  0.1, delta = -0.0712027, rho1 = 0.5, rho2 = 0.5712027), tolerance = 1e-6)
    expect_equal(q.to.cors(q = 0.30,         rho1 = 0.5, verbose = FALSE),
                 list(q =  0.3, delta = -0.1907068, rho1 = 0.5, rho2 = 0.6907068), tolerance = 1e-6)
    expect_equal(q.to.cors(q = 0.50,         rho1 = 0.5, verbose = FALSE),
                 list(q =  0.5, delta = -0.2815365, rho1 = 0.5, rho2 = 0.7815365), tolerance = 1e-6)
    expect_equal(cors.to.q(rho2 = 0.5712027, rho1 = 0.5, verbose = FALSE),
                 list(q = -0.1, delta = -0.0712027, rho1 = 0.5, rho2 = 0.5712027), tolerance = 1e-6)
    expect_equal(cors.to.q(rho2 = 0.6907068, rho1 = 0.5, verbose = FALSE),
                 list(q = -0.3, delta = -0.1907068, rho1 = 0.5, rho2 = 0.6907068), tolerance = 1e-6)
    expect_equal(cors.to.q(rho2 = 0.7815365, rho1 = 0.5, verbose = FALSE),
                 list(q = -0.5, delta = -0.2815365, rho1 = 0.5, rho2 = 0.7815365), tolerance = 1e-6)
})
