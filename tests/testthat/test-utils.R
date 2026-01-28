test_that("utils.R works", {
    # inflate.sample ---------------------------------------------------------------------------------------------------
    expect_equal(vapply(2 ^ seq(10), inflate.sample, numeric(1), rate = 0.05, verbose = FALSE), c(3, 5,  9, 17, 34, 68, 135, 270, 539, 1078))
    expect_equal(vapply(2 ^ seq(10), inflate.sample, numeric(1), rate = 0.20, verbose = FALSE), c(3, 5, 10, 20, 40, 80, 160, 320, 640, 1280))
    expect_equal(vapply(2 ^ seq(10), inflate.sample, numeric(1), rate = 0.20, ceiling = FALSE, verbose = FALSE),
                 c(2.5, 5, 10, 20, 40, 80, 160, 320, 640, 1280))
    expect_equal(capture.output(invisible(vapply(2 ^ seq(10), inflate.sample, numeric(1), rate = 0.20))), "35102040801603206401280")

    # etasq.to.f -------------------------------------------------------------------------------------------------------
    expect_equal(etasq.to.f(0.009900990, verbose = FALSE), list(f.squared = 0.0100, f = 0.10, eta.squared = 0.009900990))
    expect_equal(etasq.to.f(0.058823530, verbose = FALSE), list(f.squared = 0.0625, f = 0.25, eta.squared = 0.058823530))
    expect_equal(etasq.to.f(0.137931034, verbose = FALSE), list(f.squared = 0.1600, f = 0.40, eta.squared = 0.137931034))

    # f.to.etasq -------------------------------------------------------------------------------------------------------
    expect_equal(f.to.etasq(0.10,     verbose = FALSE), list(eta.squared = 0.009900990, f.squared = 0.0100, f = 0.10))
    expect_equal(f.to.etasq(0.25,     verbose = FALSE), list(eta.squared = 0.058823530, f.squared = 0.0625, f = 0.25))
    expect_equal(f.to.etasq(0.40,     verbose = FALSE), list(eta.squared = 0.137931034, f.squared = 0.1600, f = 0.40))

    # cor.to.z ---------------------------------------------------------------------------------------------------------
    expect_equal(cor.to.z(1.000,  verbose = FALSE), list(z =  Inf,         rho =  1.000))
    expect_equal(cor.to.z(-1.000, verbose = FALSE), list(z = -Inf,         rho = -1.000))
    expect_equal(cor.to.z(0.999,  verbose = FALSE), list(z =  3.800201170, rho =  0.999))
    expect_equal(cor.to.z(-0.999, verbose = FALSE), list(z = -3.800201170, rho = -0.999))
    expect_equal(cor.to.z(0.990,  verbose = FALSE), list(z =  2.646652410, rho =  0.990))
    expect_equal(cor.to.z(-0.990, verbose = FALSE), list(z = -2.646652410, rho = -0.990))
    expect_equal(cor.to.z(0.950,  verbose = FALSE), list(z =  1.831780820, rho =  0.950))
    expect_equal(cor.to.z(-0.950, verbose = FALSE), list(z = -1.831780820, rho = -0.950))
    expect_equal(cor.to.z(0.900,  verbose = FALSE), list(z =  1.472219490, rho =  0.900))
    expect_equal(cor.to.z(-0.900, verbose = FALSE), list(z = -1.472219490, rho = -0.900))
    expect_equal(cor.to.z(0.500,  verbose = FALSE), list(z =  0.549306144, rho =  0.500))
    expect_equal(cor.to.z(-0.500, verbose = FALSE), list(z = -0.549306144, rho = -0.500))
    expect_equal(cor.to.z(0.200,  verbose = FALSE), list(z =  0.202732554, rho =  0.200))
    expect_equal(cor.to.z(-0.200, verbose = FALSE), list(z = -0.202732554, rho = -0.200))
    expect_equal(cor.to.z(0.100,  verbose = FALSE), list(z =  0.100335348, rho =  0.100))
    expect_equal(cor.to.z(-0.100, verbose = FALSE), list(z = -0.100335348, rho = -0.100))
    expect_equal(cor.to.z(0.000,  verbose = FALSE), list(z =  0.000000000, rho =  0.000))

    # z.to.cor ---------------------------------------------------------------------------------------------------------
    expect_equal(z.to.cor(0.1,  verbose = FALSE), list(rho =  0.099667995, z =  0.1))
    expect_equal(z.to.cor(-0.1, verbose = FALSE), list(rho = -0.099667995, z = -0.1))
    expect_equal(z.to.cor(1.0,  verbose = FALSE), list(rho =  0.761594160, z =  1.0))
    expect_equal(z.to.cor(-1.0, verbose = FALSE), list(rho = -0.761594160, z = -1.0))
    expect_equal(z.to.cor(2.0,  verbose = FALSE), list(rho =  0.964027580, z =  2.0))
    expect_equal(z.to.cor(-2.0, verbose = FALSE), list(rho = -0.964027580, z = -2.0))
    expect_equal(z.to.cor(4.0,  verbose = FALSE), list(rho =  0.999329300, z =  4.0))
    expect_equal(z.to.cor(-4.0, verbose = FALSE), list(rho = -0.999329300, z = -4.0))

    # cors.to.q --------------------------------------------------------------------------------------------------------
    expect_equal(q.to.cors(q = 0.10,         rho1 = 0.5, verbose = FALSE),
                 list(q =  0.1, delta = -0.071202682, rho1 = 0.5, rho2 = 0.571202682))
    expect_equal(q.to.cors(q = 0.30,         rho1 = 0.5, verbose = FALSE),
                 list(q =  0.3, delta = -0.190706810, rho1 = 0.5, rho2 = 0.690706810))
    expect_equal(q.to.cors(q = 0.50,         rho1 = 0.5, verbose = FALSE),
                 list(q =  0.5, delta = -0.281536455, rho1 = 0.5, rho2 = 0.781536455))

    # q.to.cors --------------------------------------------------------------------------------------------------------
    expect_equal(cors.to.q(rho2 = 0.571202682, rho1 = 0.5, verbose = FALSE),
                 list(q = -0.1, delta = -0.071202682, rho1 = 0.5, rho2 = 0.571202682))
    expect_equal(cors.to.q(rho2 = 0.690706810, rho1 = 0.5, verbose = FALSE),
                 list(q = -0.3, delta = -0.190706810, rho1 = 0.5, rho2 = 0.690706810))
    expect_equal(cors.to.q(rho2 = 0.781536455, rho1 = 0.5, verbose = FALSE),
                 list(q = -0.5, delta = -0.281536455, rho1 = 0.5, rho2 = 0.781536455))

    # d.to.cles --------------------------------------------------------------------------------------------------------
    expect_equal(d.to.cles(0.2,       verbose = FALSE), list(cles = 0.556231458, d = 0.2))
    expect_equal(d.to.cles(0.5,       verbose = FALSE), list(cles = 0.638163195, d = 0.5))
    expect_equal(d.to.cles(0.8,       verbose = FALSE), list(cles = 0.714196178, d = 0.8))
    expect_equal(capture.output(d.to.cles(0.2,       verbose = TRUE)), c("     cles         d ", "0.5562315 0.2000000 "))
    expect_equal(capture.output(d.to.cles(0.5,       verbose = TRUE)), c("     cles         d ", "0.6381632 0.5000000 "))
    expect_equal(capture.output(d.to.cles(0.8,       verbose = TRUE)), c("     cles         d ", "0.7141962 0.8000000 "))

    # cles.to.d --------------------------------------------------------------------------------------------------------
    expect_equal(cles.to.d(0.556231458, verbose = FALSE), list(d = 0.2, cles = 0.556231458))
    expect_equal(cles.to.d(0.638163195, verbose = FALSE), list(d = 0.5, cles = 0.638163195))
    expect_equal(cles.to.d(0.714196178, verbose = FALSE), list(d = 0.8, cles = 0.714196178))
    expect_equal(capture.output(cles.to.d(0.5562315, verbose = TRUE)), c("        d      cles ", "0.2000002 0.5562315 "))
    expect_equal(capture.output(cles.to.d(0.6381632, verbose = TRUE)), c("        d      cles ", "0.5000000 0.6381632 "))
    expect_equal(capture.output(cles.to.d(0.7141962, verbose = TRUE)), c("        d      cles ", "0.8000001 0.7141962 "))

    # means.to.d -------------------------------------------------------------------------------------------------------
    expect_equal(suppressWarnings(means.to.d(mu1 = 20, mu2 = 17.5, sd1 = 5, sd2 = 15, n2 = 30, verbose = FALSE)),
                 list(d = 0.223606800, mu1 = 20, mu2 = 17.5, sd1 = 5, sd2 = 15, pooled.sd = 11.1803400, var.ratio = 1 / 9,
                      n1 = 30, n2 = 30, n.ratio = 1, paired = FALSE, rho.paired = 0.5, verbose = FALSE))
    expect_warning(means.to.d(mu1 = 20, mu2 = 17.5, sd1 = 5, sd2 = 15, n2 = 30, verbose = FALSE),
                   "Interpretation of Cohen's d may no longer be valid when variances differ beyond sampling error\\.")
    expect_equal(means.to.d(mu1 = 20, mu2 = 17.5, sd1 = 5, sd2 = 6, n2 = 30, verbose = FALSE),
                 list(d = 0.452678730, mu1 = 20, mu2 = 17.5, sd1 = 5, sd2 = 6, pooled.sd =  5.52268051, var.ratio = 0.694444444,
                      n1 = 30, n2 = 30, n.ratio = 1, paired = FALSE, rho.paired = 0.5, verbose = FALSE))
    expect_equal(suppressWarnings(means.to.d(mu1 = 20, mu2 = 17.5, sd1 = 5, sd2 = 15, n2 = 30, paired = TRUE, verbose = FALSE)),
                 list(d = 0.188982237, mu1 = 20, mu2 = 17.5, sd1 = 5, sd2 = 15, pooled.sd = 13.2287566, var.ratio = 1 / 9,
                      n1 = 30, n2 = 30, n.ratio = 1, paired = TRUE, rho.paired = 0.5, verbose = FALSE))
    expect_warning(means.to.d(mu1 = 20, mu2 = 17.5, sd1 = 5, sd2 = 15, n2 = 30, paired = TRUE, verbose = FALSE),
                   "Interpretation of Cohen's d may no longer be valid when variances differ beyond sampling error\\.")
    expect_equal(means.to.d(mu1 = 20, mu2 = 17.5, sd1 = 5, sd2 = 6, n2 = 30, paired = TRUE, verbose = FALSE),
                 list(d = 0.449013255, mu1 = 20, mu2 = 17.5, sd1 = 5, sd2 = 6, pooled.sd =  5.56776436, var.ratio = 0.694444444,
                      n1 = 30, n2 = 30, n.ratio = 1, paired = TRUE, rho.paired = 0.5, verbose = FALSE))

    # probs.to.h -------------------------------------------------------------------------------------------------------
    expect_equal(probs.to.h(prob1 = 0.56,   prob2 = 0.50, verbose = FALSE), list(h = 0.120289882, prob1 = 0.56,   prob2 = 0.50))
    expect_equal(probs.to.h(prob1 = 0.60,   prob2 = 0.40, verbose = FALSE), list(h = 0.402715842, prob1 = 0.60,   prob2 = 0.40))
    expect_equal(probs.to.h(prob1 = 0.25,   prob2 = 0.15, verbose = FALSE), list(h = 0.251798721, prob1 = 0.25,   prob2 = 0.15))
    expect_equal(probs.to.h(prob1 = 8 / 15, prob2 = 0.40, verbose = FALSE), list(h = 0.268074069, prob1 = 8 / 15, prob2 = 0.40))
    expect_equal(capture.output(probs.to.h(prob1 = 0.56,   prob2 = 0.50)), c("        h     prob1     prob2 ", "0.1202899 0.5600000 0.5000000 "))
    expect_equal(capture.output(probs.to.h(prob1 = 0.60,   prob2 = 0.40)), c("        h     prob1     prob2 ", "0.4027158 0.6000000 0.4000000 "))
    expect_equal(capture.output(probs.to.h(prob1 = 0.25,   prob2 = 0.15)), c("        h     prob1     prob2 ", "0.2517987 0.2500000 0.1500000 "))
    expect_equal(capture.output(probs.to.h(prob1 = 8 / 15, prob2 = 0.40)), c("        h     prob1     prob2 ", "0.2680741 0.5333333 0.4000000 "))

    # joint.probs.2x2 --------------------------------------------------------------------------------------------------

    # marginal.probs.2x2 -----------------------------------------------------------------------------------------------

    # probs.to.w -------------------------------------------------------------------------------------------------------
    expect_equal(probs.to.w(c(0.28, 0.72), rep(0.5, 2), verbose = FALSE),
                 list(w = 0.44, df = 1, prob.matrix = c(0.28, 0.72), null.prob.matrix = rep(0.5, 2)))
    expect_equal(probs.to.w(c(0.28, 0.72), verbose = FALSE),
                 list(w = 0.44, df = 1, prob.matrix = c(0.28, 0.72), null.prob.matrix = rep(0.5, 2)))
    crrPrb <- rbind(c(0.056, 0.132), c(0.944, 0.868))
    expect_equal(probs.to.w(crrPrb, verbose = FALSE),
                 list(w = 0.130213368, df = 1, prob.matrix = crrPrb, null.prob.matrix = outer(rowSums(crrPrb), colSums(crrPrb)) / sum(crrPrb)))
    crrPrb <- cbind(c(0.6759, 0.1559, 0.1281, 0.0323, 0.0078), c(0.6771, 0.1519, 0.1368, 0.0241, 0.0101))
    expect_equal(probs.to.w(crrPrb, verbose = FALSE),
                 list(w = 0.030220075, df = 4, prob.matrix = crrPrb, null.prob.matrix = outer(rowSums(crrPrb), colSums(crrPrb)) / sum(crrPrb)))
    expect_equal(capture.output(probs.to.w(c(0.28, 0.72), rep(0.5, 2))), c("   w   df ", "0.44 1.00 "))
    expect_equal(capture.output(probs.to.w(rbind(c(0.056, 0.132), c(0.944, 0.868)))), c("        w        df ", "0.1302134 1.0000000 "))
    expect_equal(capture.output(probs.to.w(cbind(c(0.6759, 0.1559, 0.1281, 0.0323, 0.0078), c(0.6771, 0.1519, 0.1368, 0.0241, 0.0101)))),
                 c("         w         df ", "0.03022008 4.00000000 "))
    expect_error(probs.to.w(c(0, 1.1)), "Matrix elements outside of \\[0, 1\\] range.")
    expect_error(probs.to.w(c(0.2, 0.8), c(0, 1.1)), "Matrix elements outside of \\[0, 1\\] range.")
    expect_error(probs.to.w(c(0.1, 0.2, 0.3, 0.4), c(0.5, 0.5)), "Length of `prob.matrix` and `null.prob.matrix` should match.")
    expect_error(probs.to.w(c(0.2, 0.3, 0.4, 0.5)), "Cell probabilities should sum to 1.")
    expect_error(probs.to.w(matrix(c(0.2, 0.3, 0.4, 0.5), nrow = 2), matrix(c(0.2, 0.3, 0.4, 0.5), nrow = 1)),
                 "Dimensions for `prob.matrix` and `null.prob.matrix` do not match.")
    expect_error(probs.to.w(data.frame(A = c(0.2, 0.3, 0.4, 0.5))), "`prob.matrix` must be either a vector or a matrix.")
})
