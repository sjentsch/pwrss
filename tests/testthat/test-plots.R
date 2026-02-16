test_that("plots work", {
    # binomial-tests ---------------------------------------------------------------------------------------------------
    path <- save_png(power.binom.test(size = 200, prob = 0.6, null.prob = 0.5, alpha = 0.05, alternative = "one.sided", verbose = 0))
    expect_snapshot_file(path, "binom_plot_1")
    unlink(path)

    path <- save_png(power.binom.test(size = 200, prob = 0.4, null.prob = 0.5, alpha = 0.05, alternative = "one.sided", verbose = 0))
    expect_snapshot_file(path, "binom_plot_2")
    unlink(path)

    path <- save_png(power.binom.test(size = 200, prob = 0.4, null.prob = 0.5, alpha = 0.05, alternative = "two.sided", verbose = 0))
    expect_snapshot_file(path, "binom_plot_3")
    unlink(path)

    path <- save_png(power.binom.test(size = 200, prob = 0.5, null.prob = c(0.4, 0.6), alpha = 0.05, alternative = "two.one.sided", verbose = 0))
    expect_snapshot_file(path, "binom_plot_4")
    unlink(path)

    path <- save_png(power.binom.test(size = 200, prob = 0.5, null.prob = c(0.3, 0.4), alpha = 0.05, alternative = "two.one.sided", verbose = 0))
    expect_snapshot_file(path, "binom_plot_5")
    unlink(path)
    
    path <- save_png(.plot.binom.t1t2(size = 200, prob = 0.5, null.prob = 0.2, alpha = 0.05, alternative = "two.one.sided"))
    expect_snapshot_file(path, "binom_plot_6")
    unlink(path)

    expect_error(.plot.binom.t1t2(size = 5, prob = 0.4, alpha = 0.05, alternative = "two.sided"),
                 "Number of trials should be greater than 10 for plotting.")

    # chisq-tests ------------------------------------------------------------------------------------------------------
    path <- save_png(power.chisq.test(ncp = 20, df =    1, alpha = 0.05, verbose = 0))
    expect_snapshot_file(path, "chisq_plot_1")
    unlink(path)

    path <- save_png(power.chisq.test(ncp = 20, df =    2, alpha = 0.05, verbose = 0))
    expect_snapshot_file(path, "chisq_plot_2")
    unlink(path)

    path <- save_png(power.chisq.test(ncp = 20, df =    4, alpha = 0.05, verbose = 0))
    expect_snapshot_file(path, "chisq_plot_3")
    unlink(path)

    path <- save_png(power.chisq.test(ncp = 20, df =   10, alpha = 0.05, verbose = 0))
    expect_snapshot_file(path, "chisq_plot_4")
    unlink(path)

    path <- save_png(power.chisq.test(ncp = 20, df =  100, alpha = 0.05, verbose = 0))
    expect_snapshot_file(path, "chisq_plot_5")
    unlink(path)

    path <- save_png(power.chisq.test(ncp = 20, df = 1000, alpha = 0.05, verbose = 0))
    expect_snapshot_file(path, "chisq_plot_6")
    unlink(path)

    path <- save_png(power.chisq.test(ncp = 40, df =  100, alpha = 0.05, verbose = 0))
    expect_snapshot_file(path, "chisq_plot_7")
    unlink(path)

    expect_error(.plot.chisq.t1t2(ncp = 40, df = Inf),
                 "`df` must be numeric, finite, have a value of at least 1 and have a length of 1.")

    # F-tests ----------------------------------------------------------------------------------------------------------
    path <- save_png(power.f.test(ncp = 1, df1 = 1, df2 =   10, alpha = 0.05, verbose = 0))
    expect_snapshot_file(path, "F_plot_01")
    unlink(path)

    path <- save_png(power.f.test(ncp = 1, df1 = 1, df2 =  100, alpha = 0.05, verbose = 0))
    expect_snapshot_file(path, "F_plot_02")
    unlink(path)

    path <- save_png(power.f.test(ncp = 1, df1 = 1, df2 = 1000, alpha = 0.05, verbose = 0))
    expect_snapshot_file(path, "F_plot_03")
    unlink(path)

    path <- save_png(power.f.test(ncp = 2, df1 = 1, df2 =   10, alpha = 0.05, verbose = 0))
    expect_snapshot_file(path, "F_plot_04")
    unlink(path)

    path <- save_png(power.f.test(ncp = 2, df1 = 1, df2 =  100, alpha = 0.05, verbose = 0))
    expect_snapshot_file(path, "F_plot_05")
    unlink(path)

    path <- save_png(power.f.test(ncp = 2, df1 = 1, df2 = 1000, alpha = 0.05, verbose = 0))
    expect_snapshot_file(path, "F_plot_06")
    unlink(path)

    path <- save_png(power.f.test(ncp = 4, df1 = 1, df2 =   10, alpha = 0.05, verbose = 0))
    expect_snapshot_file(path, "F_plot_07")
    unlink(path)

    path <- save_png(power.f.test(ncp = 4, df1 = 1, df2 =  100, alpha = 0.05, verbose = 0))
    expect_snapshot_file(path, "F_plot_08")
    unlink(path)

    path <- save_png(power.f.test(ncp = 4, df1 = 1, df2 = 1000, alpha = 0.05, verbose = 0))
    expect_snapshot_file(path, "F_plot_09")
    unlink(path)

    path <- save_png(power.f.test(ncp = 8, df1 = 1, df2 =   10, alpha = 0.05, verbose = 0))
    expect_snapshot_file(path, "F_plot_10")
    unlink(path)

    path <- save_png(power.f.test(ncp = 8, df1 = 1, df2 =  100, alpha = 0.05, verbose = 0))
    expect_snapshot_file(path, "F_plot_11")
    unlink(path)

    path <- save_png(power.f.test(ncp = 8, df1 = 1, df2 = 1000, alpha = 0.05, verbose = 0))
    expect_snapshot_file(path, "F_plot_12")
    unlink(path)

    path <- save_png(power.f.test(ncp = 4, df1 = 2, df2 =   10, alpha = 0.05, verbose = 0))
    expect_snapshot_file(path, "F_plot_13")
    unlink(path)

    path <- save_png(power.f.test(ncp = 4, df1 = 2, df2 =  100, alpha = 0.05, verbose = 0))
    expect_snapshot_file(path, "F_plot_14")
    unlink(path)

    path <- save_png(power.f.test(ncp = 4, df1 = 2, df2 = 1000, alpha = 0.05, verbose = 0))
    expect_snapshot_file(path, "F_plot_15")
    unlink(path)

    path <- save_png(power.f.test(ncp = 8, df1 = 4, df2 =   10, alpha = 0.05, verbose = 0))
    expect_snapshot_file(path, "F_plot_16")
    unlink(path)

    path <- save_png(power.f.test(ncp = 8, df1 = 4, df2 =  100, alpha = 0.05, verbose = 0))
    expect_snapshot_file(path, "F_plot_17")
    unlink(path)

    path <- save_png(power.f.test(ncp = 8, df1 = 4, df2 = 1000, alpha = 0.05, verbose = 0))
    expect_snapshot_file(path, "F_plot_18")

    expect_error(.plot.f.t1t2(ncp = 1, df1 = 0, df2 = 10),
                 "`df1` must be numeric, finite, have a value of at least 1 and have a length of 1.")
    expect_error(.plot.f.t1t2(ncp = 1, df1 = 1, df2 = Inf),
                 "`df2` must be numeric, finite, have a value of at least 1 and have a length of 1.")    

    # t-tests ----------------------------------------------------------------------------------------------------------
    path <- save_png(power.t.test(ncp = 1.96, df = 100, alpha = 0.05, alternative = "two.sided", verbose = 0))
    expect_snapshot_file(path, "t_plot_1")
    unlink(path)

    path <- save_png(power.t.test(ncp = -1.96, df = 100, alpha = 0.05, alternative = "two.sided", verbose = 0))
    expect_snapshot_file(path, "t_plot_2")
    unlink(path)

    path <- save_png(power.t.test(ncp = 1.96, df = 100, alpha = 0.05, alternative = "one.sided", verbose = 0))
    expect_snapshot_file(path, "t_plot_3")
    unlink(path)

    path <- save_png(power.t.test(ncp = 0, null.ncp = c(-2, 2), df = 100, alpha = 0.05, alternative = "two.one.sided", verbose = 0))
    expect_snapshot_file(path, "t_plot_4")
    unlink(path)

    path <- save_png(power.t.test(ncp = 2, null.ncp = c(-1, 1), df = 100, alpha = 0.05, alternative = "two.one.sided", verbose = 0))
    expect_snapshot_file(path, "t_plot_5")
    unlink(path)

    # z-tests ----------------------------------------------------------------------------------------------------------
    path <- save_png(power.z.test(mean = 1.96, alpha = 0.05, alternative = "two.sided", verbose = 0))
    expect_snapshot_file(path, "z_plot_1")
    unlink(path)

    path <- save_png(power.z.test(mean = -1.96, alpha = 0.05, alternative = "two.sided", verbose = 0))
    expect_snapshot_file(path, "z_plot_2")
    unlink(path)

    path <- save_png(power.z.test(mean = 1.96, alpha = 0.05, alternative = "one.sided", verbose = 0))
    expect_snapshot_file(path, "z_plot_3")
    unlink(path)

    path <- save_png(power.z.test(mean = 0, null.mean = c(-2, 2), alpha = 0.05, alternative = "two.one.sided", verbose = 0))
    expect_snapshot_file(path, "z_plot_4")
    unlink(path)

    path <- save_png(power.z.test(mean = 2, null.mean = c(-1, 1), alpha = 0.05, alternative = "two.one.sided", verbose = 0))
    expect_snapshot_file(path, "z_plot_5")
    unlink(path)

    # plots.R (taking a results-obj and using it for plotting) ---------------------------------------------------------
    # binom: proportions.onetwo (only exact.oneprop) -------------------------------------------------------------------
    path <- save_png(plot(power.exact.oneprop(prob = 0.45, null.prob = 0.50, alpha = 0.05, n = 500, alternative = "one.sided", verbose = 0)))
    expect_snapshot_file(path, "plot_oneprop.binom")
    unlink(path)
    
    # chisq: chisq.gof -------------------------------------------------------------------------------------------------
    mtxW <- probs.to.w(cbind(c(0.6759, 0.1559, 0.1281, 0.0323, 0.0078), c(0.6771, 0.1519, 0.1368, 0.0241, 0.0101)), verbose = 0)
    path <- save_png(plot(power.chisq.gof(w = mtxW$w, df = mtxW$df, power = 0.80, alpha = 0.05, verbose = 0)))
    expect_snapshot_file(path, "plot_gof")
    unlink(path)

    # F: ancova, keppel, shieh, mixed.anova, (f.)regression ------------------------------------------------------------
    path <- save_png(plot(power.f.ancova(eta.squared = 0.059, factor.levels = 2, alpha = 0.05, power = .80, verbose = 0)))
    expect_snapshot_file(path, "plot_ancova")
    unlink(path)

    path <- save_png(plot(power.f.ancova.keppel(mu.vector = c(0.50, 0.00), sd.vector = rep(1, 2), p.vector = rep(0.50, 2),
                                                k.covariates = 1, r.squared = 0.50, alpha = 0.05, power = 0.80, verbose = 0)))
    expect_snapshot_file(path, "plot_ancova.keppel")
    unlink(path)

    path <- save_png(plot(power.f.ancova.shieh(mu.vector = c(0.20, 0), sd.vector = rep(1, 2), n.vector = rep(150, 2),
                                               r.squared = 0.50, k.covariates = 1, alpha = 0.05, verbose = 0)))
    expect_snapshot_file(path, "plot_ancova.shieh")
    unlink(path)

    path <- save_png(plot(power.f.mixed.anova(eta.squared = 0.022, factor.levels = c(1, 2), rho.within = 0.50,
                                              effect = "within", power = 0.80, alpha = 0.05, verbose = 0)))
    expect_snapshot_file(path, "plot_mixed.anova")
    unlink(path)

    path <- save_png(plot(power.f.regression(r.squared = 0.15, k.total = 3, power = 0.80, verbose = 0)))
    expect_snapshot_file(path, "plot_regression.f")
    unlink(path)    

    # t: student, welch, wilcoxon, (t.)regression ----------------------------------------------------------------------
    path <- save_png(plot(power.t.student(d = 0.20, power = 0.80, alternative = "two.sided", design = "independent", verbose = 0)))
    expect_snapshot_file(path, "plot_student")
    unlink(path)    

    path <- save_png(plot(power.t.welch(d = 0.20, n.ratio = 2, var.ratio = 2, power = 0.80, alternative = "two.sided", verbose = 0)))
    expect_snapshot_file(path, "plot_welch")
    unlink(path)    
    
    path <- save_png(plot(power.np.wilcoxon(d = 0.25, power = 0.80, alternative = "two.sided", design = "independent", verbose = 0)))
    expect_snapshot_file(path, "plot_wilcoxon")
    unlink(path)

    path <- save_png(plot(power.t.regression(beta = 0.20, k.total = 5, r.squared = 0.30, power = 0.80, verbose = 0)))
    expect_snapshot_file(path, "plot_regression.t")
    unlink(path)

    # z: proportions.onetwo, correlations (steiger, twocors, onecor), logistic, poisson, mediation ---------------------
    path <- save_png(plot(power.z.oneprop(prob = 0.45, null.prob = 0.50, alpha = 0.05, n = 500, alternative = "one.sided", verbose = 0)))
    expect_snapshot_file(path, "plot_oneprop.z")
    unlink(path)

    path <- save_png(plot(power.z.twoprops(prob1 = 0.65, prob2 = 0.60, alpha = 0.05, n2 = 500, alternative = "one.sided", verbose = 0)))
    expect_snapshot_file(path, "plot_twoprops.z")
    unlink(path)

    path <- save_png(plot(power.z.twocors.steiger(rho12 = 0.35, rho13 = 0.45, rho23 = 0.05, power = 0.8, alpha = 0.05,
                                                  alternative = "two.sided", common.index = TRUE, verbose = 0)))
    expect_snapshot_file(path, "plot_twocors.steiger")
    unlink(path)

    path <- save_png(plot(power.z.twocors(rho1 = 0.20, rho2 = 0.30, power = 0.80, alpha = 0.05, alternative = "two.sided", verbose = 0)))
    expect_snapshot_file(path, "plot_twocors")
    unlink(path)

    path <- save_png(plot(power.z.onecor(rho = 0.20, power = 0.80, alpha = 0.05, alternative = "two.sided", verbose = 0)))
    expect_snapshot_file(path, "plot_onecor")
    unlink(path)

    path <- save_png(plot(power.z.logistic(base.prob = 0.15, prob = 0.20, alpha = 0.05, power = 0.80, distribution = "normal",
                                           verbose = 0)))
    expect_snapshot_file(path, "plot_logistic")
    unlink(path)

    path <- save_png(plot(power.z.mediation(beta.a = 0.25, beta.b = 0.25, beta.cp = 0.10, n = 200, verbose = 0)))
    expect_snapshot_file(path, "plot_mediation")
    unlink(path)

    path <- save_png(plot(power.z.poisson(beta0 = 0.50, beta1 = -0.10, alpha = 0.05, power = 0.80, dist = "normal", verbose = 0)))
    expect_snapshot_file(path, "plot_poisson")
    unlink(path)
    
    expect_error(plot(pwrss.z.mediation(a = 0.25, b = 0.25, cp = 0.10, n = 500, verbose = FALSE)),
                 "Plotting is no longer available for this type of object.")
    expect_error(plot(power.exact.fisher(prob1 = 0.60, prob2 = 0.40, n2 = 50, verbose = 0)),
                 "Plotting is not available for Fisher's or McNemar's exact test.")
    expect_error(plot.pwrss(NULL), "Not an object of the type 'pwrss'.")
})
