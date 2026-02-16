test_that("plots work", {
    if (!exists("create_snaps")) create_snaps <- FALSE

    # binomial-tests ---------------------------------------------------------------------------------------------------
    if (create_snaps || file.exists(file.path("_snaps", Sys.info()[["sysname"]], "plots/binom_plot_1"))) {
        path <- save_png(power.binom.test(size = 200, prob = 0.6, null.prob = 0.5, alpha = 0.05, alternative = "one.sided", verbose = 0))
        expect_snapshot_file(path, "binom_plot_1", variant = Sys.info()[["sysname"]])
        unlink(path)
    }

    if (create_snaps || file.exists(file.path("_snaps", Sys.info()[["sysname"]], "plots/binom_plot_2"))) {
        path <- save_png(power.binom.test(size = 200, prob = 0.4, null.prob = 0.5, alpha = 0.05, alternative = "one.sided", verbose = 0))
        expect_snapshot_file(path, "binom_plot_2", variant = Sys.info()[["sysname"]])
        unlink(path)
    }

    if (create_snaps || file.exists(file.path("_snaps", Sys.info()[["sysname"]], "plots/binom_plot_3"))) {
        path <- save_png(power.binom.test(size = 200, prob = 0.4, null.prob = 0.5, alpha = 0.05, alternative = "two.sided", verbose = 0))
        expect_snapshot_file(path, "binom_plot_3", variant = Sys.info()[["sysname"]])
        unlink(path)
    }

    if (create_snaps || file.exists(file.path("_snaps", Sys.info()[["sysname"]], "plots/binom_plot_4"))) {
        path <- save_png(power.binom.test(size = 200, prob = 0.5, null.prob = c(0.4, 0.6), alpha = 0.05, alternative = "two.one.sided", verbose = 0))
        expect_snapshot_file(path, "binom_plot_4", variant = Sys.info()[["sysname"]])
        unlink(path)
    }

    if (create_snaps || file.exists(file.path("_snaps", Sys.info()[["sysname"]], "plots/binom_plot_5"))) {
        path <- save_png(power.binom.test(size = 200, prob = 0.5, null.prob = c(0.3, 0.4), alpha = 0.05, alternative = "two.one.sided", verbose = 0))
        expect_snapshot_file(path, "binom_plot_5", variant = Sys.info()[["sysname"]])
        unlink(path)
    }
 
    if (create_snaps || file.exists(file.path("_snaps", Sys.info()[["sysname"]], "plots/binom_plot_6"))) {
        path <- save_png(.plot.binom.t1t2(size = 200, prob = 0.5, null.prob = 0.2, alpha = 0.05, alternative = "two.one.sided"))
        expect_snapshot_file(path, "binom_plot_6", variant = Sys.info()[["sysname"]])
        unlink(path)
    }

    expect_error(.plot.binom.t1t2(size = 5, prob = 0.4, alpha = 0.05, alternative = "two.sided"),
                 "Number of trials should be greater than 10 for plotting.")

    # chisq-tests ------------------------------------------------------------------------------------------------------
    if (create_snaps || file.exists(file.path("_snaps", Sys.info()[["sysname"]], "plots/chisq_plot_1"))) {
        path <- save_png(power.chisq.test(ncp = 20, df =    1, alpha = 0.05, verbose = 0))
        expect_snapshot_file(path, "chisq_plot_1", variant = Sys.info()[["sysname"]])
        unlink(path)
    }

    if (create_snaps || file.exists(file.path("_snaps", Sys.info()[["sysname"]], "plots/chisq_plot_2"))) {
        path <- save_png(power.chisq.test(ncp = 20, df =    2, alpha = 0.05, verbose = 0))
        expect_snapshot_file(path, "chisq_plot_2", variant = Sys.info()[["sysname"]])
        unlink(path)
    }

    if (create_snaps || file.exists(file.path("_snaps", Sys.info()[["sysname"]], "plots/chisq_plot_3"))) {
        path <- save_png(power.chisq.test(ncp = 20, df =    4, alpha = 0.05, verbose = 0))
        expect_snapshot_file(path, "chisq_plot_3", variant = Sys.info()[["sysname"]])
        unlink(path)
    }

    if (create_snaps || file.exists(file.path("_snaps", Sys.info()[["sysname"]], "plots/chisq_plot_4"))) {
        path <- save_png(power.chisq.test(ncp = 20, df =   10, alpha = 0.05, verbose = 0))
        expect_snapshot_file(path, "chisq_plot_4", variant = Sys.info()[["sysname"]])
        unlink(path)
    }

    if (create_snaps || file.exists(file.path("_snaps", Sys.info()[["sysname"]], "plots/chisq_plot_5"))) {
        path <- save_png(power.chisq.test(ncp = 20, df =  100, alpha = 0.05, verbose = 0))
        expect_snapshot_file(path, "chisq_plot_5", variant = Sys.info()[["sysname"]])
        unlink(path)
    }

    if (create_snaps || file.exists(file.path("_snaps", Sys.info()[["sysname"]], "plots/chisq_plot_6"))) {
        path <- save_png(power.chisq.test(ncp = 20, df = 1000, alpha = 0.05, verbose = 0))
        expect_snapshot_file(path, "chisq_plot_6", variant = Sys.info()[["sysname"]])
        unlink(path)
    }

    if (create_snaps || file.exists(file.path("_snaps", Sys.info()[["sysname"]], "plots/chisq_plot_7"))) {
        path <- save_png(power.chisq.test(ncp = 40, df =  100, alpha = 0.05, verbose = 0))
        expect_snapshot_file(path, "chisq_plot_7", variant = Sys.info()[["sysname"]])
        unlink(path)
    }

    expect_error(.plot.chisq.t1t2(ncp = 40, df = Inf),
                 "`df` must be numeric, finite, have a value of at least 1 and have a length of 1.")

    # F-tests ----------------------------------------------------------------------------------------------------------
    if (create_snaps || file.exists(file.path("_snaps", Sys.info()[["sysname"]], "plots/F_plot_01"))) {
        path <- save_png(power.f.test(ncp = 1, df1 = 1, df2 =   10, alpha = 0.05, verbose = 0))
        expect_snapshot_file(path, "F_plot_01", variant = Sys.info()[["sysname"]])
        unlink(path)
    }

    if (create_snaps || file.exists(file.path("_snaps", Sys.info()[["sysname"]], "plots/F_plot_02"))) {
        path <- save_png(power.f.test(ncp = 1, df1 = 1, df2 =  100, alpha = 0.05, verbose = 0))
        expect_snapshot_file(path, "F_plot_02", variant = Sys.info()[["sysname"]])
        unlink(path)
    }

    if (create_snaps || file.exists(file.path("_snaps", Sys.info()[["sysname"]], "plots/F_plot_03"))) {
        path <- save_png(power.f.test(ncp = 1, df1 = 1, df2 = 1000, alpha = 0.05, verbose = 0))
        expect_snapshot_file(path, "F_plot_03", variant = Sys.info()[["sysname"]])
        unlink(path)
    }

    if (create_snaps || file.exists(file.path("_snaps", Sys.info()[["sysname"]], "plots/F_plot_04"))) {
        path <- save_png(power.f.test(ncp = 2, df1 = 1, df2 =   10, alpha = 0.05, verbose = 0))
        expect_snapshot_file(path, "F_plot_04", variant = Sys.info()[["sysname"]])
        unlink(path)
    }

    if (create_snaps || file.exists(file.path("_snaps", Sys.info()[["sysname"]], "plots/F_plot_05"))) {
        path <- save_png(power.f.test(ncp = 2, df1 = 1, df2 =  100, alpha = 0.05, verbose = 0))
        expect_snapshot_file(path, "F_plot_05", variant = Sys.info()[["sysname"]])
        unlink(path)
    }

    if (create_snaps || file.exists(file.path("_snaps", Sys.info()[["sysname"]], "plots/F_plot_06"))) {
        path <- save_png(power.f.test(ncp = 2, df1 = 1, df2 = 1000, alpha = 0.05, verbose = 0))
        expect_snapshot_file(path, "F_plot_06", variant = Sys.info()[["sysname"]])
        unlink(path)
    }

    if (create_snaps || file.exists(file.path("_snaps", Sys.info()[["sysname"]], "plots/F_plot_07"))) {
        path <- save_png(power.f.test(ncp = 4, df1 = 1, df2 =   10, alpha = 0.05, verbose = 0))
        expect_snapshot_file(path, "F_plot_07", variant = Sys.info()[["sysname"]])
        unlink(path)
    }

    if (create_snaps || file.exists(file.path("_snaps", Sys.info()[["sysname"]], "plots/F_plot_08"))) {
        path <- save_png(power.f.test(ncp = 4, df1 = 1, df2 =  100, alpha = 0.05, verbose = 0))
        expect_snapshot_file(path, "F_plot_08", variant = Sys.info()[["sysname"]])
        unlink(path)
    }

    if (create_snaps || file.exists(file.path("_snaps", Sys.info()[["sysname"]], "plots/F_plot_09"))) {
        path <- save_png(power.f.test(ncp = 4, df1 = 1, df2 = 1000, alpha = 0.05, verbose = 0))
        expect_snapshot_file(path, "F_plot_09", variant = Sys.info()[["sysname"]])
        unlink(path)
    }

    if (create_snaps || file.exists(file.path("_snaps", Sys.info()[["sysname"]], "plots/F_plot_10"))) {
        path <- save_png(power.f.test(ncp = 8, df1 = 1, df2 =   10, alpha = 0.05, verbose = 0))
        expect_snapshot_file(path, "F_plot_10", variant = Sys.info()[["sysname"]])
        unlink(path)
    }

    if (create_snaps || file.exists(file.path("_snaps", Sys.info()[["sysname"]], "plots/F_plot_11"))) {
        path <- save_png(power.f.test(ncp = 8, df1 = 1, df2 =  100, alpha = 0.05, verbose = 0))
        expect_snapshot_file(path, "F_plot_11", variant = Sys.info()[["sysname"]])
        unlink(path)
    }

    if (create_snaps || file.exists(file.path("_snaps", Sys.info()[["sysname"]], "plots/F_plot_12"))) {
        path <- save_png(power.f.test(ncp = 8, df1 = 1, df2 = 1000, alpha = 0.05, verbose = 0))
        expect_snapshot_file(path, "F_plot_12", variant = Sys.info()[["sysname"]])
        unlink(path)
    }

    if (create_snaps || file.exists(file.path("_snaps", Sys.info()[["sysname"]], "plots/F_plot_13"))) {
        path <- save_png(power.f.test(ncp = 4, df1 = 2, df2 =   10, alpha = 0.05, verbose = 0))
        expect_snapshot_file(path, "F_plot_13", variant = Sys.info()[["sysname"]])
        unlink(path)
    }

    if (create_snaps || file.exists(file.path("_snaps", Sys.info()[["sysname"]], "plots/F_plot_14"))) {
        path <- save_png(power.f.test(ncp = 4, df1 = 2, df2 =  100, alpha = 0.05, verbose = 0))
        expect_snapshot_file(path, "F_plot_14", variant = Sys.info()[["sysname"]])
        unlink(path)
    }

    if (create_snaps || file.exists(file.path("_snaps", Sys.info()[["sysname"]], "plots/F_plot_15"))) {
        path <- save_png(power.f.test(ncp = 4, df1 = 2, df2 = 1000, alpha = 0.05, verbose = 0))
        expect_snapshot_file(path, "F_plot_15", variant = Sys.info()[["sysname"]])
        unlink(path)
    }

    if (create_snaps || file.exists(file.path("_snaps", Sys.info()[["sysname"]], "plots/F_plot_16"))) {
        path <- save_png(power.f.test(ncp = 8, df1 = 4, df2 =   10, alpha = 0.05, verbose = 0))
        expect_snapshot_file(path, "F_plot_16", variant = Sys.info()[["sysname"]])
        unlink(path)
    }

    if (create_snaps || file.exists(file.path("_snaps", Sys.info()[["sysname"]], "plots/F_plot_17"))) {
        path <- save_png(power.f.test(ncp = 8, df1 = 4, df2 =  100, alpha = 0.05, verbose = 0))
        expect_snapshot_file(path, "F_plot_17", variant = Sys.info()[["sysname"]])
        unlink(path)
    }

    if (create_snaps || file.exists(file.path("_snaps", Sys.info()[["sysname"]], "plots/F_plot_18"))) {
        path <- save_png(power.f.test(ncp = 8, df1 = 4, df2 = 1000, alpha = 0.05, verbose = 0))
        expect_snapshot_file(path, "F_plot_18", variant = Sys.info()[["sysname"]])
        unlink(path)
    }

    expect_error(.plot.f.t1t2(ncp = 1, df1 = 0, df2 = 10),
                 "`df1` must be numeric, finite, have a value of at least 1 and have a length of 1.")
    expect_error(.plot.f.t1t2(ncp = 1, df1 = 1, df2 = Inf),
                 "`df2` must be numeric, finite, have a value of at least 1 and have a length of 1.")    

    # t-tests ----------------------------------------------------------------------------------------------------------
    if (create_snaps || file.exists(file.path("_snaps", Sys.info()[["sysname"]], "plots/t_plot_1"))) {
        path <- save_png(power.t.test(ncp = 1.96, df = 100, alpha = 0.05, alternative = "two.sided", verbose = 0))
        expect_snapshot_file(path, "t_plot_1", variant = Sys.info()[["sysname"]])
        unlink(path)
    }

    if (create_snaps || file.exists(file.path("_snaps", Sys.info()[["sysname"]], "plots/t_plot_2"))) {
        path <- save_png(power.t.test(ncp = -1.96, df = 100, alpha = 0.05, alternative = "two.sided", verbose = 0))
        expect_snapshot_file(path, "t_plot_2", variant = Sys.info()[["sysname"]])
        unlink(path)
    }

    if (create_snaps || file.exists(file.path("_snaps", Sys.info()[["sysname"]], "plots/t_plot_3"))) {
        path <- save_png(power.t.test(ncp = 1.96, df = 100, alpha = 0.05, alternative = "one.sided", verbose = 0))
        expect_snapshot_file(path, "t_plot_3", variant = Sys.info()[["sysname"]])
        unlink(path)
    }

    if (create_snaps || file.exists(file.path("_snaps", Sys.info()[["sysname"]], "plots/t_plot_4"))) {
        path <- save_png(power.t.test(ncp = 0, null.ncp = c(-2, 2), df = 100, alpha = 0.05, alternative = "two.one.sided", verbose = 0))
        expect_snapshot_file(path, "t_plot_4", variant = Sys.info()[["sysname"]])
        unlink(path)
    }

    if (create_snaps || file.exists(file.path("_snaps", Sys.info()[["sysname"]], "plots/t_plot_5"))) {
        path <- save_png(power.t.test(ncp = 2, null.ncp = c(-1, 1), df = 100, alpha = 0.05, alternative = "two.one.sided", verbose = 0))
        expect_snapshot_file(path, "t_plot_5", variant = Sys.info()[["sysname"]])
        unlink(path)
    }

    # z-tests ----------------------------------------------------------------------------------------------------------
    if (create_snaps || file.exists(file.path("_snaps", Sys.info()[["sysname"]], "plots/z_plot_1"))) {
        path <- save_png(power.z.test(mean = 1.96, alpha = 0.05, alternative = "two.sided", verbose = 0))
        expect_snapshot_file(path, "z_plot_1", variant = Sys.info()[["sysname"]])
        unlink(path)
    }

    if (create_snaps || file.exists(file.path("_snaps", Sys.info()[["sysname"]], "plots/z_plot_2"))) {
        path <- save_png(power.z.test(mean = -1.96, alpha = 0.05, alternative = "two.sided", verbose = 0))
        expect_snapshot_file(path, "z_plot_2", variant = Sys.info()[["sysname"]])
        unlink(path)
    }

    if (create_snaps || file.exists(file.path("_snaps", Sys.info()[["sysname"]], "plots/z_plot_3"))) {
        path <- save_png(power.z.test(mean = 1.96, alpha = 0.05, alternative = "one.sided", verbose = 0))
        expect_snapshot_file(path, "z_plot_3", variant = Sys.info()[["sysname"]])
        unlink(path)
    }

    if (create_snaps || file.exists(file.path("_snaps", Sys.info()[["sysname"]], "plots/z_plot_4"))) {
        path <- save_png(power.z.test(mean = 0, null.mean = c(-2, 2), alpha = 0.05, alternative = "two.one.sided", verbose = 0))
        expect_snapshot_file(path, "z_plot_4", variant = Sys.info()[["sysname"]])
        unlink(path)
    }

    if (create_snaps || file.exists(file.path("_snaps", Sys.info()[["sysname"]], "plots/z_plot_5"))) {
        path <- save_png(power.z.test(mean = 2, null.mean = c(-1, 1), alpha = 0.05, alternative = "two.one.sided", verbose = 0))
        expect_snapshot_file(path, "z_plot_5", variant = Sys.info()[["sysname"]])
        unlink(path)
    }

    # plots.R (taking a results-obj and using it for plotting) ---------------------------------------------------------
    # binom: proportions.onetwo (only exact.oneprop) -------------------------------------------------------------------
    if (create_snaps || file.exists(file.path("_snaps", Sys.info()[["sysname"]], "plots/plot_oneprop.binom"))) {
        path <- save_png(plot(power.exact.oneprop(prob = 0.45, null.prob = 0.50, alpha = 0.05, n = 500, alternative = "one.sided", verbose = 0)))
        expect_snapshot_file(path, "plot_oneprop.binom", variant = Sys.info()[["sysname"]])
        unlink(path)
    }
    
    # chisq: chisq.gof -------------------------------------------------------------------------------------------------
    if (create_snaps || file.exists(file.path("_snaps", Sys.info()[["sysname"]], "plots/plot_gof"))) {
        mtxW <- probs.to.w(cbind(c(0.6759, 0.1559, 0.1281, 0.0323, 0.0078), c(0.6771, 0.1519, 0.1368, 0.0241, 0.0101)), verbose = 0)
        path <- save_png(plot(power.chisq.gof(w = mtxW$w, df = mtxW$df, power = 0.80, alpha = 0.05, verbose = 0)))
        expect_snapshot_file(path, "plot_gof", variant = Sys.info()[["sysname"]])
        unlink(path)
    }

    # F: ancova, keppel, shieh, mixed.anova, (f.)regression ------------------------------------------------------------
    if (create_snaps || file.exists(file.path("_snaps", Sys.info()[["sysname"]], "plots/plot_ancova"))) {
        path <- save_png(plot(power.f.ancova(eta.squared = 0.059, factor.levels = 2, alpha = 0.05, power = .80, verbose = 0)))
        expect_snapshot_file(path, "plot_ancova", variant = Sys.info()[["sysname"]])
        unlink(path)
    }

    if (create_snaps || file.exists(file.path("_snaps", Sys.info()[["sysname"]], "plots/plot_ancova.keppel"))) {
        path <- save_png(plot(power.f.ancova.keppel(mu.vector = c(0.50, 0.00), sd.vector = rep(1, 2), p.vector = rep(0.50, 2),
                                                    k.covariates = 1, r.squared = 0.50, alpha = 0.05, power = 0.80, verbose = 0)))
        expect_snapshot_file(path, "plot_ancova.keppel", variant = Sys.info()[["sysname"]])
        unlink(path)
    }

    if (create_snaps || file.exists(file.path("_snaps", Sys.info()[["sysname"]], "plots/plot_ancova.shieh"))) {
        path <- save_png(plot(power.f.ancova.shieh(mu.vector = c(0.20, 0), sd.vector = rep(1, 2), n.vector = rep(150, 2),
                                                   r.squared = 0.50, k.covariates = 1, alpha = 0.05, verbose = 0)))
        expect_snapshot_file(path, "plot_ancova.shieh", variant = Sys.info()[["sysname"]])
        unlink(path)
    }

    if (create_snaps || file.exists(file.path("_snaps", Sys.info()[["sysname"]], "plots/plot_mixed.anova"))) {
        path <- save_png(plot(power.f.mixed.anova(eta.squared = 0.022, factor.levels = c(1, 2), rho.within = 0.50,
                                                  effect = "within", power = 0.80, alpha = 0.05, verbose = 0)))
        expect_snapshot_file(path, "plot_mixed.anova", variant = Sys.info()[["sysname"]])
        unlink(path)
    }

    if (create_snaps || file.exists(file.path("_snaps", Sys.info()[["sysname"]], "plots/plot_regression.f"))) {
        path <- save_png(plot(power.f.regression(r.squared = 0.15, k.total = 3, power = 0.80, verbose = 0)))
        expect_snapshot_file(path, "plot_regression.f", variant = Sys.info()[["sysname"]])
        unlink(path)    
    }

    # t: student, welch, wilcoxon, (t.)regression ----------------------------------------------------------------------
    if (create_snaps || file.exists(file.path("_snaps", Sys.info()[["sysname"]], "plots/plot_student"))) {
        path <- save_png(plot(power.t.student(d = 0.20, power = 0.80, alternative = "two.sided", design = "independent", verbose = 0)))
        expect_snapshot_file(path, "plot_student", variant = Sys.info()[["sysname"]])
        unlink(path)    
    }

    if (create_snaps || file.exists(file.path("_snaps", Sys.info()[["sysname"]], "plots/plot_welch"))) {
        path <- save_png(plot(power.t.welch(d = 0.20, n.ratio = 2, var.ratio = 2, power = 0.80, alternative = "two.sided", verbose = 0)))
        expect_snapshot_file(path, "plot_welch", variant = Sys.info()[["sysname"]])
        unlink(path)    
    }

    if (create_snaps || file.exists(file.path("_snaps", Sys.info()[["sysname"]], "plots/plot_wilcoxon"))) {
        path <- save_png(plot(power.np.wilcoxon(d = 0.25, power = 0.80, alternative = "two.sided", design = "independent", verbose = 0)))
        expect_snapshot_file(path, "plot_wilcoxon", variant = Sys.info()[["sysname"]])
        unlink(path)
    }

    if (create_snaps || file.exists(file.path("_snaps", Sys.info()[["sysname"]], "plots/plot_regression.t"))) {
        path <- save_png(plot(power.t.regression(beta = 0.20, k.total = 5, r.squared = 0.30, power = 0.80, verbose = 0)))
        expect_snapshot_file(path, "plot_regression.t", variant = Sys.info()[["sysname"]])
        unlink(path)
    }

    # z: proportions.onetwo, correlations (steiger, twocors, onecor), logistic, poisson, mediation ---------------------
    if (create_snaps || file.exists(file.path("_snaps", Sys.info()[["sysname"]], "plots/plot_oneprop.z"))) {
        path <- save_png(plot(power.z.oneprop(prob = 0.45, null.prob = 0.50, alpha = 0.05, n = 500, alternative = "one.sided", verbose = 0)))
        expect_snapshot_file(path, "plot_oneprop.z", variant = Sys.info()[["sysname"]])
        unlink(path)
    }

    if (create_snaps || file.exists(file.path("_snaps", Sys.info()[["sysname"]], "plots/plot_twoprops.z"))) {
        path <- save_png(plot(power.z.twoprops(prob1 = 0.65, prob2 = 0.60, alpha = 0.05, n2 = 500, alternative = "one.sided", verbose = 0)))
        expect_snapshot_file(path, "plot_twoprops.z", variant = Sys.info()[["sysname"]])
        unlink(path)
    }

    if (create_snaps || file.exists(file.path("_snaps", Sys.info()[["sysname"]], "plots/plot_twocors.steiger"))) {
        path <- save_png(plot(power.z.twocors.steiger(rho12 = 0.35, rho13 = 0.45, rho23 = 0.05, power = 0.8, alpha = 0.05,
                                                      alternative = "two.sided", common.index = TRUE, verbose = 0)))
        expect_snapshot_file(path, "plot_twocors.steiger", variant = Sys.info()[["sysname"]])
        unlink(path)
    }

    if (create_snaps || file.exists(file.path("_snaps", Sys.info()[["sysname"]], "plots/plot_twocors"))) {
        path <- save_png(plot(power.z.twocors(rho1 = 0.20, rho2 = 0.30, power = 0.80, alpha = 0.05, alternative = "two.sided", verbose = 0)))
        expect_snapshot_file(path, "plot_twocors", variant = Sys.info()[["sysname"]])
        unlink(path)
    }

    if (create_snaps || file.exists(file.path("_snaps", Sys.info()[["sysname"]], "plots/plot_onecor"))) {
        path <- save_png(plot(power.z.onecor(rho = 0.20, power = 0.80, alpha = 0.05, alternative = "two.sided", verbose = 0)))
        expect_snapshot_file(path, "plot_onecor", variant = Sys.info()[["sysname"]])
        unlink(path)
    }

    if (create_snaps || file.exists(file.path("_snaps", Sys.info()[["sysname"]], "plots/plot_logistic"))) {
        path <- save_png(plot(power.z.logistic(base.prob = 0.15, prob = 0.20, alpha = 0.05, power = 0.80, distribution = "normal",
                                               verbose = 0)))
        expect_snapshot_file(path, "plot_logistic", variant = Sys.info()[["sysname"]])
        unlink(path)
    }

    if (create_snaps || file.exists(file.path("_snaps", Sys.info()[["sysname"]], "plots/plot_mediation"))) {
        path <- save_png(plot(power.z.mediation(beta.a = 0.25, beta.b = 0.25, beta.cp = 0.10, n = 200, verbose = 0)))
        expect_snapshot_file(path, "plot_mediation", variant = Sys.info()[["sysname"]])
        unlink(path)
    }

    if (create_snaps || file.exists(file.path("_snaps", Sys.info()[["sysname"]], "plots/plot_poisson"))) {
        path <- save_png(plot(power.z.poisson(beta0 = 0.50, beta1 = -0.10, alpha = 0.05, power = 0.80, dist = "normal", verbose = 0)))
        expect_snapshot_file(path, "plot_poisson", variant = Sys.info()[["sysname"]])
        unlink(path)
    }
    
    expect_error(plot(pwrss.z.mediation(a = 0.25, b = 0.25, cp = 0.10, n = 500, verbose = FALSE)),
                 "Plotting is no longer available for this type of object.")
    expect_error(plot(power.exact.fisher(prob1 = 0.60, prob2 = 0.40, n2 = 50, verbose = 0)),
                 "Plotting is not available for Fisher's or McNemar's exact test.")
    expect_error(plot.pwrss(NULL), "Not an object of the type 'pwrss'.")
})
