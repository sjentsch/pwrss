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
})
