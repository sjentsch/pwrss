test_that("correlations.R works", {
    # power.z.steiger (= power.z.twocors.steiger [the latter is exported]) ---------------------------------------------
    crrRes <- power.z.twocors.steiger(rho12 = 0.35, rho13 = 0.45, rho23 = 0.05, power = 0.8, alpha = 0.05,
                                      alternative = "two.sided", common.index = TRUE, verbose = 0)
    expect_equal(class(crrRes), c("pwrss", "z", "twocors", "paired"))
    expect_equal(names(crrRes), c("parms", "test", "design", "delta", "q", "mean", "sd", "null.mean", "null.sd", "alternative",
                                  "z.alpha", "n", "power"))
    expect_equal(crrRes[["parms"]], list(rho12 = 0.35, rho13 = 0.45, rho23 = 0.05, rho14 = NULL, rho24 = NULL, rho34 = NULL, alpha = 0.05,
                                         alternative = "two.sided", pooled = TRUE, common.index = TRUE, ceiling = TRUE, verbose = 0, utf = FALSE))
    expect_equal(crrRes[c("test", "design", "delta", "q", "mean", "sd", "null.mean", "null.sd", "alternative", "z.alpha", "n", "power")],
                 list(test = "z", design = "paired", delta = -0.1, q = -0.119256524, mean = -2.8011469, sd = 0.999426958, null.mean = 0,
                      null.sd = 1, alternative = "two.sided", z.alpha = c(-1.959964, 1.959964), n = 1138, power = 0.80001327))

    crrRes <- power.z.twocors.steiger(rho12 = 0.35, rho13 = 0.45, rho23 = 0.05, n = 1000, alpha = 0.05,
                                      alternative = "two.sided", common.index = TRUE, verbose = 0)
    expect_equal(class(crrRes), c("pwrss", "z", "twocors", "paired"))
    expect_equal(names(crrRes), c("parms", "test", "design", "delta", "q", "mean", "sd", "null.mean", "null.sd", "alternative",
                                  "z.alpha", "n", "power"))
    expect_equal(crrRes[["parms"]], list(rho12 = 0.35, rho13 = 0.45, rho23 = 0.05, rho14 = NULL, rho24 = NULL, rho34 = NULL, alpha = 0.05,
                                         alternative = "two.sided", pooled = TRUE, common.index = TRUE, ceiling = TRUE, verbose = 0, utf = FALSE))
    expect_equal(crrRes[c("test", "design", "delta", "q", "mean", "sd", "null.mean", "null.sd", "alternative", "z.alpha", "n", "power")],
                 list(test = "z", design = "paired", delta = -0.1, q = -0.119256524, mean = -2.62533987, sd = 0.999426958, null.mean = 0,
                      null.sd = 1, alternative = "two.sided", z.alpha = c(-1.959964, 1.959964), n = 1000, power = 0.747219145))


    crrRes <- power.z.twocors.steiger(rho12 = 0.45, rho13 = 0.45, rho23 = 0.50, rho14 = 0.50, rho24 = 0.80, rho34 = 0.55, power = 0.8,
                                      alpha = 0.05, alternative = "two.sided", common.index = FALSE, verbose = 0)
    expect_equal(class(crrRes), c("pwrss", "z", "twocors", "paired"))
    expect_equal(names(crrRes), c("parms", "test", "design", "delta", "q", "mean", "sd", "null.mean", "null.sd", "alternative",
                                  "z.alpha", "n", "power"))
    expect_equal(crrRes[["parms"]], list(rho12 = 0.45, rho13 = 0.45, rho23 = 0.50, rho14 = 0.50, rho24 = 0.80, rho34 = 0.55,
                                         alpha = 0.05, alternative = "two.sided", pooled = TRUE, common.index = FALSE,
                                         ceiling = TRUE, verbose = 0, utf = FALSE))
    expect_equal(crrRes[c("test", "design", "delta", "q", "mean", "sd", "null.mean", "null.sd", "alternative", "z.alpha", "n", "power")],
                 list(test = "z", design = "paired", delta = -0.1, q = -0.133681035, mean = -2.80207461, sd = 0.99997767, null.mean = 0,
                      null.sd = 1, alternative = "two.sided", z.alpha = c(-1.959964, 1.959964), n = 643, power = 0.8001432))

    crrRes <- power.z.twocors.steiger(rho12 = 0.45, rho13 = 0.45, rho23 = 0.50, rho14 = 0.50, rho24 = 0.80, rho34 = 0.55, n = 643,
                                      alpha = 0.05, alternative = "two.sided", common.index = FALSE, verbose = 0)
    expect_equal(class(crrRes), c("pwrss", "z", "twocors", "paired"))
    expect_equal(names(crrRes), c("parms", "test", "design", "delta", "q", "mean", "sd", "null.mean", "null.sd", "alternative",
                                  "z.alpha", "n", "power"))
    expect_equal(crrRes[["parms"]], list(rho12 = 0.45, rho13 = 0.45, rho23 = 0.50, rho14 = 0.50, rho24 = 0.80, rho34 = 0.55,
                                         alpha = 0.05, alternative = "two.sided", pooled = TRUE, common.index = FALSE,
                                         ceiling = TRUE, verbose = 0, utf = FALSE))
    expect_equal(crrRes[c("test", "design", "delta", "q", "mean", "sd", "null.mean", "null.sd", "alternative", "z.alpha", "n", "power")],
                 list(test = "z", design = "paired", delta = -0.1, q = -0.133681035, mean = -2.80207461, sd = 0.99997767, null.mean = 0,
                      null.sd = 1, alternative = "two.sided", z.alpha = c(-1.959964, 1.959964), n = 643, power = 0.8001432))

    crrRes <- power.z.twocors.steiger(rho12 = 0.45, rho13 = 0.45, rho23 = 0.50, rho14 = 0.50, rho24 = 0.80, rho34 = 0.55, power = 0.8,
                                      alpha = 0.05, alternative = "two.sided", pooled = FALSE, common.index = FALSE, verbose = 0)
    expect_equal(class(crrRes), c("pwrss", "z", "twocors", "paired"))
    expect_equal(names(crrRes), c("parms", "test", "design", "delta", "q", "mean", "sd", "null.mean", "null.sd", "alternative",
                                  "z.alpha", "n", "power"))
    expect_equal(crrRes[["parms"]], list(rho12 = 0.45, rho13 = 0.45, rho23 = 0.50, rho14 = 0.50, rho24 = 0.80, rho34 = 0.55,
                                         alpha = 0.05, alternative = "two.sided", pooled = FALSE, common.index = FALSE,
                                         ceiling = TRUE, verbose = 0, utf = FALSE))
    expect_equal(crrRes[c("test", "design", "delta", "q", "mean", "sd", "null.mean", "null.sd", "alternative", "z.alpha", "n", "power")],
                 list(test = "z", design = "paired", delta = -0.1, q = -0.133681035, mean = -2.81260658, sd = 1.011671015, null.mean = 0,
                      null.sd = 1, alternative = "two.sided", z.alpha = c(-1.959964, 1.959964), n = 633, power = 0.800332771))

    crrRes <- power.z.twocors.steiger(rho12 = 0.45, rho13 = 0.45, rho23 = 0.50, rho14 = 0.50, rho24 = 0.80, rho34 = 0.55, n = 633,
                                      alpha = 0.05, alternative = "two.sided", pooled = FALSE, common.index = FALSE, verbose = 0)
    expect_equal(class(crrRes), c("pwrss", "z", "twocors", "paired"))
    expect_equal(names(crrRes), c("parms", "test", "design", "delta", "q", "mean", "sd", "null.mean", "null.sd", "alternative",
                                  "z.alpha", "n", "power"))
    expect_equal(crrRes[["parms"]], list(rho12 = 0.45, rho13 = 0.45, rho23 = 0.50, rho14 = 0.50, rho24 = 0.80, rho34 = 0.55,
                                         alpha = 0.05, alternative = "two.sided", pooled = FALSE, common.index = FALSE,
                                         ceiling = TRUE, verbose = 0, utf = FALSE))
    expect_equal(crrRes[c("test", "design", "delta", "q", "mean", "sd", "null.mean", "null.sd", "alternative", "z.alpha", "n", "power")],
                 list(test = "z", design = "paired", delta = -0.1, q = -0.133681035, mean = -2.81260658, sd = 1.011671015, null.mean = 0,
                      null.sd = 1, alternative = "two.sided", z.alpha = c(-1.959964, 1.959964), n = 633, power = 0.800332771))

    crrRes <- power.z.twocors.steiger(rho12 = 0.35, rho13 = 0.45, rho23 = 0.05, power = 0.8, alpha = 0.05,
                                      alternative = "two.sided", pooled = FALSE, common.index = TRUE, verbose = 0)
    expect_equal(class(crrRes), c("pwrss", "z", "twocors", "paired"))
    expect_equal(names(crrRes), c("parms", "test", "design", "delta", "q", "mean", "sd", "null.mean", "null.sd", "alternative",
                                  "z.alpha", "n", "power"))
    expect_equal(crrRes[["parms"]], list(rho12 = 0.35, rho13 = 0.45, rho23 = 0.05, rho14 = NULL, rho24 = NULL, rho34 = NULL, alpha = 0.05,
                                         alternative = "two.sided", pooled = FALSE, common.index = TRUE, ceiling = TRUE, verbose = 0, utf = FALSE))
    expect_equal(crrRes[c("test", "design", "delta", "q", "mean", "sd", "null.mean", "null.sd", "alternative", "z.alpha", "n", "power")],
                 list(test = "z", design = "paired", delta = -0.1, q = -0.119256524, mean = -2.80946813, sd = 1.00818629, null.mean = 0,
                      null.sd = 1, alternative = "two.sided", z.alpha = c(-1.959964, 1.959964), n = 1125, power = 0.8002768))


    crrRes <- power.z.twocors.steiger(rho12 = 0.35, rho13 = 0.45, rho23 = 0.05, n = 1125, alpha = 0.05,
                                      alternative = "two.sided", pooled = FALSE, common.index = TRUE, verbose = 0)
    expect_equal(class(crrRes), c("pwrss", "z", "twocors", "paired"))
    expect_equal(names(crrRes), c("parms", "test", "design", "delta", "q", "mean", "sd", "null.mean", "null.sd", "alternative",
                                  "z.alpha", "n", "power"))
    expect_equal(crrRes[["parms"]], list(rho12 = 0.35, rho13 = 0.45, rho23 = 0.05, rho14 = NULL, rho24 = NULL, rho34 = NULL, alpha = 0.05,
                                         alternative = "two.sided", pooled = FALSE, common.index = TRUE, ceiling = TRUE, verbose = 0, utf = FALSE))
    expect_equal(crrRes[c("test", "design", "delta", "q", "mean", "sd", "null.mean", "null.sd", "alternative", "z.alpha", "n", "power")],
                 list(test = "z", design = "paired", delta = -0.1, q = -0.119256524, mean = -2.80946813, sd = 1.00818629, null.mean = 0,
                      null.sd = 1, alternative = "two.sided", z.alpha = c(-1.959964, 1.959964), n = 1125, power = 0.8002768))

    # example 28.3 from the GPower manual (no common index)
    crrRes <- power.z.twocors.steiger(rho12 = 0.1, rho13 = 0.5, rho23 = -0.4, rho14 = 0.4, rho24 = 0.8, rho34 = 0.2, power = 0.8,
                                      alpha = 0.05, alternative = "one.sided", pooled = FALSE, common.index = FALSE, verbose = 0)
    expect_equal(class(crrRes), c("pwrss", "z", "twocors", "paired"))
    expect_equal(names(crrRes), c("parms", "test", "design", "delta", "q", "mean", "sd", "null.mean", "null.sd", "alternative",
                                  "z.alpha", "n", "power"))
    expect_equal(crrRes[["parms"]],
                 list(rho12 = 0.1, rho13 = 0.5, rho23 = -0.4, rho14 = 0.4, rho24 = 0.8, rho34 = 0.2, alpha = 0.05,
                      alternative = "one.sided", pooled = FALSE, common.index = FALSE, ceiling = TRUE, verbose = 0, utf = FALSE))
    expect_equal(crrRes[c("test", "design", "delta", "q", "mean", "sd", "null.mean", "null.sd", "alternative", "z.alpha", "n", "power")],
                 list(test = "z", design = "paired", delta = -0.1, q = -0.102397206, mean = -2.48614145, sd = 0.99894152,
                      null.mean = 0, null.sd = 1, alternative = "one.sided", z.alpha = -1.64485363, n = 886, power = 0.80015619))
    # results are nearly identical: z crit = -1.644854, power ~ 0.8001, n = 886

    # example 28.3 from the GPower manual (no common index; rho13 = rho14 = rho23 = rho24 = 0?)
    crrRes <- power.z.twocors.steiger(rho12 = 0.1, rho13 = 0, rho23 = 0, rho14 = 0, rho24 = 0, rho34 = 0.2, power = 0.8,
                                      alpha = 0.05, alternative = "one.sided", pooled = FALSE, common.index = FALSE, verbose = 0)
    expect_equal(class(crrRes), c("pwrss", "z", "twocors", "paired"))
    expect_equal(names(crrRes), c("parms", "test", "design", "delta", "q", "mean", "sd", "null.mean", "null.sd", "alternative",
                                  "z.alpha", "n", "power"))
    expect_equal(crrRes[["parms"]],
                 list(rho12 = 0.1, rho13 = 0, rho23 = 0, rho14 = 0, rho24 = 0, rho34 = 0.2, alpha = 0.05,
                      alternative = "one.sided", pooled = FALSE, common.index = FALSE, ceiling = TRUE, verbose = 0, utf = FALSE))
    expect_equal(crrRes[c("test", "design", "delta", "q", "mean", "sd", "null.mean", "null.sd", "alternative", "z.alpha", "n", "power")],
                 list(test = "z", design = "paired", delta = -0.1, q = -0.102397206, mean = -2.4872195, sd = 1,
                      null.mean = 0, null.sd = 1, alternative = "one.sided", z.alpha = -1.64485363, n = 1183, power = 0.80020841))
    expect_equal(crrRes[["n"]], power.z.twocors(rho1 = 0.1, rho2 = 0.2, power = 0.8, alternative = "one.sided", verbose = 0)[["n"]][[1]])
    # result is identical: n = 1183, as does the suggestion to use the procedure for independent correlations with a n.ratio of 1

    # example 28.3 from the GPower manual (common index)
    crrRes <- power.z.twocors.steiger(rho12 = 0.4, rho13 = 0.2, rho23 = 0.5, power = 0.8, alpha = 0.05, alternative = "one.sided",
                                      pooled = FALSE, common.index = TRUE, verbose = 0)
    expect_equal(class(crrRes), c("pwrss", "z", "twocors", "paired"))
    expect_equal(names(crrRes), c("parms", "test", "design", "delta", "q", "mean", "sd", "null.mean", "null.sd", "alternative",
                                  "z.alpha", "n", "power"))
    expect_equal(crrRes[["parms"]],
                 list(rho12 = 0.4, rho13 = 0.2, rho23 = 0.5, rho14 = NULL, rho24 = NULL, rho34 = NULL, alpha = 0.05,
                      alternative = "one.sided", pooled = FALSE, common.index = TRUE, ceiling = TRUE, verbose = 0, utf = FALSE))
    expect_equal(crrRes[c("test", "design", "delta", "q", "mean", "sd", "null.mean", "null.sd", "alternative", "z.alpha", "n", "power")],
                 list(test = "z", design = "paired", delta = 0.2, q = 0.220916376, mean = 2.4636089, sd = 0.96805217,
                      null.mean = 0, null.sd = 1, alternative = "one.sided", z.alpha = 1.64485363, n = 144, power = 0.80116113))
    # results are identical: z crit = -1.644854, power ~ 0.801161, n = 144

    # example 28.3 from the GPower manual (adapted from sensitivity analysis, rho13 is given as parameter but estimated in GPower)
    crrRes <- power.z.twocors.steiger(rho12 = 0.4, rho13 = 0.047702, rho23 = -0.6, power = 0.8, alpha = 0.05, alternative = "one.sided",
                                      pooled = FALSE, common.index = TRUE, verbose = 0)
    expect_equal(class(crrRes), c("pwrss", "z", "twocors", "paired"))
    expect_equal(names(crrRes), c("parms", "test", "design", "delta", "q", "mean", "sd", "null.mean", "null.sd", "alternative",
                                  "z.alpha", "n", "power"))
    expect_equal(crrRes[["parms"]],
                 list(rho12 = 0.4, rho13 = 0.047702, rho23 = -0.6, rho14 = NULL, rho24 = NULL, rho34 = NULL, alpha = 0.05,
                      alternative = "one.sided", pooled = FALSE, common.index = TRUE, ceiling = TRUE, verbose = 0, utf = FALSE))
    expect_equal(crrRes[c("test", "design", "delta", "q", "mean", "sd", "null.mean", "null.sd", "alternative", "z.alpha", "n", "power")],
                 list(test = "z", design = "paired", delta = 0.352298, q = 0.3759107, mean = 2.48403976, sd = 0.997104865,
                      null.mean = 0, null.sd = 1, alternative = "one.sided", z.alpha = 1.64485363, n = 144, power = 0.80000042))
    # results are identical: z crit = -1.644854, power ~ 0.80, n = 144

    expect_error(power.z.twocors.steiger(rho12 = 2e-4, rho13 = 1e-4, rho23 = 0.01, power = 1 - 1e-8, alpha = 1e-8,
                                         alternative = "two.sided", common.index = TRUE),
                 "Design is not feasible.")
    expect_error(power.z.twocors.steiger(rho12 = 0.01, rho13 = 0.01, rho23 = 0.02, power = 0.8, alpha = 0.05,
                                         alternative = "two.sided", common.index = TRUE),
                 "`common.index` is TRUE and `alternative` is \"two.sided\" but `rho12` = `rho13`.")
    expect_error(power.z.twocors.steiger(rho12 = 0.45, rho13 = 0.45, rho23 = 0.50, rho14 = 0.50, rho24 = 0.80, rho34 = 0.45, power = 0.8,
                                         alpha = 0.05, alternative = "two.sided", common.index = FALSE, verbose = 0),
                 "`common.index` is FALSE and `alternative` = \"two.sided\" but `rho12` = `rho34`.")

    expect_warning(power.z.twocors.steiger(rho12 = 0.1, rho13 = 0.2, rho23 = 0.3, rho14 = 0.4, power = 0.8, alpha = 0.05,
                                         alternative = "two.sided", common.index = TRUE, verbose = 0),
                   "Ignoring `rho14` `rho24`, or `rho34` because `common.index` is TRUE.")

    # power.z.twocors (= pwrss.z.2cors) --------------------------------------------------------------------------------
    crrRes <- power.z.twocors(rho1 = 0.20, rho2 = 0.30, power = 0.80, alpha = 0.05, alternative = "two.sided", verbose = 0)
    expect_equal(class(crrRes), c("pwrss", "z", "twocors", "independent"))
    expect_equal(names(crrRes), c("parms", "test", "design", "delta", "q", "mean", "sd", "null.mean", "null.sd", "alternative",
                                  "z.alpha", "n", "power"))
    expect_equal(crrRes[["parms"]], list(rho1 = 0.20, rho2 = 0.30, n.ratio = 1, alpha = 0.05, alternative = "two.sided",
                                         ceiling = TRUE, verbose = 0, utf = FALSE))
    expect_equal(crrRes[c("test", "design", "delta", "q", "mean", "sd", "null.mean", "null.sd", "alternative", "z.alpha", "n", "power")],
                 list(test = "z", design = "independent", delta = -0.1, q = -0.10678705, mean = -2.80201569, sd = 1,
                      null.mean = 0, null.sd = 1, alternative = "two.sided", z.alpha = c(-1.959964, 1.959964),
                      n = c(n1 = 1380, n2 = 1380), power = 0.800121451))
    expect_equal(crrRes, pwrss.z.2corrs(r1 = 0.20, r2 = 0.30, power = 0.80, alpha = 0.05, alternative = "not equal", verbose = FALSE))

    crrRes <- power.z.twocors(rho1 = 0.20, rho2 = 0.30, n2 = 1380, alpha = 0.05, alternative = "two.sided", verbose = 0)
    expect_equal(class(crrRes), c("pwrss", "z", "twocors", "independent"))
    expect_equal(names(crrRes), c("parms", "test", "design", "delta", "q", "mean", "sd", "null.mean", "null.sd", "alternative",
                                  "z.alpha", "n", "power"))
    expect_equal(crrRes[["parms"]], list(rho1 = 0.20, rho2 = 0.30, n.ratio = 1, alpha = 0.05, alternative = "two.sided",
                                         ceiling = TRUE, verbose = 0, utf = FALSE))
    expect_equal(crrRes[c("test", "design", "delta", "q", "mean", "sd", "null.mean", "null.sd", "alternative", "z.alpha", "n", "power")],
                 list(test = "z", design = "independent", delta = -0.1, q = -0.10678705, mean = -2.80201569, sd = 1,
                      null.mean = 0, null.sd = 1, alternative = "two.sided", z.alpha = c(-1.959964, 1.959964),
                      n = c(n1 = 1380, n2 = 1380), power = 0.800121451))
    expect_equal(crrRes, pwrss.z.2corrs(r1 = 0.20, r2 = 0.30, n2 = 1380, alpha = 0.05, alternative = "not equal", verbose = FALSE))

    crrRes <- power.z.twocors(rho1 = 0.30, rho2 = 0.20, power = 0.80, alpha = 0.05, alternative = "one.sided", verbose = 0)
    expect_equal(class(crrRes), c("pwrss", "z", "twocors", "independent"))
    expect_equal(names(crrRes), c("parms", "test", "design", "delta", "q", "mean", "sd", "null.mean", "null.sd", "alternative",
                                  "z.alpha", "n", "power"))
    expect_equal(crrRes[["parms"]], list(rho1 = 0.30, rho2 = 0.20, n.ratio = 1, alpha = 0.05, alternative = "one.sided",
                                         ceiling = TRUE, verbose = 0, utf = FALSE))
    expect_equal(crrRes[c("test", "design", "delta", "q", "mean", "sd", "null.mean", "null.sd", "alternative", "z.alpha", "n", "power")],
                 list(test = "z", design = "independent", delta = 0.1, q = 0.10678705, mean = 2.4872444, sd = 1,
                      null.mean = 0, null.sd = 1, alternative = "one.sided", z.alpha = 1.64485363,
                      n = c(n1 = 1088, n2 = 1088), power = 0.80021537))
    expect_equal(crrRes, pwrss.z.2corrs(r1 = 0.30, r2 = 0.20, power = 0.80, alpha = 0.05, alternative = "less", verbose = FALSE))

    crrRes <- power.z.twocors(rho1 = 0.30, rho2 = 0.20, n2 = 1088, alpha = 0.05, alternative = "one.sided", verbose = 0)
    expect_equal(class(crrRes), c("pwrss", "z", "twocors", "independent"))
    expect_equal(names(crrRes), c("parms", "test", "design", "delta", "q", "mean", "sd", "null.mean", "null.sd", "alternative",
                                  "z.alpha", "n", "power"))
    expect_equal(crrRes[["parms"]], list(rho1 = 0.30, rho2 = 0.20, n.ratio = 1, alpha = 0.05, alternative = "one.sided",
                                         ceiling = TRUE, verbose = 0, utf = FALSE))
    expect_equal(crrRes[c("test", "design", "delta", "q", "mean", "sd", "null.mean", "null.sd", "alternative", "z.alpha", "n", "power")],
                 list(test = "z", design = "independent", delta = 0.1, q = 0.10678705, mean = 2.4872444, sd = 1,
                      null.mean = 0, null.sd = 1, alternative = "one.sided", z.alpha = 1.64485363,
                      n = c(n1 = 1088, n2 = 1088), power = 0.80021537))

    crrRes <- power.z.twocors(rho1 = 0.30, rho2 = 0.20, n.ratio = 2, power = 0.80, alpha = 0.05, alternative = "two.sided", verbose = 0)
    expect_equal(class(crrRes), c("pwrss", "z", "twocors", "independent"))
    expect_equal(names(crrRes), c("parms", "test", "design", "delta", "q", "mean", "sd", "null.mean", "null.sd", "alternative",
                                  "z.alpha", "n", "power"))
    expect_equal(crrRes[["parms"]], list(rho1 = 0.30, rho2 = 0.20, n.ratio = 2, alpha = 0.05, alternative = "two.sided",
                                         ceiling = TRUE, verbose = 0, utf = FALSE))
    expect_equal(crrRes[c("test", "design", "delta", "q", "mean", "sd", "null.mean", "null.sd", "alternative", "z.alpha", "n", "power")],
                 list(test = "z", design = "independent", delta = 0.1, q = 0.10678705, mean = 2.80167586, sd = 1,
                      null.mean = 0, null.sd = 1, alternative = "two.sided", z.alpha = c(-1.959964, 1.959964),
                      n = c(n1 = 2070, n2 = 1035), power = 0.800026336))

    crrRes <- power.z.twocors(rho1 = 0.30, rho2 = 0.20, n.ratio = 2, n2 = 1035, alpha = 0.05, alternative = "two.sided", verbose = 0)
    expect_equal(class(crrRes), c("pwrss", "z", "twocors", "independent"))
    expect_equal(names(crrRes), c("parms", "test", "design", "delta", "q", "mean", "sd", "null.mean", "null.sd", "alternative",
                                  "z.alpha", "n", "power"))
    expect_equal(crrRes[["parms"]], list(rho1 = 0.30, rho2 = 0.20, n.ratio = 2, alpha = 0.05, alternative = "two.sided",
                                         ceiling = TRUE, verbose = 0, utf = FALSE))
    expect_equal(crrRes[c("test", "design", "delta", "q", "mean", "sd", "null.mean", "null.sd", "alternative", "z.alpha", "n", "power")],
                 list(test = "z", design = "independent", delta = 0.1, q = 0.10678705, mean = 2.80167586, sd = 1,
                      null.mean = 0, null.sd = 1, alternative = "two.sided", z.alpha = c(-1.959964, 1.959964),
                      n = c(n1 = 2070, n2 = 1035), power = 0.800026336))

    crrRes <- power.z.twocors(rho1 = 0.20, rho2 = 0.30, n.ratio = 1 / 2, power = 0.80, alpha = 0.05, alternative = "one.sided", verbose = 0)
    expect_equal(class(crrRes), c("pwrss", "z", "twocors", "independent"))
    expect_equal(names(crrRes), c("parms", "test", "design", "delta", "q", "mean", "sd", "null.mean", "null.sd", "alternative",
                                  "z.alpha", "n", "power"))
    expect_equal(crrRes[["parms"]], list(rho1 = 0.20, rho2 = 0.30, n.ratio = 1 / 2, alpha = 0.05, alternative = "one.sided",
                                         ceiling = TRUE, verbose = 0, utf = FALSE))
    expect_equal(crrRes[c("test", "design", "delta", "q", "mean", "sd", "null.mean", "null.sd", "alternative", "z.alpha", "n", "power")],
                 list(test = "z", design = "independent", delta = -0.1, q = -0.10678705, mean = -2.4868614, sd = 1,
                      null.mean = 0, null.sd = 1, alternative = "one.sided", z.alpha = -1.64485363,
                      n = c(n1 = 816, n2 = 1632), power = 0.8001082))

    crrRes <- power.z.twocors(rho1 = 0.20, rho2 = 0.30, n.ratio = 1 / 2, n2 = 1632, alpha = 0.05, alternative = "one.sided", verbose = 0)
    expect_equal(class(crrRes), c("pwrss", "z", "twocors", "independent"))
    expect_equal(names(crrRes), c("parms", "test", "design", "delta", "q", "mean", "sd", "null.mean", "null.sd", "alternative",
                                  "z.alpha", "n", "power"))
    expect_equal(crrRes[["parms"]], list(rho1 = 0.20, rho2 = 0.30, n.ratio = 1 / 2, alpha = 0.05, alternative = "one.sided",
                                         ceiling = TRUE, verbose = 0, utf = FALSE))
    expect_equal(crrRes[c("test", "design", "delta", "q", "mean", "sd", "null.mean", "null.sd", "alternative", "z.alpha", "n", "power")],
                 list(test = "z", design = "independent", delta = -0.1, q = -0.10678705, mean = -2.4868614, sd = 1,
                      null.mean = 0, null.sd = 1, alternative = "one.sided", z.alpha = -1.64485363,
                      n = c(n1 = 816, n2 = 1632), power = 0.8001082))

    # example 27.3 from the GPower manual, taken from Cohen (1988, p. 131)
    crrRes <- power.z.twocors(rho1 = 0.75, rho2 = 0.88, n.ratio = 51 / 260, n2 = 260, verbose = 0)
    expect_equal(class(crrRes), c("pwrss", "z", "twocors", "independent"))
    expect_equal(names(crrRes), c("parms", "test", "design", "delta", "q", "mean", "sd", "null.mean", "null.sd", "alternative",
                                  "z.alpha", "n", "power"))
    expect_equal(crrRes[["parms"]], list(rho1 = 0.75, rho2 = 0.88, n.ratio = 51 / 260, alpha = 0.05, alternative = "two.sided",
                                         ceiling = TRUE, verbose = 0, utf = FALSE))
    expect_equal(crrRes[c("test", "design", "delta", "q", "mean", "sd", "null.mean", "null.sd", "alternative", "z.alpha", "n", "power")],
                 list(test = "z", design = "independent", delta = -0.13, q = -0.402812582, mean = -2.5617709, sd = 1,
                      null.mean = 0, null.sd = 1, alternative = "two.sided", z.alpha = 1.95996398 * c(-1, 1),
                      n = c(n1 = 51, n2 = 260), power = 0.72635173))
    # results are identical: q ~ -0.4028126, z / x² crit = -1.959964, power ~ 0.726352

    # example 27.3 from the GPower manual, using the power from above and assuming equally-sized groups
    crrRes <- power.z.twocors(rho1 = 0.75, rho2 = 0.88, power = 0.72635173, verbose = 0)
    expect_equal(class(crrRes), c("pwrss", "z", "twocors", "independent"))
    expect_equal(names(crrRes), c("parms", "test", "design", "delta", "q", "mean", "sd", "null.mean", "null.sd", "alternative",
                                  "z.alpha", "n", "power"))
    expect_equal(crrRes[["parms"]], list(rho1 = 0.75, rho2 = 0.88, n.ratio = 1, alpha = 0.05, alternative = "two.sided",
                                         ceiling = TRUE, verbose = 0, utf = FALSE))
    expect_equal(crrRes[c("test", "design", "delta", "q", "mean", "sd", "null.mean", "null.sd", "alternative", "z.alpha", "n", "power")],
                 list(test = "z", design = "independent", delta = -0.13, q = -0.402812582, mean = -2.56348357, sd = 1,
                      null.mean = 0, null.sd = 1, alternative = "two.sided", z.alpha = 1.95996398 * c(-1, 1),
                      n = c(n1 = 84, n2 = 84), power = 0.7269215))
    # results are identical: n = 84 / 84

    # power.z.onecor (= pwrss.z.cor) -----------------------------------------------------------------------------------
    crrRes <- power.z.onecor(rho = 0.20, power = 0.80, alpha = 0.05, alternative = "two.sided", verbose = 0)
    expect_equal(class(crrRes), c("pwrss", "z", "onecor"))
    expect_equal(names(crrRes), c("parms", "test", "design", "delta", "q", "mean", "sd", "null.mean", "null.sd", "alternative",
                                  "z.alpha", "n", "power"))
    expect_equal(crrRes[["parms"]], list(rho = 0.20, null.rho = 0, alpha = 0.05, alternative = "two.sided", ceiling = TRUE,
                                         verbose = 0, utf = FALSE))
    expect_equal(crrRes[c("test", "design", "delta", "q", "mean", "sd", "null.mean", "null.sd", "alternative", "z.alpha", "n", "power")],
                 list(test = "z", design = "one.sample", delta = 0.2, q = 0.202732554, mean = 2.80181964, sd = 1, null.mean = 0, null.sd = 1,
                      alternative = "two.sided", z.alpha = c(-1.959964, 1.959964), n = 194, power = 0.80006658))
    expect_equal(crrRes, pwrss.z.corr(r = 0.20, power = 0.80, alpha = 0.05, alternative = "not equal", verbose = FALSE))

    crrRes <- power.z.onecor(rho = 0.20, n = 194, alpha = 0.05, alternative = "two.sided", verbose = 0)
    expect_equal(class(crrRes), c("pwrss", "z", "onecor"))
    expect_equal(names(crrRes), c("parms", "test", "design", "delta", "q", "mean", "sd", "null.mean", "null.sd", "alternative",
                                  "z.alpha", "n", "power"))
    expect_equal(crrRes[["parms"]], list(rho = 0.20, null.rho = 0, alpha = 0.05, alternative = "two.sided", ceiling = TRUE,
                                         verbose = 0, utf = FALSE))
    expect_equal(crrRes[c("test", "design", "delta", "q", "mean", "sd", "null.mean", "null.sd", "alternative", "z.alpha", "n", "power")],
                 list(test = "z", design = "one.sample", delta = 0.2, q = 0.202732554, mean = 2.80181964, sd = 1, null.mean = 0, null.sd = 1,
                      alternative = "two.sided", z.alpha = c(-1.959964, 1.959964), n = 194, power = 0.80006658))
    expect_equal(crrRes, pwrss.z.corr(r = 0.20, n = 194, alpha = 0.05, alternative = "not equal", verbose = FALSE))

    crrRes <- power.z.onecor(rho = 0.20, null = 0.10, power = 0.80, alpha = 0.05, alternative = "one.sided", verbose = 0)
    expect_equal(class(crrRes), c("pwrss", "z", "onecor"))
    expect_equal(names(crrRes), c("parms", "test", "design", "delta", "q", "mean", "sd", "null.mean", "null.sd", "alternative",
                                  "z.alpha", "n", "power"))
    expect_equal(crrRes[["parms"]], list(rho = 0.20, null.rho = 0.10, alpha = 0.05, alternative = "one.sided", ceiling = TRUE,
                                         verbose = 0, utf = FALSE))
    expect_equal(crrRes[c("test", "design", "delta", "q", "mean", "sd", "null.mean", "null.sd", "alternative", "z.alpha", "n", "power")],
                 list(test = "z", design = "one.sample", delta = 0.1, q = 0.102397206, mean = 2.4872195, sd = 1, null.mean = 0, null.sd = 1,
                      alternative = "one.sided", z.alpha = 1.64485363, n = 593, power = 0.8002084))
    expect_equal(crrRes, pwrss.z.corr(r = 0.20, r0 = 0.10, power = 0.80, alpha = 0.05, alternative = "greater", verbose = FALSE))

    crrRes <- power.z.onecor(rho = 0.20, null = 0.10, n = 593, alpha = 0.05, alternative = "one.sided", verbose = 0)
    expect_equal(class(crrRes), c("pwrss", "z", "onecor"))
    expect_equal(names(crrRes), c("parms", "test", "design", "delta", "q", "mean", "sd", "null.mean", "null.sd", "alternative",
                                  "z.alpha", "n", "power"))
    expect_equal(crrRes[["parms"]], list(rho = 0.20, null.rho = 0.10, alpha = 0.05, alternative = "one.sided", ceiling = TRUE,
                                         verbose = 0, utf = FALSE))
    expect_equal(crrRes[c("test", "design", "delta", "q", "mean", "sd", "null.mean", "null.sd", "alternative", "z.alpha", "n", "power")],
                 list(test = "z", design = "one.sample", delta = 0.1, q = 0.102397206, mean = 2.4872195, sd = 1, null.mean = 0, null.sd = 1,
                      alternative = "one.sided", z.alpha = 1.64485363, n = 593, power = 0.8002084))

    crrRes <- power.z.onecor(rho = 0.65, null.rho = 0.60, alpha = 0.05, power = 0.95, verbose = 0) # example 3.3 from GPower manual
    expect_equal(class(crrRes), c("pwrss", "z", "onecor"))
    expect_equal(names(crrRes), c("parms", "test", "design", "delta", "q", "mean", "sd", "null.mean", "null.sd", "alternative",
                                  "z.alpha", "n", "power"))
    expect_equal(crrRes[["parms"]], list(rho = 0.65, null.rho = 0.60, alpha = 0.05, alternative = "two.sided", ceiling = TRUE,
                                         verbose = 0, utf = FALSE))
    expect_equal(crrRes[c("test", "design", "delta", "q", "mean", "sd", "null.mean", "null.sd", "alternative", "z.alpha", "n", "power")],
                 list(test = "z", design = "one.sample", delta = 0.05, q = 0.082151526, mean = 3.60531964, sd = 1, null.mean = 0, null.sd = 1,
                      alternative = "two.sided", z.alpha = 1.95996398 * c(-1, 1), n = 1929, power = 0.95005177))
    # pwrss uses approximation, GPower an exact calculation
    # -> values are slighly off: n = 1928 (GPower) / 1929 (pwrss), power = 0.950028 (GPower) / 0.950052 (pwrss)

    crrRes <- power.z.onecor(rho = 0.30, null.rho = 0.80, alpha = 0.05, n = 8, verbose = 0) # example 3.3 from GPower manual
    expect_equal(class(crrRes), c("pwrss", "z", "onecor"))
    expect_equal(names(crrRes), c("parms", "test", "design", "delta", "q", "mean", "sd", "null.mean", "null.sd", "alternative",
                                  "z.alpha", "n", "power"))
    expect_equal(crrRes[["parms"]], list(rho = 0.30, null.rho = 0.80, alpha = 0.05, alternative = "two.sided", ceiling = TRUE,
                                         verbose = 0, utf = FALSE))
    expect_equal(crrRes[c("test", "design", "delta", "q", "mean", "sd", "null.mean", "null.sd", "alternative", "z.alpha", "n", "power")],
                 list(test = "z", design = "one.sample", delta = -0.50, q = -0.789092684, mean = -1.7644649, sd = 1, null.mean = 0, null.sd = 1,
                      alternative = "two.sided", z.alpha = 1.95996398 * c(-1, 1), n = 8, power = 0.422599))
    # pwrss uses approximation, GPower an exact calculation
    # -> values are correct for approx.: power = 0.482927 / 0.422599 (GPower - exact / appr.) / 0.422599 (pwrss)
})
