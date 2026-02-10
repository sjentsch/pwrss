test_that("means.student.R works", {
    # power.t.student (= pwrss.t.mean / pwrss.t.2means) ----------------------------------------------------------------
    crrRes <- power.t.student(d = 0.20, power = 0.80, alternative = "two.sided", design = "independent", verbose = 0)
    expect_equal(class(crrRes), c("pwrss", "t", "student"))
    expect_equal(names(crrRes), c("parms", "test", "df", "ncp", "null.ncp", "t.alpha", "power", "n"))
    expect_equal(crrRes[["parms"]],
                 list(d = 0.2, null.d = 0, margin = 0, n.ratio = 1, alpha = 0.05, alternative = "two.sided",
                      design = "independent", claim.basis = "md.pval", ceiling = TRUE, verbose = 0, pretty = FALSE))
    expect_equal(crrRes[c("test", "df", "ncp", "null.ncp", "t.alpha", "power", "n")],
                 list(test = "t", df = 786, ncp = 2.80713377, null.ncp = 0, t.alpha = 1.96298672 * c(-1, 1),
                      power = 0.800593128, n = c(n1 = 394, n2 = 394)))

    crrAsc <- capture.output(power.t.student(d = 0.20, power = 0.80, alternative = "two.sided", design = "independent", verbose = 2))
    expect_equal(crrAsc, c("+--------------------------------------------------+",
                           "|             SAMPLE SIZE CALCULATION              |",
                           "+--------------------------------------------------+", "",
                           "Student's T-Test (Independent Samples)", "",
                           "----------------------------------------------------",
                           "Hypotheses",
                           "----------------------------------------------------",
                           "  H0 (Null Claim) : d - null.d  = 0",
                           "  H1 (Alt. Claim) : d - null.d != 0", "",
                           "----------------------------------------------------",
                           "Key Parameters",
                           "----------------------------------------------------",
                           "  Cohen's d              = 0.200",
                           "  Cohen's d Under Null   = 0.000",
                           "  Margin                 = 0",
                           "  Degrees of Freedom     = 786",
                           "  Non-centrality of Alt. = 2.807",
                           "  Non-centrality of Null = 0",
                           "  Critical Value         = -1.963 and 1.963 ", "",
                           "----------------------------------------------------",
                           "Results",
                           "----------------------------------------------------",
                           "  Sample Size          = 394 and 394  <<",
                           "  Type 1 Error (alpha) = 0.050",
                           "  Type 2 Error (beta)  = 0.199",
                           "  Statistical Power    = 0.801", "",
                           "----------------------------------------------------",
                           "Definitions",
                           "----------------------------------------------------",
                           "  Margin : Smallest d - null.d difference that matters ", ""))

    crrPty <- capture.output(power.t.student(d = 0.20, power = 0.80, alternative = "two.sided", design = "independent",
                                             verbose = 2, pretty = TRUE))
    expect_equal(crrPty, c("╔══════════════════════════════════════════════════╗",
                           "║           \033[34m SAMPLE SIZE CALCULATION \033[0m              ║",
                           "╚══════════════════════════════════════════════════╝", "",
                           "Student's T-Test (Independent Samples)", "",
                           "────────────────────────────────────────────────────",
                           "Hypotheses",
                           "────────────────────────────────────────────────────",
                           "  H₀ (Null)         : d - d₀ = 0 ",
                           "  H₁ (Alternative)  : d - d₀ ≠ 0 ", "",
                           "────────────────────────────────────────────────────",
                           "Key Parameters",
                           "────────────────────────────────────────────────────",
                           "  d                 = 0.200",
                           "  d₀                 = 0.000",
                           "  δ                 = 0 ",
                           "  df                = 786",
                           "  λ                 = 2.807",
                           "  λ₀                 = 0 ",
                           "  T⁻¹(α, λ₀)          = -1.963 and 1.963 ", "",
                           "────────────────────────────────────────────────────",
                           "Results",
                           "────────────────────────────────────────────────────",
                           "  \033[34mSample Size        = 394 and 394\033[0m  \033[1;35m◄◄\033[0m",
                           "  Type 1 Error (α)   = 0.050",
                           "  Type 2 Error (β)   = 0.199",
                           "  Statistical Power  = 0.801", "",
                           "\033[36m────────────────────────────────────────────────────\033[0m",
                           "\033[36mDefinitions\033[0m",
                           "\033[36m────────────────────────────────────────────────────\033[0m",
                           "\033[36m  d   : Cohen's d under alternative ",
                           "\033[0m\033[36m  d₀ : Cohen's d under null ",
                           "\033[0m\033[36m  δ   : Margin - ignorable d - d₀ difference ",
                           "\033[0m\033[36m  λ   : Non-centrality parameter under alternative ",
                           "\033[0m\033[36m  λ₀ : Non-centrality parameter under null ", "", "\033[0m"))

    crrRes <- power.t.student(d = 0.20, n2 = 394, alternative = "two.sided", design = "independent", verbose = 0)
    expect_equal(class(crrRes), c("pwrss", "t", "student"))
    expect_equal(names(crrRes), c("parms", "test", "df", "ncp", "null.ncp", "t.alpha", "power", "n"))
    expect_equal(crrRes[["parms"]],
                 list(d = 0.2, null.d = 0, margin = 0, n.ratio = 1, alpha = 0.05, alternative = "two.sided",
                      design = "independent", claim.basis = "md.pval", ceiling = TRUE, verbose = 0, pretty = FALSE))
    expect_equal(crrRes[c("test", "df", "ncp", "null.ncp", "t.alpha", "power", "n")],
                 list(test = "t", df = 786, ncp = 2.80713377, null.ncp = 0, t.alpha = 1.96298672 * c(-1, 1),
                      power = 0.800593128, n = c(n1 = 394, n2 = 394)))

    crrRes <- power.t.student(d = 0.20, power = 0.80, alternative = "two.sided", design = "independent",
                              claim.basis = "smd.ci", verbose = 0)
    expect_equal(class(crrRes), c("pwrss", "t", "student"))
    expect_equal(names(crrRes), c("parms", "test", "df", "ncp", "null.ncp", "t.alpha", "power", "n"))
    expect_equal(crrRes[["parms"]],
                 list(d = 0.2, null.d = 0, margin = 0, n.ratio = 1, alpha = 0.05, alternative = "two.sided",
                      design = "independent", claim.basis = "smd.ci", ceiling = TRUE, verbose = 0, pretty = FALSE))
    expect_equal(crrRes[c("test", "df", "ncp", "null.ncp", "t.alpha", "power", "n")],
                 list(test = "t", df = 790, ncp = 2.80722243, null.ncp = 0, t.alpha = 1.96297139 * c(-1, 1),
                      power = 0.80062273, n = c(n1 = 396, n2 = 396)))

    crrRes <- power.t.student(d = 0.20, n2 = 396, alternative = "two.sided", design = "independent",
                              claim.basis = "smd.ci", verbose = 0)
    expect_equal(class(crrRes), c("pwrss", "t", "student"))
    expect_equal(names(crrRes), c("parms", "test", "df", "ncp", "null.ncp", "t.alpha", "power", "n"))
    expect_equal(crrRes[["parms"]],
                 list(d = 0.2, null.d = 0, margin = 0, n.ratio = 1, alpha = 0.05, alternative = "two.sided",
                      design = "independent", claim.basis = "smd.ci", ceiling = TRUE, verbose = 0, pretty = FALSE))
    expect_equal(crrRes[c("test", "df", "ncp", "null.ncp", "t.alpha", "power", "n")],
                 list(test = "t", df = 790, ncp = 2.80722243, null.ncp = 0, t.alpha = 1.96297139 * c(-1, 1),
                      power = 0.80062273, n = c(n1 = 396, n2 = 396)))

    crrRes <- power.t.student(d = 0.20, power = 0.80, alternative = "one.sided", design = "independent", verbose = 0)
    expect_equal(class(crrRes), c("pwrss", "t", "student"))
    expect_equal(names(crrRes), c("parms", "test", "df", "ncp", "null.ncp", "t.alpha", "power", "n"))
    expect_equal(crrRes[["parms"]],
                 list(d = 0.2, null.d = 0, margin = 0, n.ratio = 1, alpha = 0.05, alternative = "one.sided",
                      design = "independent", claim.basis = "md.pval", ceiling = TRUE, verbose = 0, pretty = FALSE))
    expect_equal(crrRes[c("test", "df", "ncp", "null.ncp", "t.alpha", "power", "n")],
                 list(test = "t", df = 618, ncp = 2.48997992, null.ncp = 0, t.alpha = 1.647323,
                      power = 0.8002178, n = c(n1 = 310, n2 = 310)))

    crrRes <- power.t.student(d = 0.20, n2 = 310, alternative = "one.sided", design = "independent", verbose = 0)
    expect_equal(class(crrRes), c("pwrss", "t", "student"))
    expect_equal(names(crrRes), c("parms", "test", "df", "ncp", "null.ncp", "t.alpha", "power", "n"))
    expect_equal(crrRes[["parms"]],
                 list(d = 0.2, null.d = 0, margin = 0, n.ratio = 1, alpha = 0.05, alternative = "one.sided",
                      design = "independent", claim.basis = "md.pval", ceiling = TRUE, verbose = 0, pretty = FALSE))
    expect_equal(crrRes[c("test", "df", "ncp", "null.ncp", "t.alpha", "power", "n")],
                 list(test = "t", df = 618, ncp = 2.48997992, null.ncp = 0, t.alpha = 1.647323,
                      power = 0.8002178, n = c(n1 = 310, n2 = 310)))

    crrRes <- power.t.student(d = 0.20, margin = -0.05, power = 0.80, alternative = "one.sided", design = "independent", verbose = 0)
    expect_equal(class(crrRes), c("pwrss", "t", "student"))
    expect_equal(names(crrRes), c("parms", "test", "df", "ncp", "null.ncp", "t.alpha", "power", "n"))
    expect_equal(crrRes[["parms"]],
                 list(d = 0.2, null.d = 0, margin = -0.05, n.ratio = 1, alpha = 0.05, alternative = "one.sided",
                      design = "independent", claim.basis = "md.pval", ceiling = TRUE, verbose = 0, pretty = FALSE))
    expect_equal(crrRes[c("test", "df", "ncp", "null.ncp", "t.alpha", "power", "n")],
                 list(test = "t", df = 396, ncp = 1.994993734, null.ncp = -0.49874843, t.alpha = 1.14819672,
                      power = 0.80145163, n = c(n1 = 199, n2 = 199)))

    crrRes <- power.t.student(d = 0.20, margin = -0.05, n2 = 199, alternative = "one.sided", design = "independent", verbose = 0)
    expect_equal(class(crrRes), c("pwrss", "t", "student"))
    expect_equal(names(crrRes), c("parms", "test", "df", "ncp", "null.ncp", "t.alpha", "power", "n"))
    expect_equal(crrRes[["parms"]],
                 list(d = 0.2, null.d = 0, margin = -0.05, n.ratio = 1, alpha = 0.05, alternative = "one.sided",
                      design = "independent", claim.basis = "md.pval", ceiling = TRUE, verbose = 0, pretty = FALSE))
    expect_equal(crrRes[c("test", "df", "ncp", "null.ncp", "t.alpha", "power", "n")],
                 list(test = "t", df = 396, ncp = 1.994993734, null.ncp = -0.49874843, t.alpha = 1.14819672,
                      power = 0.80145163, n = c(n1 = 199, n2 = 199)))

    crrRes <- power.t.student(d = 0.20, margin = 0.05, power = 0.80, alternative = "one.sided", design = "independent", verbose = 0)
    expect_equal(class(crrRes), c("pwrss", "t", "student"))
    expect_equal(names(crrRes), c("parms", "test", "df", "ncp", "null.ncp", "t.alpha", "power", "n"))
    expect_equal(crrRes[["parms"]],
                 list(d = 0.2, null.d = 0, margin = 0.05, n.ratio = 1, alpha = 0.05, alternative = "one.sided",
                      design = "independent", claim.basis = "md.pval", ceiling = TRUE, verbose = 0, pretty = FALSE))
    expect_equal(crrRes[c("test", "df", "ncp", "null.ncp", "t.alpha", "power", "n")],
                 list(test = "t", df = 1102, ncp = 3.32264955, null.ncp = 0.830662386, t.alpha = 2.4783672,
                      power = 0.800573124, n = c(n1 = 552, n2 = 552)))

    crrRes <- power.t.student(d = 0.20, margin = 0.05, n2 = 552, alternative = "one.sided", design = "independent", verbose = 0)
    expect_equal(class(crrRes), c("pwrss", "t", "student"))
    expect_equal(names(crrRes), c("parms", "test", "df", "ncp", "null.ncp", "t.alpha", "power", "n"))
    expect_equal(crrRes[["parms"]],
                 list(d = 0.2, null.d = 0, margin = 0.05, n.ratio = 1, alpha = 0.05, alternative = "one.sided",
                      design = "independent", claim.basis = "md.pval", ceiling = TRUE, verbose = 0, pretty = FALSE))
    expect_equal(crrRes[c("test", "df", "ncp", "null.ncp", "t.alpha", "power", "n")],
                 list(test = "t", df = 1102, ncp = 3.32264955, null.ncp = 0.830662386, t.alpha = 2.4783672,
                      power = 0.800573124, n = c(n1 = 552, n2 = 552)))

    crrRes <- power.t.student(d = 0, margin = c(-0.05, 0.05), power = 0.80, alternative = "two.one.sided", design = "independent", verbose = 0)
    expect_equal(class(crrRes), c("pwrss", "t", "student"))
    expect_equal(names(crrRes), c("parms", "test", "df", "ncp", "null.ncp", "t.alpha", "power", "n"))
    expect_equal(crrRes[["parms"]],
                 list(d = 0, null.d = 0, margin = 0.05 * c(-1, 1), n.ratio = 1, alpha = 0.05, alternative = "two.one.sided",
                      design = "independent", claim.basis = "md.pval", ceiling = TRUE, verbose = 0, pretty = FALSE))
    expect_equal(crrRes[c("test", "df", "ncp", "null.ncp", "t.alpha", "power", "n")],
                 list(test = "t", df = 13702, ncp = 0, null.ncp = 2.92660213 * c(-1, 1), t.alpha = 1.2817226 * c(-1, 1),
                      power = 0.800038332, n = c(n1 = 6852, n2 = 6852)))

    crrRes <- power.t.student(d = 0, margin = c(-0.05, 0.05), n2 = 6852, alternative = "two.one.sided", design = "independent", verbose = 0)
    expect_equal(class(crrRes), c("pwrss", "t", "student"))
    expect_equal(names(crrRes), c("parms", "test", "df", "ncp", "null.ncp", "t.alpha", "power", "n"))
    expect_equal(crrRes[["parms"]],
                 list(d = 0, null.d = 0, margin = 0.05 * c(-1, 1), n.ratio = 1, alpha = 0.05, alternative = "two.one.sided",
                      design = "independent", claim.basis = "md.pval", ceiling = TRUE, verbose = 0, pretty = FALSE))
    expect_equal(crrRes[c("test", "df", "ncp", "null.ncp", "t.alpha", "power", "n")],
                 list(test = "t", df = 13702, ncp = 0, null.ncp = 2.92660213 * c(-1, 1), t.alpha = 1.2817226 * c(-1, 1),
                      power = 0.800038332, n = c(n1 = 6852, n2 = 6852)))

    crrRes <- power.t.student(d = 0.05, margin = c(-0.05, -0.10), power = 0.80, alternative = "two.one.sided", design = "independent", verbose = 0)
    expect_equal(class(crrRes), c("pwrss", "t", "student"))
    expect_equal(names(crrRes), c("parms", "test", "df", "ncp", "null.ncp", "t.alpha", "power", "n"))
    expect_equal(crrRes[["parms"]],
                 list(d = 0.05, null.d = 0, margin = c(-0.05, -0.10), n.ratio = 1, alpha = 0.05, alternative = "two.one.sided",
                      design = "independent", claim.basis = "md.pval", ceiling = TRUE, verbose = 0, pretty = FALSE))
    expect_equal(crrRes[c("test", "df", "ncp", "null.ncp", "t.alpha", "power", "n")],
                 list(test = "t", df = 3138, ncp = 1.400892573, null.ncp = c(-1.400892573, -2.801785145), t.alpha = c(-4.76567037, 0.55916477),
                      power = 0.800036434, n = c(n1 = 1570, n2 = 1570)))

    crrRes <- power.t.student(d = 0.05, margin = c(-0.05, -0.10), n2 = 1570, alternative = "two.one.sided", design = "independent", verbose = 0)
    expect_equal(class(crrRes), c("pwrss", "t", "student"))
    expect_equal(names(crrRes), c("parms", "test", "df", "ncp", "null.ncp", "t.alpha", "power", "n"))
    expect_equal(crrRes[["parms"]],
                 list(d = 0.05, null.d = 0, margin = c(-0.05, -0.10), n.ratio = 1, alpha = 0.05, alternative = "two.one.sided",
                      design = "independent", claim.basis = "md.pval", ceiling = TRUE, verbose = 0, pretty = FALSE))
    expect_equal(crrRes[c("test", "df", "ncp", "null.ncp", "t.alpha", "power", "n")],
                 list(test = "t", df = 3138, ncp = 1.400892573, null.ncp = c(-1.400892573, -2.801785145), t.alpha = c(-4.76567037, 0.55916477),
                      power = 0.800036434, n = c(n1 = 1570, n2 = 1570)))

    crrRes <- power.t.student(d = -0.20, power = 0.80, alternative = "two.sided", design = "paired", verbose = 0)
    expect_equal(class(crrRes), c("pwrss", "t", "student"))
    expect_equal(names(crrRes), c("parms", "test", "df", "ncp", "null.ncp", "t.alpha", "power", "n"))
    expect_equal(crrRes[["parms"]],
                 list(d = -0.20, null.d = 0, margin = 0, n.ratio = 1, alpha = 0.05, alternative = "two.sided",
                      design = "paired", claim.basis = "md.pval", ceiling = TRUE, verbose = 0, pretty = FALSE))
    expect_equal(crrRes[c("test", "df", "ncp", "null.ncp", "t.alpha", "power", "n")],
                 list(test = "t", df = 198, ncp = -2.8213472, null.ncp = 0, t.alpha = 1.97201748 * c(-1, 1),
                      power = 0.801691024, n = 199))
    expect_equal(crrRes, pwrss.t.2means(mu1 = -0.20, sd1 = 1, power = 0.80, alternative = "not equal", paired = TRUE, verbose = FALSE))

    crrRes <- power.t.student(d = -0.20, n2 = 199, alternative = "two.sided", design = "paired", verbose = 0)
    expect_equal(class(crrRes), c("pwrss", "t", "student"))
    expect_equal(names(crrRes), c("parms", "test", "df", "ncp", "null.ncp", "t.alpha", "power", "n"))
    expect_equal(crrRes[["parms"]],
                 list(d = -0.20, null.d = 0, margin = 0, n.ratio = 1, alpha = 0.05, alternative = "two.sided",
                      design = "paired", claim.basis = "md.pval", ceiling = TRUE, verbose = 0, pretty = FALSE))
    expect_equal(crrRes[c("test", "df", "ncp", "null.ncp", "t.alpha", "power", "n")],
                 list(test = "t", df = 198, ncp = -2.8213472, null.ncp = 0, t.alpha = 1.97201748 * c(-1, 1),
                      power = 0.801691024, n = 199))

    crrRes <- power.t.student(d = -0.20, power = 0.80, alternative = "two.sided", claim.basis = "smd.ci", design = "paired",
                              verbose = 0)
    expect_equal(class(crrRes), c("pwrss", "t", "student"))
    expect_equal(names(crrRes), c("parms", "test", "df", "ncp", "null.ncp", "t.alpha", "power", "n"))
    expect_equal(crrRes[["parms"]],
                 list(d = -0.20, null.d = 0, margin = 0, n.ratio = 1, alpha = 0.05, alternative = "two.sided",
                      design = "paired", claim.basis = "smd.ci", ceiling = TRUE, verbose = 0, pretty = FALSE))
    expect_equal(crrRes[c("test", "df", "ncp", "null.ncp", "t.alpha", "power", "n")],
                 list(test = "t", df = 198, ncp = -2.8213472, null.ncp = 0, t.alpha = 1.97201748 * c(-1, 1),
                      power = 0.801691024, n = 199))


    crrRes <- power.t.student(d = -0.20, power = 0.80, alternative = "one.sided", design = "paired", verbose = 0)
    expect_equal(class(crrRes), c("pwrss", "t", "student"))
    expect_equal(names(crrRes), c("parms", "test", "df", "ncp", "null.ncp", "t.alpha", "power", "n"))
    expect_equal(crrRes[["parms"]],
                 list(d = -0.20, null.d = 0, margin = 0, n.ratio = 1, alpha = 0.05, alternative = "one.sided",
                      design = "paired", claim.basis = "md.pval", ceiling = TRUE, verbose = 0, pretty = FALSE))
    expect_equal(crrRes[c("test", "df", "ncp", "null.ncp", "t.alpha", "power", "n")],
                 list(test = "t", df = 155, ncp = -2.4979992, null.ncp = 0, t.alpha = -1.654743774,
                      power = 0.80016732, n = 156))

    crrRes <- power.t.student(d = -0.20, n2 = 156, alternative = "one.sided", design = "paired", verbose = 0)
    expect_equal(class(crrRes), c("pwrss", "t", "student"))
    expect_equal(names(crrRes), c("parms", "test", "df", "ncp", "null.ncp", "t.alpha", "power", "n"))
    expect_equal(crrRes[["parms"]],
                 list(d = -0.20, null.d = 0, margin = 0, n.ratio = 1, alpha = 0.05, alternative = "one.sided",
                      design = "paired", claim.basis = "md.pval", ceiling = TRUE, verbose = 0, pretty = FALSE))
    expect_equal(crrRes[c("test", "df", "ncp", "null.ncp", "t.alpha", "power", "n")],
                 list(test = "t", df = 155, ncp = -2.4979992, null.ncp = 0, t.alpha = -1.654743774,
                      power = 0.80016732, n = 156))

    crrRes <- power.t.student(d = 0.20, margin = 0.05, power = 0.80, alternative = "one.sided", design = "paired", verbose = 0)
    expect_equal(class(crrRes), c("pwrss", "t", "student"))
    expect_equal(names(crrRes), c("parms", "test", "df", "ncp", "null.ncp", "t.alpha", "power", "n"))
    expect_equal(crrRes[["parms"]],
                 list(d = 0.20, null.d = 0, margin = 0.05, n.ratio = 1, alpha = 0.05, alternative = "one.sided",
                      design = "paired", claim.basis = "md.pval", ceiling = TRUE, verbose = 0, pretty = FALSE))
    expect_equal(crrRes[c("test", "df", "ncp", "null.ncp", "t.alpha", "power", "n")],
                 list(test = "t", df = 277, ncp = 3.3346664, null.ncp = 0.8336666, t.alpha = 2.489922,
                      power = 0.80018923, n = 278))

    crrRes <- power.t.student(d = 0.20, margin = 0.05, n2 = 278, alternative = "one.sided", design = "paired", verbose = 0)
    expect_equal(class(crrRes), c("pwrss", "t", "student"))
    expect_equal(names(crrRes), c("parms", "test", "df", "ncp", "null.ncp", "t.alpha", "power", "n"))
    expect_equal(crrRes[["parms"]],
                 list(d = 0.20, null.d = 0, margin = 0.05, n.ratio = 1, alpha = 0.05, alternative = "one.sided",
                      design = "paired", claim.basis = "md.pval", ceiling = TRUE, verbose = 0, pretty = FALSE))
    expect_equal(crrRes[c("test", "df", "ncp", "null.ncp", "t.alpha", "power", "n")],
                 list(test = "t", df = 277, ncp = 3.3346664, null.ncp = 0.8336666, t.alpha = 2.489922,
                      power = 0.80018923, n = 278))

    crrRes <- power.t.student(d = 0.20, margin = -0.05, power = 0.80, alternative = "one.sided", design = "paired", verbose = 0)
    expect_equal(class(crrRes), c("pwrss", "t", "student"))
    expect_equal(names(crrRes), c("parms", "test", "df", "ncp", "null.ncp", "t.alpha", "power", "n"))
    expect_equal(crrRes[["parms"]],
                 list(d = 0.20, null.d = 0, margin = -0.05, n.ratio = 1, alpha = 0.05, alternative = "one.sided",
                      design = "paired", claim.basis = "md.pval", ceiling = TRUE, verbose = 0, pretty = FALSE))
    expect_equal(crrRes[c("test", "df", "ncp", "null.ncp", "t.alpha", "power", "n")],
                 list(test = "t", df = 99, ncp = 2, null.ncp = -0.5, t.alpha = 1.1532522,
                      power = 0.801453942, n = 100))

    crrRes <- power.t.student(d = 0.20, margin = -0.05, n2 = 100, alternative = "one.sided", design = "paired", verbose = 0)
    expect_equal(class(crrRes), c("pwrss", "t", "student"))
    expect_equal(names(crrRes), c("parms", "test", "df", "ncp", "null.ncp", "t.alpha", "power", "n"))
    expect_equal(crrRes[["parms"]],
                 list(d = 0.20, null.d = 0, margin = -0.05, n.ratio = 1, alpha = 0.05, alternative = "one.sided",
                      design = "paired", claim.basis = "md.pval", ceiling = TRUE, verbose = 0, pretty = FALSE))
    expect_equal(crrRes[c("test", "df", "ncp", "null.ncp", "t.alpha", "power", "n")],
                 list(test = "t", df = 99, ncp = 2, null.ncp = -0.5, t.alpha = 1.1532522,
                      power = 0.801453942, n = 100))

    crrRes <- power.t.student(d = 0, margin = c(-0.05, 0.05), power = 0.80, alternative = "two.one.sided", design = "paired", verbose = 0)
    expect_equal(class(crrRes), c("pwrss", "t", "student"))
    expect_equal(names(crrRes), c("parms", "test", "df", "ncp", "null.ncp", "t.alpha", "power", "n"))
    expect_equal(crrRes[["parms"]],
                 list(d = 0, null.d = 0, margin = 0.05 * c(-1, 1), n.ratio = 1, alpha = 0.05, alternative = "two.one.sided",
                      design = "paired", claim.basis = "md.pval", ceiling = TRUE, verbose = 0, pretty = FALSE))
    expect_equal(crrRes[c("test", "df", "ncp", "null.ncp", "t.alpha", "power", "n")],
                 list(test = "t", df = 3426, ncp = 0, null.ncp = 2.92702921 * c(-1, 1), t.alpha = 1.282071859 * c(-1, 1),
                      power = 0.800095812, n = 3427))

    crrRes <- power.t.student(d = 0, margin = c(-0.05, 0.05), n2 = 3427, alternative = "two.one.sided", design = "paired", verbose = 0)
    expect_equal(class(crrRes), c("pwrss", "t", "student"))
    expect_equal(names(crrRes), c("parms", "test", "df", "ncp", "null.ncp", "t.alpha", "power", "n"))
    expect_equal(crrRes[["parms"]],
                 list(d = 0, null.d = 0, margin = 0.05 * c(-1, 1), n.ratio = 1, alpha = 0.05, alternative = "two.one.sided",
                      design = "paired", claim.basis = "md.pval", ceiling = TRUE, verbose = 0, pretty = FALSE))
    expect_equal(crrRes[c("test", "df", "ncp", "null.ncp", "t.alpha", "power", "n")],
                 list(test = "t", df = 3426, ncp = 0, null.ncp = 2.92702921 * c(-1, 1), t.alpha = 1.282071859 * c(-1, 1),
                      power = 0.800095812, n = 3427))

    crrRes <- power.t.student(d = 0.05, margin = c(-0.05, -0.10), power = 0.80, alternative = "two.one.sided", design = "paired", verbose = 0)
    expect_equal(class(crrRes), c("pwrss", "t", "student"))
    expect_equal(names(crrRes), c("parms", "test", "df", "ncp", "null.ncp", "t.alpha", "power", "n"))
    expect_equal(crrRes[["parms"]],
                 list(d = 0.05, null.d = 0, margin = c(-0.05, -0.10), n.ratio = 1, alpha = 0.05, alternative = "two.one.sided",
                      design = "paired", claim.basis = "md.pval", ceiling = TRUE, verbose = 0, pretty = FALSE))
    expect_equal(crrRes[c("test", "df", "ncp", "null.ncp", "t.alpha", "power", "n")],
                 list(test = "t", df = 785, ncp = 1.401784577, null.ncp = c(-1.401784577, -2.803569154),
                      t.alpha = c(-4.7792355, 0.5585519), power = 0.800477147, n = 786))

    crrRes <- power.t.student(d = 0.05, margin = c(-0.05, -0.10), n2 = 786, alternative = "two.one.sided", design = "paired", verbose = 0)
    expect_equal(class(crrRes), c("pwrss", "t", "student"))
    expect_equal(names(crrRes), c("parms", "test", "df", "ncp", "null.ncp", "t.alpha", "power", "n"))
    expect_equal(crrRes[["parms"]],
                 list(d = 0.05, null.d = 0, margin = c(-0.05, -0.10), n.ratio = 1, alpha = 0.05, alternative = "two.one.sided",
                      design = "paired", claim.basis = "md.pval", ceiling = TRUE, verbose = 0, pretty = FALSE))
    expect_equal(crrRes[c("test", "df", "ncp", "null.ncp", "t.alpha", "power", "n")],
                 list(test = "t", df = 785, ncp = 1.401784577, null.ncp = c(-1.401784577, -2.803569154),
                      t.alpha = c(-4.7792355, 0.5585519), power = 0.800477147, n = 786))

    crrRes <- power.t.student(d = -0.20, power = 0.80, alternative = "two.sided", design = "one.sample", verbose = 0)
    expect_equal(class(crrRes), c("pwrss", "t", "student"))
    expect_equal(names(crrRes), c("parms", "test", "df", "ncp", "null.ncp", "t.alpha", "power", "n"))
    expect_equal(crrRes[["parms"]],
                 list(d = -0.20, null.d = 0, margin = 0, n.ratio = 1, alpha = 0.05, alternative = "two.sided",
                      design = "one.sample", claim.basis = "md.pval", ceiling = TRUE, verbose = 0, pretty = FALSE))
    expect_equal(crrRes[c("test", "df", "ncp", "null.ncp", "t.alpha", "power", "n")],
                 list(test = "t", df = 198, ncp = -2.8213472, null.ncp = 0, t.alpha = 1.97201748 * c(-1, 1),
                      power = 0.801691024, n = 199))
    expect_equal(crrRes, pwrss.t.mean(mu = -0.20, sd = 1, power = 0.8, alternative = "not equal", verbose = FALSE))

    crrRes <- power.t.student(d = -0.20, n2 = 199, alternative = "two.sided", design = "one.sample", verbose = 0)
    expect_equal(class(crrRes), c("pwrss", "t", "student"))
    expect_equal(names(crrRes), c("parms", "test", "df", "ncp", "null.ncp", "t.alpha", "power", "n"))
    expect_equal(crrRes[["parms"]],
                 list(d = -0.20, null.d = 0, margin = 0, n.ratio = 1, alpha = 0.05, alternative = "two.sided",
                      design = "one.sample", claim.basis = "md.pval", ceiling = TRUE, verbose = 0, pretty = FALSE))
    expect_equal(crrRes[c("test", "df", "ncp", "null.ncp", "t.alpha", "power", "n")],
                 list(test = "t", df = 198, ncp = -2.8213472, null.ncp = 0, t.alpha = 1.97201748 * c(-1, 1),
                      power = 0.801691024, n = 199))
    expect_equal(crrRes, pwrss.t.mean(mu = -0.20, sd = 1, n = 199, alternative = "not equal", verbose = FALSE))

    crrRes <- power.t.student(d = -0.20, power = 0.80, alternative = "one.sided", design = "one.sample", verbose = 0)
    expect_equal(class(crrRes), c("pwrss", "t", "student"))
    expect_equal(names(crrRes), c("parms", "test", "df", "ncp", "null.ncp", "t.alpha", "power", "n"))
    expect_equal(crrRes[["parms"]],
                 list(d = -0.20, null.d = 0, margin = 0, n.ratio = 1, alpha = 0.05, alternative = "one.sided",
                      design = "one.sample", claim.basis = "md.pval", ceiling = TRUE, verbose = 0, pretty = FALSE))
    expect_equal(crrRes[c("test", "df", "ncp", "null.ncp", "t.alpha", "power", "n")],
                 list(test = "t", df = 155, ncp = -2.4979992, null.ncp = 0, t.alpha = -1.654743774,
                      power = 0.80016732, n = 156))
    expect_equal(crrRes, pwrss.t.mean(mu = -0.20, sd = 1, power = 0.8, alternative = "less", verbose = FALSE))

    crrRes <- power.t.student(d = -0.20, n2 = 156, alternative = "one.sided", design = "one.sample", verbose = 0)
    expect_equal(class(crrRes), c("pwrss", "t", "student"))
    expect_equal(names(crrRes), c("parms", "test", "df", "ncp", "null.ncp", "t.alpha", "power", "n"))
    expect_equal(crrRes[["parms"]],
                 list(d = -0.20, null.d = 0, margin = 0, n.ratio = 1, alpha = 0.05, alternative = "one.sided",
                      design = "one.sample", claim.basis = "md.pval", ceiling = TRUE, verbose = 0, pretty = FALSE))
    expect_equal(crrRes[c("test", "df", "ncp", "null.ncp", "t.alpha", "power", "n")],
                 list(test = "t", df = 155, ncp = -2.4979992, null.ncp = 0, t.alpha = -1.654743774,
                      power = 0.80016732, n = 156))

    expect_error(power.t.student(d = 1e-4, power = 1 - 1e-4, alpha = 1e-4, alternative = "two.sided", design = "independent"),
                 "Design is not feasible.")

    # power.t.welch ----------------------------------------------------------------------------------------------------
    crrRes <- power.t.welch(d = 0.20, n.ratio = 2, var.ratio = 2, power = 0.80, alternative = "two.sided", verbose = 0)
    expect_equal(class(crrRes), c("pwrss", "t", "welch"))
    expect_equal(names(crrRes), c("parms", "test", "df", "ncp", "null.ncp", "t.alpha", "power", "n"))
    expect_equal(crrRes[["parms"]],
                 list(d = 0.20, null.d = 0, margin = 0, var.ratio = 2, n.ratio = 2, alpha = 0.05, alternative = "two.sided",
                      claim.basis = "md.pval", ceiling = TRUE, verbose = 0, pretty = FALSE))
    expect_equal(crrRes[c("test", "df", "ncp", "null.ncp", "t.alpha", "power", "n")],
                 list(test = "t", df = 629.77715, ncp = 2.81109027, null.ncp = 0, t.alpha = 1.96373795 * c(-1, 1),
                      power = 0.801457968, n = c(n1 = 474, n2 = 237)))

    crrRes <- power.t.welch(d = 0.20, n.ratio = 2, var.ratio = 2, n2 = 237, alternative = "two.sided", verbose = 0)
    expect_equal(class(crrRes), c("pwrss", "t", "welch"))
    expect_equal(names(crrRes), c("parms", "test", "df", "ncp", "null.ncp", "t.alpha", "power", "n"))
    expect_equal(crrRes[["parms"]],
                 list(d = 0.20, null.d = 0, margin = 0, var.ratio = 2, n.ratio = 2, alpha = 0.05, alternative = "two.sided",
                      claim.basis = "md.pval", ceiling = TRUE, verbose = 0, pretty = FALSE))
    expect_equal(crrRes[c("test", "df", "ncp", "null.ncp", "t.alpha", "power", "n")],
                 list(test = "t", df = 629.77715, ncp = 2.81109027, null.ncp = 0, t.alpha = 1.96373795 * c(-1, 1),
                      power = 0.801457968, n = c(n1 = 474, n2 = 237)))

    crrAsc <- capture.output(power.t.welch(d = 0.20, n.ratio = 2, var.ratio = 2, n2 = 237, alternative = "two.sided", verbose = 2))
    expect_equal(crrAsc, c("+--------------------------------------------------+",
                           "|                POWER CALCULATION                 |",
                           "+--------------------------------------------------+", "",
                           "Welch's T-Test (Independent Samples)", "",
                           "----------------------------------------------------",
                           "Hypotheses",
                           "----------------------------------------------------",
                           "  H0 (Null Claim) : d - null.d  = 0",
                           "  H1 (Alt. Claim) : d - null.d != 0", "",
                           "----------------------------------------------------",
                           "Key Parameters",
                           "----------------------------------------------------",
                           "  Cohen's d              = 0.200",
                           "  Cohen's d Under Null   = 0.000",
                           "  Margin                 = 0",
                           "  Degrees of Freedom     = 630",
                           "  Non-centrality of Alt. = 2.811",
                           "  Non-centrality of Null = 0",
                           "  Critical Value         = -1.964 and 1.964 ", "",
                           "----------------------------------------------------",
                           "Results",
                           "----------------------------------------------------",
                           "  Sample Size          = 474 and 237",
                           "  Type 1 Error (alpha) = 0.050",
                           "  Type 2 Error (beta)  = 0.199",
                           "  Statistical Power    = 0.801  <<", "",
                           "----------------------------------------------------",
                           "Definitions",
                           "----------------------------------------------------",
                           "  Margin : Smallest d - null.d difference that matters ", ""))

    crrPty <- capture.output(power.t.welch(d = 0.20, n.ratio = 2, var.ratio = 2, n2 = 237, alternative = "two.sided",
                                           verbose = 2, pretty = TRUE))
    expect_equal(crrPty, c("╔══════════════════════════════════════════════════╗",
                           "║               \033[34m POWER CALCULATION \033[0m                ║",
                           "╚══════════════════════════════════════════════════╝", "",
                           "Welch's T-Test (Independent Samples)", "",
                           "────────────────────────────────────────────────────",
                           "Hypotheses",
                           "────────────────────────────────────────────────────",
                           "  H₀ (Null)         : d - d₀ = 0 ",
                           "  H₁ (Alternative)  : d - d₀ ≠ 0 ", "",
                           "────────────────────────────────────────────────────",
                           "Key Parameters",
                           "────────────────────────────────────────────────────",
                           "  d                 = 0.200",
                           "  d₀                 = 0.000",
                           "  δ                 = 0 ",
                           "  df                = 630",
                           "  λ                 = 2.811",
                           "  λ₀                 = 0 ",
                           "  T⁻¹(α, λ₀)          = -1.964 and 1.964 ", "",
                           "────────────────────────────────────────────────────",
                           "Results",
                           "────────────────────────────────────────────────────",
                           "  Sample Size        = 474 and 237",
                           "  Type 1 Error (α)   = 0.050",
                           "  Type 2 Error (β)   = 0.199",
                           "  \033[34mStatistical Power  = 0.801\033[0m  \033[1;35m◄◄\033[0m", "",
                           "\033[36m────────────────────────────────────────────────────\033[0m",
                           "\033[36mDefinitions\033[0m",
                           "\033[36m────────────────────────────────────────────────────\033[0m",
                           "\033[36m  d   : Cohen's d under alternative ",
                           "\033[0m\033[36m  d₀ : Cohen's d under null ",
                           "\033[0m\033[36m  δ   : Margin - ignorable d - d₀ difference ",
                           "\033[0m\033[36m  λ   : Non-centrality parameter under alternative ",
                           "\033[0m\033[36m  λ₀ : Non-centrality parameter under null ", "", "\033[0m"))

    crrRes <- power.t.welch(d = 0.20, n.ratio = 2, var.ratio = 2, power = 0.80, alternative = "two.sided", claim.basis = "smd.ci", verbose = 0)
    expect_equal(class(crrRes), c("pwrss", "t", "welch"))
    expect_equal(names(crrRes), c("parms", "test", "df", "ncp", "null.ncp", "t.alpha", "power", "n"))
    expect_equal(crrRes[["parms"]],
                 list(d = 0.20, null.d = 0, margin = 0, var.ratio = 2, n.ratio = 2, alpha = 0.05, alternative = "two.sided",
                      claim.basis = "smd.ci", ceiling = TRUE, verbose = 0, pretty = FALSE))
    expect_equal(crrRes[c("test", "df", "ncp", "null.ncp", "t.alpha", "power", "n")],
                 list(test = "t", df = 632.44382, ncp = 2.80857462, null.ncp = 0, t.alpha = 1.963722 * c(-1, 1),
                      power = 0.800762167, n = c(n1 = 476, n2 = 238)))

    crrRes <- power.t.welch(d = 0.20, n.ratio = 2, var.ratio = 2, n2 = 238, alternative = "two.sided", claim.basis = "smd.ci", verbose = 0)
    expect_equal(class(crrRes), c("pwrss", "t", "welch"))
    expect_equal(names(crrRes), c("parms", "test", "df", "ncp", "null.ncp", "t.alpha", "power", "n"))
    expect_equal(crrRes[["parms"]],
                 list(d = 0.20, null.d = 0, margin = 0, var.ratio = 2, n.ratio = 2, alpha = 0.05, alternative = "two.sided",
                      claim.basis = "smd.ci", ceiling = TRUE, verbose = 0, pretty = FALSE))
    expect_equal(crrRes[c("test", "df", "ncp", "null.ncp", "t.alpha", "power", "n")],
                 list(test = "t", df = 632.44382, ncp = 2.80857462, null.ncp = 0, t.alpha = 1.963722 * c(-1, 1),
                      power = 0.800762167, n = c(n1 = 476, n2 = 238)))

    crrRes <- power.t.welch(d = 0.20, n.ratio = 2, var.ratio = 2, power = 0.80, alternative = "one.sided", verbose = 0)
    expect_equal(class(crrRes), c("pwrss", "t", "welch"))
    expect_equal(names(crrRes), c("parms", "test", "df", "ncp", "null.ncp", "t.alpha", "power", "n"))
    expect_equal(crrRes[["parms"]],
                 list(d = 0.20, null.d = 0, margin = 0, var.ratio = 2, n.ratio = 2, alpha = 0.05, alternative = "one.sided",
                      claim.basis = "md.pval", ceiling = TRUE, verbose = 0, pretty = FALSE))
    expect_equal(crrRes[c("test", "df", "ncp", "null.ncp", "t.alpha", "power", "n")],
                 list(test = "t", df = 493.776978, ncp = 2.4904277, null.ncp = 0, t.alpha = 1.64794541,
                      power = 0.80015083, n = c(n1 = 372, n2 = 186)))

    crrRes <- power.t.welch(d = 0.20, n.ratio = 2, var.ratio = 2, n2 = 186, alternative = "one.sided", verbose = 0)
    expect_equal(class(crrRes), c("pwrss", "t", "welch"))
    expect_equal(names(crrRes), c("parms", "test", "df", "ncp", "null.ncp", "t.alpha", "power", "n"))
    expect_equal(crrRes[["parms"]],
                 list(d = 0.20, null.d = 0, margin = 0, var.ratio = 2, n.ratio = 2, alpha = 0.05, alternative = "one.sided",
                      claim.basis = "md.pval", ceiling = TRUE, verbose = 0, pretty = FALSE))
    expect_equal(crrRes[c("test", "df", "ncp", "null.ncp", "t.alpha", "power", "n")],
                 list(test = "t", df = 493.776978, ncp = 2.4904277, null.ncp = 0, t.alpha = 1.64794541,
                      power = 0.80015083, n = c(n1 = 372, n2 = 186)))

    crrRes <- power.t.welch(d = 0.20, margin = -0.05, n.ratio = 2, var.ratio = 2, power = 0.80, alternative = "one.sided", verbose = 0)
    expect_equal(class(crrRes), c("pwrss", "t", "welch"))
    expect_equal(names(crrRes), c("parms", "test", "df", "ncp", "null.ncp", "t.alpha", "power", "n"))
    expect_equal(crrRes[["parms"]],
                 list(d = 0.20, null.d = 0, margin = -0.05, var.ratio = 2, n.ratio = 2, alpha = 0.05, alternative = "one.sided",
                      claim.basis = "md.pval", ceiling = TRUE, verbose = 0, pretty = FALSE))
    expect_equal(crrRes[c("test", "df", "ncp", "null.ncp", "t.alpha", "power", "n")],
                 list(test = "t", df = 315.109859, ncp = 1.99221018, null.ncp = -0.49805255, t.alpha = 1.149433434,
                      power = 0.8003316, n = c(n1 = 238, n2 = 119)))

    crrRes <- power.t.welch(d = 0.20, margin = -0.05, n.ratio = 2, var.ratio = 2, n2 = 119, alternative = "one.sided", verbose = 0)
    expect_equal(class(crrRes), c("pwrss", "t", "welch"))
    expect_equal(names(crrRes), c("parms", "test", "df", "ncp", "null.ncp", "t.alpha", "power", "n"))
    expect_equal(crrRes[["parms"]],
                 list(d = 0.20, null.d = 0, margin = -0.05, var.ratio = 2, n.ratio = 2, alpha = 0.05, alternative = "one.sided",
                      claim.basis = "md.pval", ceiling = TRUE, verbose = 0, pretty = FALSE))
    expect_equal(crrRes[c("test", "df", "ncp", "null.ncp", "t.alpha", "power", "n")],
                 list(test = "t", df = 315.109859, ncp = 1.99221018, null.ncp = -0.49805255, t.alpha = 1.149433434,
                      power = 0.8003316, n = c(n1 = 238, n2 = 119)))

    crrRes <- power.t.welch(d = 0.20, margin = 0.05, n.ratio = 2, var.ratio = 2, power = 0.80, alternative = "one.sided", verbose = 0)
    expect_equal(class(crrRes), c("pwrss", "t", "welch"))
    expect_equal(names(crrRes), c("parms", "test", "df", "ncp", "null.ncp", "t.alpha", "power", "n"))
    expect_equal(crrRes[["parms"]],
                 list(d = 0.20, null.d = 0, margin = 0.05, var.ratio = 2, n.ratio = 2, alpha = 0.05, alternative = "one.sided",
                      claim.basis = "md.pval", ceiling = TRUE, verbose = 0, pretty = FALSE))
    expect_equal(crrRes[c("test", "df", "ncp", "null.ncp", "t.alpha", "power", "n")],
                 list(test = "t", df = 880.444, ncp = 3.32198134, null.ncp = 0.830495336, t.alpha = 2.478918093,
                      power = 0.80018977, n = c(n1 = 662, n2 = 331)))

    crrRes <- power.t.welch(d = 0.20, margin = 0.05, n.ratio = 2, var.ratio = 2, n2 = 331, alternative = "one.sided", verbose = 0)
    expect_equal(class(crrRes), c("pwrss", "t", "welch"))
    expect_equal(names(crrRes), c("parms", "test", "df", "ncp", "null.ncp", "t.alpha", "power", "n"))
    expect_equal(crrRes[["parms"]],
                 list(d = 0.20, null.d = 0, margin = 0.05, var.ratio = 2, n.ratio = 2, alpha = 0.05, alternative = "one.sided",
                      claim.basis = "md.pval", ceiling = TRUE, verbose = 0, pretty = FALSE))
    expect_equal(crrRes[c("test", "df", "ncp", "null.ncp", "t.alpha", "power", "n")],
                 list(test = "t", df = 880.444, ncp = 3.32198134, null.ncp = 0.830495336, t.alpha = 2.478918093,
                      power = 0.80018977, n = c(n1 = 662, n2 = 331)))

    crrRes <- power.t.welch(d = 0, margin = c(-0.05, 0.05), n.ratio = 2, var.ratio = 2, power = 0.80, alternative = "two.one.sided", verbose = 0)
    expect_equal(class(crrRes), c("pwrss", "t", "welch"))
    expect_equal(names(crrRes), c("parms", "test", "df", "ncp", "null.ncp", "t.alpha", "power", "n"))
    expect_equal(crrRes[["parms"]],
                 list(d = 0, null.d = 0, margin = 0.05 * c(-1, 1), var.ratio = 2, n.ratio = 2, alpha = 0.05, alternative = "two.one.sided",
                      claim.basis = "md.pval", ceiling = TRUE, verbose = 0, pretty = FALSE))
    expect_equal(crrRes[c("test", "df", "ncp", "null.ncp", "t.alpha", "power", "n")],
                 list(test = "t", df = 10960.44441, ncp = 0, null.ncp = 2.926554671 * c(-1, 1), t.alpha = 1.28166865 * c(-1, 1),
                      power = 0.800013981, n = c(n1 = 8222, n2 = 4111)))

    crrRes <- power.t.welch(d = 0, margin = c(-0.05, 0.05), n.ratio = 2, var.ratio = 2, n2 = 4111, alternative = "two.one.sided", verbose = 0)
    expect_equal(class(crrRes), c("pwrss", "t", "welch"))
    expect_equal(names(crrRes), c("parms", "test", "df", "ncp", "null.ncp", "t.alpha", "power", "n"))
    expect_equal(crrRes[["parms"]],
                 list(d = 0, null.d = 0, margin = 0.05 * c(-1, 1), var.ratio = 2, n.ratio = 2, alpha = 0.05, alternative = "two.one.sided",
                      claim.basis = "md.pval", ceiling = TRUE, verbose = 0, pretty = FALSE))
    expect_equal(crrRes[c("test", "df", "ncp", "null.ncp", "t.alpha", "power", "n")],
                 list(test = "t", df = 10960.44441, ncp = 0, null.ncp = 2.926554671 * c(-1, 1), t.alpha = 1.28166865 * c(-1, 1),
                      power = 0.800013981, n = c(n1 = 8222, n2 = 4111)))

    crrRes <- power.t.welch(d = 0.05, margin = c(-0.10, -0.05), n.ratio = 2, var.ratio = 2, power = 0.80,
                            alternative = "two.one.sided", verbose = 0)
    expect_equal(class(crrRes), c("pwrss", "t", "welch"))
    expect_equal(names(crrRes), c("parms", "test", "df", "ncp", "null.ncp", "t.alpha", "power", "n"))
    expect_equal(crrRes[["parms"]],
                 list(d = 0.05, null.d = 0, margin = c(-0.10, -0.05), var.ratio = 2, n.ratio = 2, alpha = 0.05, alternative = "two.one.sided",
                      claim.basis = "md.pval", ceiling = TRUE, verbose = 0, pretty = FALSE))
    expect_equal(crrRes[c("test", "df", "ncp", "null.ncp", "t.alpha", "power", "n")],
                 list(test = "t", df = 2509.77762, ncp = 1.400942178, null.ncp = c(-2.801884357, -1.400942178),
                      t.alpha = c(-4.766751724, 0.559138526), power = 0.80005932, n = c(n1 = 1884, n2 = 942)))

    crrRes <- power.t.welch(d = 0.05, margin = c(-0.10, -0.05), n.ratio = 2, var.ratio = 2, n2 = 942,
                            alternative = "two.one.sided", verbose = 0)
    expect_equal(class(crrRes), c("pwrss", "t", "welch"))
    expect_equal(names(crrRes), c("parms", "test", "df", "ncp", "null.ncp", "t.alpha", "power", "n"))
    expect_equal(crrRes[["parms"]],
                 list(d = 0.05, null.d = 0, margin = c(-0.10, -0.05), var.ratio = 2, n.ratio = 2, alpha = 0.05, alternative = "two.one.sided",
                      claim.basis = "md.pval", ceiling = TRUE, verbose = 0, pretty = FALSE))
    expect_equal(crrRes[c("test", "df", "ncp", "null.ncp", "t.alpha", "power", "n")],
                 list(test = "t", df = 2509.77762, ncp = 1.400942178, null.ncp = c(-2.801884357, -1.400942178),
                      t.alpha = c(-4.766751724, 0.559138526), power = 0.80005932, n = c(n1 = 1884, n2 = 942)))

    expect_equal(power.t.welch(d = 0.20, n.ratio = 2, var.ratio = 1, power = 0.80, alternative = "two.sided", verbose = 0),
                 pwrss.t.2means(mu1 = 0.20, sd1 = 1, kappa = 2, power = 0.8, alternative = "not equal", verbose = FALSE))

    expect_equal(power.t.welch(d = 0.20, n.ratio = 2, var.ratio = 1, power = 0.80, alternative = "one.sided", verbose = 0),
                 pwrss.t.2means(mu1 = 0.20, sd1 = 1, kappa = 2, power = 0.8, alternative = "less", verbose = FALSE))

    expect_equal(power.t.welch(d = 0.20, margin = c(-0.1, 0.1), n.ratio = 2, var.ratio = 1, power = 0.80, alternative = "two.one.sided", verbose = 0),
                 pwrss.t.2means(mu1 = 0.20, sd1 = 1, margin = 0.10, kappa = 2, power = 0.8, alternative = "equivalent", verbose = FALSE))

    expect_equal(power.t.welch(d = 0.20, n.ratio = 1 / 2, var.ratio = 1, n2 = 400, alternative = "two.sided", verbose = 0),
                 pwrss.t.2means(mu1 = 0.20, sd1 = 1, kappa = 1 / 2, n2 = 400, alternative = "not equal", verbose = FALSE))

    expect_equal(power.t.welch(d = 0.20, n.ratio = 1 / 2, var.ratio = 1, n2 = 400, alternative = "one.sided", verbose = 0),
                 pwrss.t.2means(mu1 = 0.20, sd1 = 1, kappa = 1 / 2, n2 = 400, alternative = "less", verbose = FALSE))

    expect_equal(power.t.welch(d = 0.20, margin = c(-0.1, 0.1), n.ratio = 2, var.ratio = 1, n2 = 800, alternative = "two.one.sided", verbose = 0),
                 pwrss.t.2means(mu1 = 0.20, sd1 = 1, margin = 0.1, kappa = 2, n2 = 800, alternative = "equivalent", verbose = FALSE))

    expect_error(power.t.welch(d = 1e-4, power = 1 - 1e-4, alpha = 1e-4, alternative = "two.sided"),
                 "Design is not feasible.")
    expect_warning(pwrss.t.2means(mu1 = 0.20, sd1 = 1, power = 0.8, welch.df = FALSE, alternative = "not equal", verbose = FALSE),
                   "Forcing welch.df = TRUE.")
    expect_warning(pwrss.t.2means(mu1 = -0.20, sd1 = 1, margin = 0.1, power = 0.80, alternative = "not equal", verbose = FALSE),
                   "Margin is forced to be 0 for the 'two.sided' test.")

    # pwrss.z.mean, pwrss.z.2means (not longer supported) --------------------------------------------------------------
    expect_error(pwrss.z.mean(),   "This function is no longer available. Please use `power.t.student\\(\\)`.")
    expect_error(pwrss.z.2means(), "This function is no longer available. Please use `power.t.student\\(\\)` or `power.t.welch\\(\\)`.")
})
