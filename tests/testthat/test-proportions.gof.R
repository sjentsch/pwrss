test_that("proportions.gof.R works", {
    # power.chisq.gof (= pwrss.chisq.gofit) ----------------------------------------------------------------------------
    mtxW <- probs.to.w(c(0.28, 0.72), verbose = 0)
    crrRes <- power.chisq.gof(w = mtxW$w, df = mtxW$df, power = 0.80, alpha = 0.05, verbose = 0)
    crrOut <- capture.output(power.chisq.gof(w = mtxW$w, df = mtxW$df, power = 0.80, alpha = 0.05))
    crrDtl <- capture.output(power.chisq.gof(w = mtxW$w, df = mtxW$df, power = 0.80, alpha = 0.05, verbose = 2))
    crrPty <- capture.output(power.chisq.gof(w = mtxW$w, df = mtxW$df, power = 0.80, alpha = 0.05, pretty = TRUE))
    crrPnD <- capture.output(power.chisq.gof(w = mtxW$w, df = mtxW$df, power = 0.80, alpha = 0.05, verbose = 2, pretty = TRUE))
    expect_equal(class(crrRes), c("pwrss", "chisq", "gof"))
    expect_equal(names(crrRes), c("parms", "test", "df", "ncp", "null.ncp", "chisq.alpha", "power", "n"))
    expect_equal(crrRes[["parms"]],
                 list(w = mtxW$w, null.w = 0, df = mtxW$df, alpha = 0.05, ceiling = TRUE, verbose = 0, pretty = FALSE))
    expect_equal(crrRes[c("test", "df", "ncp", "null.ncp", "chisq.alpha", "power", "n")],
                 list(test = "Chi-Square Test for Goodness-of-Fit or Independence", df = mtxW$df, ncp = 7.9376, null.ncp = 0,
                      chisq.alpha = 3.84145882, power = 0.8043919, n = 41))
    expect_equal(crrOut, c("+--------------------------------------------------+",
                           "|             SAMPLE SIZE CALCULATION              |",
                           "+--------------------------------------------------+", "",
                           "Chi-Square Test for Goodness-of-Fit or Independence", "",
                           "----------------------------------------------------",
                           "Hypotheses",
                           "----------------------------------------------------",
                           "  H0 (Null Claim) : P[i,j]  = P0[i,j] for all (i,j)",
                           "  H1 (Alt. Claim) : P[i,j] != P0[i,j] for some (i,j)", "",
                           "----------------------------------------------------",
                           "Results",
                           "----------------------------------------------------",
                           "  Sample Size          = 41  <<",
                           "  Type 1 Error (alpha) = 0.050",
                           "  Type 2 Error (beta)  = 0.196",
                           "  Statistical Power    = 0.804", ""))
    expect_equal(crrDtl, c("+--------------------------------------------------+",
                           "|             SAMPLE SIZE CALCULATION              |",
                           "+--------------------------------------------------+", "",
                           "Chi-Square Test for Goodness-of-Fit or Independence", "",
                           "----------------------------------------------------",
                           "Hypotheses",
                           "----------------------------------------------------",
                           "  H0 (Null Claim) : P[i,j]  = P0[i,j] for all (i,j)",
                           "  H1 (Alt. Claim) : P[i,j] != P0[i,j] for some (i,j)", "",
                           "----------------------------------------------------",
                           "Key Parameters",
                           "----------------------------------------------------",
                           "  Degrees of Freedom     = 1 ",
                           "  Non-centrality of Alt. = 7.938 ",
                           "  Non-centrality of Null = 0.000 ",
                           "  Critical Value         = 3.841", "",
                           "----------------------------------------------------",
                           "Results",
                           "----------------------------------------------------",
                           "  Sample Size          = 41  <<",
                           "  Type 1 Error (alpha) = 0.050",
                           "  Type 2 Error (beta)  = 0.196",
                           "  Statistical Power    = 0.804", "",
                           "----------------------------------------------------",
                           "Definitions",
                           "----------------------------------------------------",
                           "  For goodness-of-fit, comparisons are P[i] vs P0[i] ",
                           "  For independence, comparisons are P[i,j] vs P0[i,j] ",
                           "  Independence implies (default) P0[i,j] = P[i,.] * P[.,j] ", "",
                           "  P[i,j] : Joint probability for cell (i,j) ",
                           "  P[i,.] : Marginal probability for row i (sum over j) ",
                           "  P[.,j] : Marginal probability for column j (sum over i) ", ""))
    expect_equal(crrPty, c("╔══════════════════════════════════════════════════╗",
                           "║           \033[34m SAMPLE SIZE CALCULATION \033[0m              ║",
                           "╚══════════════════════════════════════════════════╝", "",
                           "Chi-Square Test for Goodness-of-Fit or Independence", "",
                           "────────────────────────────────────────────────────",
                           "Hypotheses",
                           "────────────────────────────────────────────────────",
                           "  H₀ (Null)         : P[i,j] = P₀[i,j] for ∀(i,j) ",
                           "  H₁ (Alternative)  : P[i,j] ≠ P₀[i,j] for ∃(i,j)", "",
                           "────────────────────────────────────────────────────",
                           "Results",
                           "────────────────────────────────────────────────────",
                           "  \033[34mSample Size        = 41\033[0m  \033[1;35m◄◄\033[0m",
                           "  Type 1 Error (α)   = 0.050",
                           "  Type 2 Error (β)   = 0.196",
                           "  Statistical Power  = 0.804", ""))
    expect_equal(crrPnD, c("╔══════════════════════════════════════════════════╗",
                           "║           \033[34m SAMPLE SIZE CALCULATION \033[0m              ║",
                           "╚══════════════════════════════════════════════════╝", "",
                           "Chi-Square Test for Goodness-of-Fit or Independence", "",
                           "────────────────────────────────────────────────────",
                           "Hypotheses",
                           "────────────────────────────────────────────────────",
                           "  H₀ (Null)         : P[i,j] = P₀[i,j] for ∀(i,j) ",
                           "  H₁ (Alternative)  : P[i,j] ≠ P₀[i,j] for ∃(i,j)", "",
                           "────────────────────────────────────────────────────",
                           "Key Parameters",
                           "────────────────────────────────────────────────────",
                           "  df                = 1",
                           "  λ                 = 7.938 ",
                           "  λ₀                 = 0.000 ", "",
                           "────────────────────────────────────────────────────",
                           "Results",
                           "────────────────────────────────────────────────────",
                           "  \033[34mSample Size        = 41\033[0m  \033[1;35m◄◄\033[0m",
                           "  Type 1 Error (α)   = 0.050",
                           "  Type 2 Error (β)   = 0.196",
                           "  Statistical Power  = 0.804", "",
                           "\033[36m────────────────────────────────────────────────────\033[0m",
                           "\033[36mDefinitions\033[0m",
                           "\033[36m────────────────────────────────────────────────────\033[0m",
                           "\033[36m  Independence implies P₀[i,j] = P[i,.] * P[.,j] ",
                           "\033[0m\033[36m  P[i,j] : Joint prob. for cell (i,j) ",
                           "\033[0m\033[36m  P[i,.] : Marginal prob. for row i (sum over j) ",
                           "\033[0m\033[36m  P[.,j] : Marginal prob. for column j (sum over i) ",
                           "\033[0m\033[36m  For goodness-of-fit test, it is P[i] vs P₀[i] ", "",
                           "\033[0m\033[36m  λ      : Non-centrality parameter under alternative",
                           "\033[0m\033[36m  λ₀      : Non-centrality parameter under null", "", "\033[0m"))
    expect_equal(crrRes, pwrss.chisq.gofit(w = mtxW$w, df = mtxW$df, power = 0.80, verbose = FALSE))
    expect_equal(crrRes, pwrss.chisq.gofit(p1 = mtxW$prob.matrix, power = 0.80, verbose = FALSE))
    expect_equal(crrRes, pwrss.chisq.gofit(p1 = mtxW$prob.matrix, p0 = mtxW$null.prob.matrix, power = 0.80, verbose = FALSE))
    expect_equal(crrRes, pwrss.chisq.gofit(w = mtxW$w, df = mtxW$df, n = 41, verbose = FALSE))

    crrRes <- power.chisq.gof(w = mtxW$w, df = mtxW$df, n = 41, alpha = 0.05, verbose = 0)
#   crrOut <- capture.output(power.chisq.gof(w = mtxW$w, df = mtxW$df, n = 41, alpha = 0.05))
#   crrDtl <- capture.output(power.chisq.gof(w = mtxW$w, df = mtxW$df, n = 41, alpha = 0.05, verbose = 2))
#   crrPty <- capture.output(power.chisq.gof(w = mtxW$w, df = mtxW$df, n = 41, alpha = 0.05, pretty = TRUE))
#   crrPnD <- capture.output(power.chisq.gof(w = mtxW$w, df = mtxW$df, n = 41, alpha = 0.05, verbose = 2, pretty = TRUE))
    expect_equal(class(crrRes), c("pwrss", "chisq", "gof"))
    expect_equal(names(crrRes), c("parms", "test", "df", "ncp", "null.ncp", "chisq.alpha", "power", "n"))
    expect_equal(crrRes[["parms"]],
                 list(w = mtxW$w, null.w = 0, df = mtxW$df, alpha = 0.05, ceiling = TRUE, verbose = 0, pretty = FALSE))
    expect_equal(crrRes[c("test", "df", "ncp", "null.ncp", "chisq.alpha", "power", "n")],
                 list(test = "Chi-Square Test for Goodness-of-Fit or Independence", df = mtxW$df, ncp = 7.9376, null.ncp = 0,
                      chisq.alpha = 3.84145882, power = 0.8043919, n = 41))

    mtxW <- probs.to.w(rbind(c(0.056, 0.132), c(0.944, 0.868)), verbose = 0)
    crrRes <- power.chisq.gof(w = mtxW$w, df = mtxW$df, power = 0.80, alpha = 0.05, verbose = 0)
    expect_equal(class(crrRes), c("pwrss", "chisq", "gof"))
    expect_equal(names(crrRes), c("parms", "test", "df", "ncp", "null.ncp", "chisq.alpha", "power", "n"))
    expect_equal(crrRes[["parms"]],
                 list(w = mtxW$w, null.w = 0, df = mtxW$df, alpha = 0.05, ceiling = TRUE, verbose = 0, pretty = FALSE))
    expect_equal(crrRes[c("test", "df", "ncp", "null.ncp", "chisq.alpha", "power", "n")],
                 list(test = "Chi-Square Test for Goodness-of-Fit or Independence", df = mtxW$df, ncp = 7.8504063, null.ncp = 0,
                      chisq.alpha = 3.84145882, power = 0.80007722, n = 463))

    crrRes <- power.chisq.gof(w = mtxW$w, df = mtxW$df, n = 463, alpha = 0.05, verbose = 0)
    crrRes <- power.chisq.gof(w = mtxW$w, df = mtxW$df, power = 0.80, alpha = 0.05, verbose = 0)
    expect_equal(class(crrRes), c("pwrss", "chisq", "gof"))
    expect_equal(names(crrRes), c("parms", "test", "df", "ncp", "null.ncp", "chisq.alpha", "power", "n"))
    expect_equal(crrRes[["parms"]],
                 list(w = mtxW$w, null.w = 0, df = mtxW$df, alpha = 0.05, ceiling = TRUE, verbose = 0, pretty = FALSE))
    expect_equal(crrRes[c("test", "df", "ncp", "null.ncp", "chisq.alpha", "power", "n")],
                 list(test = "Chi-Square Test for Goodness-of-Fit or Independence", df = mtxW$df, ncp = 7.8504063, null.ncp = 0,
                      chisq.alpha = 3.84145882, power = 0.80007722, n = 463))

    mtxW <- probs.to.w(cbind(c(0.6759, 0.1559, 0.1281, 0.0323, 0.0078), c(0.6771, 0.1519, 0.1368, 0.0241, 0.0101)), verbose = 0)
    crrRes <- power.chisq.gof(w = mtxW$w, df = mtxW$df, power = 0.80, alpha = 0.05, verbose = 0)
    expect_equal(class(crrRes), c("pwrss", "chisq", "gof"))
    expect_equal(names(crrRes), c("parms", "test", "df", "ncp", "null.ncp", "chisq.alpha", "power", "n"))
    expect_equal(crrRes[["parms"]],
                 list(w = mtxW$w, null.w = 0, df = mtxW$df, alpha = 0.05, ceiling = TRUE, verbose = 0, pretty = FALSE))
    expect_equal(crrRes[c("test", "df", "ncp", "null.ncp", "chisq.alpha", "power", "n")],
                 list(test = "Chi-Square Test for Goodness-of-Fit or Independence", df = mtxW$df, ncp = 11.9353027, null.ncp = 0,
                      chisq.alpha = 9.48772904, power = 0.80000063, n = 13069))

    crrRes <- power.chisq.gof(w = mtxW$w, df = mtxW$df, n = 13069, alpha = 0.05, verbose = 0)
    expect_equal(class(crrRes), c("pwrss", "chisq", "gof"))
    expect_equal(names(crrRes), c("parms", "test", "df", "ncp", "null.ncp", "chisq.alpha", "power", "n"))
    expect_equal(crrRes[["parms"]],
                 list(w = mtxW$w, null.w = 0, df = mtxW$df, alpha = 0.05, ceiling = TRUE, verbose = 0, pretty = FALSE))
    expect_equal(crrRes[c("test", "df", "ncp", "null.ncp", "chisq.alpha", "power", "n")],
                 list(test = "Chi-Square Test for Goodness-of-Fit or Independence", df = mtxW$df, ncp = 11.9353027, null.ncp = 0,
                      chisq.alpha = 9.48772904, power = 0.80000063, n = 13069))

    expect_error(power.chisq.gof(w = 0.01, null.w = 0.1, df = 1, power = 0.80, alpha = 0.05, verbose = 0),
                 "`w` should be greater than or equal to `null.w`.")
    expect_error(pwrss.chisq.gofit(p1 = data.frame, df = 1, power = 0.80, alpha = 0.05, verbose = FALSE),
                 "`p1` needs to be either a vector or a matrix.")
    expect_error(pwrss.chisq.gofit(p1 = array(1, c(2, 2, 2)), df = 1, power = 0.80, alpha = 0.05, verbose = FALSE),
                 "`p1` needs to be either a vector or a matrix.")
    expect_error(pwrss.chisq.gofit(p1 = c(0.280, 0.721), df = 1, power = 0.80, alpha = 0.05, verbose = FALSE),
                 "Cell probabilities in `p1` \\(and `p0` if given\\) should sum to 1.")
    expect_error(pwrss.chisq.gofit(p1 = matrix(c(0.2, 0.3, 0.4, 0.3, 0.5, 0.2), ncol = 2), df = 1, power = 0.80),
                 "Cell probabilities \\(per column\\) in `p1` \\(and `p0` if given\\) should sum to 1.")
    expect_error(pwrss.chisq.gofit(p1 = matrix(c(0.2, 0.5, 0.3), ncol = 1), p0 = matrix(c(0.2, 0.5, 0.3), nrow = 1), df = 1, power = 0.80),
                 "Dimensions of `p1` and `p0` do not match up.")
    expect_error(pwrss.chisq.gofit(w = 0.44, power = 0.80, alpha = 0.05, verbose = 0),
                 "You need to specify both `w` and `df`.")
    expect_warning(pwrss.chisq.gofit(p1 = c(0.72, 0.28), w = 0.44, df = 1, power = 0.80, alpha = 0.05, verbose = 0),
                   "Ignoring any specifications to `p1`, or `p0`.")
})
