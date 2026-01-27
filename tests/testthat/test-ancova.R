test_that("ancova.R works", {
    # power.f.ancova (= pwrss.f.ancova) --------------------------------------------------------------------------------

    # power.f.ancova.keppel --------------------------------------------------------------------------------------------

    # factorial.contrasts ----------------------------------------------------------------------------------------------
    crrRes <- factorial.contrasts(factor.levels = 3, coding = "deviation", verbose = FALSE)
    expect_equal(class(crrRes), "list")
    expect_equal(names(crrRes), c("factor.levels", "factor.data", "model.matrix", "contrast.matrix"))
    expect_equal(crrRes$factor.levels, 3)
    expect_equal(crrRes$factor.data, data.frame(A = gl(3, 1)))
    expect_equal(crrRes$model.matrix, model.matrix(~ A, data.frame(A = as.factor(seq(3))), contrasts = list(A = contr.sum(3, 3))))
    expect_equal(crrRes$contrast.matrix, matrix(c(2/3, -1/3, -1/3, 2/3, -1/3, -1/3), nrow = 2, dimnames = list(c("A1", "A2"), c("A1", "A2", "A3"))))
    expect_equal(capture.output(factorial.contrasts(factor.levels = 3, coding = "deviation", verbose = TRUE)),
                 c("       A1     A2     A3", "A1  0.667 -0.333 -0.333", "A2 -0.333  0.667 -0.333"))

    crrRes <- factorial.contrasts(factor.levels = 3, coding = "deviation", intercept = TRUE, verbose = FALSE)
    expect_equal(class(crrRes), "list")
    expect_equal(names(crrRes), c("factor.levels", "factor.data", "model.matrix", "contrast.matrix"))
    expect_equal(crrRes$factor.levels, 3)
    expect_equal(crrRes$factor.data, data.frame(A = gl(3, 1)))
    expect_equal(crrRes$model.matrix, model.matrix(~ A, data.frame(A = as.factor(seq(3))), contrasts = list(A = contr.sum(3, 3))))
    expect_equal(crrRes$contrast.matrix, matrix(c(1/3, 2/3, -1/3, 1/3, -1/3, 2/3, 1/3, -1/3, -1/3), nrow = 3,
                                                dimnames = list(c("(Intercept)", "A1", "A2"), c("A1", "A2", "A3"))))
    expect_equal(capture.output(factorial.contrasts(factor.levels = 3, coding = "deviation", verbose = TRUE)),
                 c("       A1     A2     A3", "A1  0.667 -0.333 -0.333", "A2 -0.333  0.667 -0.333"))

    crrRes <- factorial.contrasts(factor.levels = 3, coding = "treatment", verbose = FALSE)
    expect_equal(class(crrRes), "list")
    expect_equal(names(crrRes), c("factor.levels", "factor.data", "model.matrix", "contrast.matrix"))
    expect_equal(crrRes$factor.levels, 3)
    expect_equal(crrRes$factor.data, data.frame(A = gl(3, 1)))
    expect_equal(crrRes$model.matrix, model.matrix(~ A, data.frame(A = as.factor(seq(3))), contrasts = list(A = contr.treatment(3, 3))))
    expect_equal(crrRes$contrast.matrix, matrix(c(1, 0, 0, 1, -1, -1), nrow = 2, dimnames = list(c("A1", "A2"), c("A1", "A2", "A3"))))
    expect_equal(capture.output(factorial.contrasts(factor.levels = 3, coding = "treatment", verbose = TRUE)),
                 c("   A1 A2 A3", "A1  1  0 -1", "A2  0  1 -1"))

    crrRes <- factorial.contrasts(factor.levels = 3, base = 1, coding = "treatment", verbose = FALSE)
    expect_equal(class(crrRes), "list")
    expect_equal(names(crrRes), c("factor.levels", "factor.data", "model.matrix", "contrast.matrix"))
    expect_equal(crrRes$factor.levels, 3)
    expect_equal(crrRes$factor.data, data.frame(A = gl(3, 1)))
    expect_equal(crrRes$model.matrix, model.matrix(~ A, data.frame(A = as.factor(seq(3))), contrasts = list(A = contr.treatment(3, 1))))
    expect_equal(crrRes$contrast.matrix, matrix(c(-1, -1, 1, 0, 0, 1), nrow = 2, dimnames = list(c("A2", "A3"), c("A1", "A2", "A3"))))
    expect_equal(capture.output(factorial.contrasts(factor.levels = 3, coding = "treatment", verbose = TRUE)),
                 c("   A1 A2 A3", "A1  1  0 -1", "A2  0  1 -1"))

    crrRes <- factorial.contrasts(factor.levels = 3, coding = "helmert", verbose = FALSE)
    expect_equal(class(crrRes), "list")
    expect_equal(names(crrRes), c("factor.levels", "factor.data", "model.matrix", "contrast.matrix"))
    expect_equal(crrRes$factor.levels, 3)
    expect_equal(crrRes$factor.data, data.frame(A = gl(3, 1)))
    expect_equal(crrRes$model.matrix, model.matrix(~ A, data.frame(A = as.factor(seq(3))), contrasts = list(A = contr.helmert(3, 3))))
    expect_equal(crrRes$contrast.matrix, matrix(c(-1/2, -1/6, 1/2, -1/6, 0, 1/3), nrow = 2, dimnames = list(c("A1", "A2"), c("A1", "A2", "A3"))))
    expect_equal(capture.output(factorial.contrasts(factor.levels = 3, coding = "helmert", verbose = TRUE)),
                 c("       A1     A2    A3", "A1 -0.500  0.500 0.000", "A2 -0.167 -0.167 0.333"))
    
    crrRes <- factorial.contrasts(factor.levels = 3, coding = "poly", verbose = FALSE)
    expect_equal(class(crrRes), "list")
    expect_equal(names(crrRes), c("factor.levels", "factor.data", "model.matrix", "contrast.matrix"))
    expect_equal(crrRes$factor.levels, 3)
    expect_equal(crrRes$factor.data, data.frame(A = gl(3, 1)))
    expect_equal(crrRes$model.matrix, model.matrix(~ A, data.frame(A = as.factor(seq(3))), contrasts = list(A = contr.poly)))
    expect_equal(crrRes$contrast.matrix, t(matrix(as.vector(contr.poly(3)), ncol = 2, dimnames = list(c("A1", "A2", "A3"), c("A.L", "A.Q")))))
    expect_equal(capture.output(factorial.contrasts(factor.levels = 3, coding = "poly", verbose = TRUE)),
                 c("        A1     A2    A3", "A.L -0.707  0.000 0.707", "A.Q  0.408 -0.816 0.408"))

    crrRes <- suppressMessages(factorial.contrasts(factor.levels = c(4, 3), coding.scheme = "deviation", intercept = FALSE, verbose = FALSE))
    crrOut <- suppressMessages(capture.output(factorial.contrasts(factor.levels = c(4, 3), coding.scheme = "deviation",
                                                                  intercept = FALSE, verbose = TRUE)))
    crrMdM <- model.matrix(~ A * B, data.frame(A = gl(4, 3), B = gl(3, 1, 3 * 4)), contrasts = list(A = contr.sum, B = contr.sum))
    expect_equal(class(crrRes), "list")
    expect_equal(names(crrRes), c("factor.levels", "factor.data", "model.matrix", "contrast.matrix"))
    expect_equal(crrRes$factor.levels, c(4, 3))
    expect_equal(crrRes$factor.data, data.frame(A = gl(4, 3), B = gl(3, 1, 3 * 4)))
    expect_equal(crrRes$model.matrix, crrMdM)
    expect_equal(unname(crrRes$contrast.matrix), unname(solve(crrMdM)[-1, ]))
    expect_equal(dimnames(crrRes$contrast.matrix),
                 list(c("A1", "A2", "A3", "B1", "B2", "A1:B1", "A2:B1", "A3:B1", "A1:B2", "A2:B2", "A3:B2"),
                      c("A1:B1", "A1:B2", "A1:B3", "A2:B1", "A2:B2", "A2:B3", "A3:B1", "A3:B2", "A3:B3", "A4:B1", "A4:B2", "A4:B3")))
    expect_equal(crrOut, c("       A1:B1  A1:B2  A1:B3  A2:B1  A2:B2  A2:B3  A3:B1  A3:B2  A3:B3  A4:B1",
                           "A1     0.250  0.250  0.250 -0.083 -0.083 -0.083 -0.083 -0.083 -0.083 -0.083",
                           "A2    -0.083 -0.083 -0.083  0.250  0.250  0.250 -0.083 -0.083 -0.083 -0.083",
                           "A3    -0.083 -0.083 -0.083 -0.083 -0.083 -0.083  0.250  0.250  0.250 -0.083",
                           "B1     0.167 -0.083 -0.083  0.167 -0.083 -0.083  0.167 -0.083 -0.083  0.167",
                           "B2    -0.083  0.167 -0.083 -0.083  0.167 -0.083 -0.083  0.167 -0.083 -0.083",
                           "A1:B1  0.500 -0.250 -0.250 -0.167  0.083  0.083 -0.167  0.083  0.083 -0.167",
                           "A2:B1 -0.167  0.083  0.083  0.500 -0.250 -0.250 -0.167  0.083  0.083 -0.167",
                           "A3:B1 -0.167  0.083  0.083 -0.167  0.083  0.083  0.500 -0.250 -0.250 -0.167", 
                           "A1:B2 -0.250  0.500 -0.250  0.083 -0.167  0.083  0.083 -0.167  0.083  0.083",
                           "A2:B2  0.083 -0.167  0.083 -0.250  0.500 -0.250  0.083 -0.167  0.083  0.083",
                           "A3:B2  0.083 -0.167  0.083  0.083 -0.167  0.083 -0.250  0.500 -0.250  0.083",
                           "       A4:B2  A4:B3", "A1    -0.083 -0.083", "A2    -0.083 -0.083", "A3    -0.083 -0.083",
                           "B1    -0.083 -0.083", "B2     0.167 -0.083", "A1:B1  0.083  0.083", "A2:B1  0.083  0.083",
                           "A3:B1  0.083  0.083", "A1:B2 -0.167  0.083", "A2:B2 -0.167  0.083", "A3:B2 -0.167  0.083"))

    crrRes <- suppressMessages(factorial.contrasts(factor.levels = c(4, 3), coding.scheme = "poly", intercept = FALSE, verbose = FALSE))
    crrOut <- suppressMessages(capture.output(factorial.contrasts(factor.levels = c(4, 3), coding.scheme = "poly",
                                                                  intercept = FALSE, verbose = TRUE)))
    crrMdM <- model.matrix(~ A * B, data.frame(A = gl(4, 3), B = gl(3, 1, 3 * 4)), contrasts = list(A = contr.poly, B = contr.poly))
    expect_equal(class(crrRes), "list")
    expect_equal(names(crrRes), c("factor.levels", "factor.data", "model.matrix", "contrast.matrix"))
    expect_equal(crrRes$factor.levels, c(4, 3))
    expect_equal(crrRes$factor.data, data.frame(A = gl(4, 3), B = gl(3, 1, 3 * 4)))
    expect_equal(crrRes$model.matrix, crrMdM)
    expect_equal(unname(crrRes$contrast.matrix), unname(solve(crrMdM)[-1, ]))
    expect_equal(dimnames(crrRes$contrast.matrix),
                 list(c("A.L", "A.Q", "A.C", "B.L", "B.Q", "A.L:B.L", "A.Q:B.L", "A.C:B.L", "A.L:B.Q", "A.Q:B.Q", "A.C:B.Q"),
                      c("A1:B1", "A1:B2", "A1:B3", "A2:B1", "A2:B2", "A2:B3", "A3:B1", "A3:B2", "A3:B3", "A4:B1", "A4:B2", "A4:B3")))
    expect_equal(crrOut, c("         A1:B1  A1:B2  A1:B3  A2:B1  A2:B2  A2:B3  A3:B1  A3:B2  A3:B3  A4:B1",
                           "A.L     -0.224 -0.224 -0.224 -0.075 -0.075 -0.075  0.075  0.075  0.075  0.224",
                           "A.Q      0.167  0.167  0.167 -0.167 -0.167 -0.167 -0.167 -0.167 -0.167  0.167",
                           "A.C     -0.075 -0.075 -0.075  0.224  0.224  0.224 -0.224 -0.224 -0.224  0.075",
                           "B.L     -0.177  0.000  0.177 -0.177  0.000  0.177 -0.177  0.000  0.177 -0.177",
                           "B.Q      0.102 -0.204  0.102  0.102 -0.204  0.102  0.102 -0.204  0.102  0.102",
                           "A.L:B.L  0.474  0.000 -0.474  0.158  0.000 -0.158 -0.158  0.000  0.158 -0.474",
                           "A.Q:B.L -0.354  0.000  0.354  0.354  0.000 -0.354  0.354  0.000 -0.354 -0.354",
                           "A.C:B.L  0.158  0.000 -0.158 -0.474  0.000  0.474  0.474  0.000 -0.474 -0.158",
                           "A.L:B.Q -0.274  0.548 -0.274 -0.091  0.183 -0.091  0.091 -0.183  0.091  0.274",
                           "A.Q:B.Q  0.204 -0.408  0.204 -0.204  0.408 -0.204 -0.204  0.408 -0.204  0.204",
                           "A.C:B.Q -0.091  0.183 -0.091  0.274 -0.548  0.274 -0.274  0.548 -0.274  0.091",
                           "         A4:B2 A4:B3", "A.L      0.224 0.224", "A.Q      0.167 0.167", "A.C      0.075 0.075",
                           "B.L      0.000 0.177", "B.Q     -0.204 0.102", "A.L:B.L  0.000 0.474", "A.Q:B.L  0.000 0.354",
                           "A.C:B.L  0.000 0.158", "A.L:B.Q -0.548 0.274", "A.Q:B.Q -0.408 0.204", "A.C:B.Q -0.183 0.091"))

    crrRes <- suppressMessages(factorial.contrasts(factor.levels = c(4, 3), coding.scheme = c("deviation", "poly"), intercept = FALSE, verbose = FALSE))
    crrOut <- suppressMessages(capture.output(factorial.contrasts(factor.levels = c(4, 3), coding.scheme = c("deviation", "poly"),
                                                                  intercept = FALSE, verbose = TRUE)))
    crrMdM <- model.matrix(~ A * B, data.frame(A = gl(4, 3), B = gl(3, 1, 3 * 4)), contrasts = list(A = contr.sum, B = contr.poly))
    expect_equal(class(crrRes), "list")
    expect_equal(names(crrRes), c("factor.levels", "factor.data", "model.matrix", "contrast.matrix"))
    expect_equal(crrRes$factor.levels, c(4, 3))
    expect_equal(crrRes$factor.data, data.frame(A = gl(4, 3), B = gl(3, 1, 3 * 4)))
    expect_equal(crrRes$model.matrix, crrMdM)
    expect_equal(unname(crrRes$contrast.matrix), unname(solve(crrMdM)[-1, ]))
    expect_equal(dimnames(crrRes$contrast.matrix),
                 list(c("A1", "A2", "A3", "B.L", "B.Q", "A1:B.L", "A2:B.L", "A3:B.L", "A1:B.Q", "A2:B.Q", "A3:B.Q"),
                      c("A1:B1", "A1:B2", "A1:B3", "A2:B1", "A2:B2", "A2:B3", "A3:B1", "A3:B2", "A3:B3", "A4:B1", "A4:B2", "A4:B3")))
    expect_equal(crrOut, c("        A1:B1  A1:B2  A1:B3  A2:B1  A2:B2  A2:B3  A3:B1  A3:B2  A3:B3  A4:B1",
                           "A1      0.250  0.250  0.250 -0.083 -0.083 -0.083 -0.083 -0.083 -0.083 -0.083",
                           "A2     -0.083 -0.083 -0.083  0.250  0.250  0.250 -0.083 -0.083 -0.083 -0.083",
                           "A3     -0.083 -0.083 -0.083 -0.083 -0.083 -0.083  0.250  0.250  0.250 -0.083",
                           "B.L    -0.177  0.000  0.177 -0.177  0.000  0.177 -0.177  0.000  0.177 -0.177",
                           "B.Q     0.102 -0.204  0.102  0.102 -0.204  0.102  0.102 -0.204  0.102  0.102",
                           "A1:B.L -0.530  0.000  0.530  0.177  0.000 -0.177  0.177  0.000 -0.177  0.177",
                           "A2:B.L  0.177  0.000 -0.177 -0.530  0.000  0.530  0.177  0.000 -0.177  0.177",
                           "A3:B.L  0.177  0.000 -0.177  0.177  0.000 -0.177 -0.530  0.000  0.530  0.177",
                           "A1:B.Q  0.306 -0.612  0.306 -0.102  0.204 -0.102 -0.102  0.204 -0.102 -0.102",
                           "A2:B.Q -0.102  0.204 -0.102  0.306 -0.612  0.306 -0.102  0.204 -0.102 -0.102",
                           "A3:B.Q -0.102  0.204 -0.102 -0.102  0.204 -0.102  0.306 -0.612  0.306 -0.102",
                           "        A4:B2  A4:B3", "A1     -0.083 -0.083", "A2     -0.083 -0.083", "A3     -0.083 -0.083",
                           "B.L     0.000  0.177", "B.Q    -0.204  0.102", "A1:B.L  0.000 -0.177", "A2:B.L  0.000 -0.177",
                           "A3:B.L  0.000 -0.177", "A1:B.Q  0.204 -0.102", "A2:B.Q  0.204 -0.102", "A3:B.Q  0.204 -0.102" ))

    crrRes <- suppressMessages(factorial.contrasts(factor.levels = c(3, 2, 2), coding.scheme = "poly", intercept = FALSE, verbose = FALSE))
    crrOut <- suppressMessages(capture.output(factorial.contrasts(factor.levels = c(3, 2, 2), coding.scheme = "poly",
                                                                  intercept = FALSE, verbose = TRUE)))
    crrMdM <- model.matrix(~ A * B * C, data.frame(A = gl(3, 4), B = gl(2, 2, 12), C = gl(2, 1, 12)),
                           contrasts = list(A = contr.poly, B = contr.poly, C = contr.poly))
    expect_equal(class(crrRes), "list")
    expect_equal(names(crrRes), c("factor.levels", "factor.data", "model.matrix", "contrast.matrix"))
    expect_equal(crrRes$factor.levels, c(3, 2, 2))
    expect_equal(crrRes$factor.data, data.frame(A = gl(3, 4), B = gl(2, 2, 12), C = gl(2, 1, 12)))
    expect_equal(crrRes$model.matrix, crrMdM)
    expect_equal(unname(crrRes$contrast.matrix), unname(solve(crrMdM)[-1, ]))
    expect_equal(dimnames(crrRes$contrast.matrix),
                 list(c("A.L", "A.Q", "B.L", "C.L", "A.L:B.L", "A.Q:B.L", "A.L:C.L", "A.Q:C.L", "B.L:C.L", "A.L:B.L:C.L", "A.Q:B.L:C.L"), 
                      c("A1:B1:C1", "A1:B1:C2", "A1:B2:C1", "A1:B2:C2", "A2:B1:C1", "A2:B1:C2",
                        "A2:B2:C1", "A2:B2:C2", "A3:B1:C1", "A3:B1:C2", "A3:B2:C1", "A3:B2:C2")))
    expect_equal(crrOut, c("            A1:B1:C1 A1:B1:C2 A1:B2:C1 A1:B2:C2 A2:B1:C1 A2:B1:C2 A2:B2:C1",
                           "A.L           -0.177   -0.177   -0.177   -0.177    0.000    0.000    0.000",
                           "A.Q            0.102    0.102    0.102    0.102   -0.204   -0.204   -0.204",
                           "B.L           -0.118   -0.118    0.118    0.118   -0.118   -0.118    0.118",
                           "C.L           -0.118    0.118   -0.118    0.118   -0.118    0.118   -0.118",
                           "A.L:B.L        0.250    0.250   -0.250   -0.250    0.000    0.000    0.000",
                           "A.Q:B.L       -0.144   -0.144    0.144    0.144    0.289    0.289   -0.289",
                           "A.L:C.L        0.250   -0.250    0.250   -0.250    0.000    0.000    0.000",
                           "A.Q:C.L       -0.144    0.144   -0.144    0.144    0.289   -0.289    0.289",
                           "B.L:C.L        0.167   -0.167   -0.167    0.167    0.167   -0.167   -0.167",
                           "A.L:B.L:C.L   -0.354    0.354    0.354   -0.354    0.000    0.000    0.000",
                           "A.Q:B.L:C.L    0.204   -0.204   -0.204    0.204   -0.408    0.408    0.408",
                           "            A2:B2:C2 A3:B1:C1 A3:B1:C2 A3:B2:C1 A3:B2:C2",
                           "A.L            0.000    0.177    0.177    0.177    0.177",
                           "A.Q           -0.204    0.102    0.102    0.102    0.102",
                           "B.L            0.118   -0.118   -0.118    0.118    0.118",
                           "C.L            0.118   -0.118    0.118   -0.118    0.118",
                           "A.L:B.L        0.000   -0.250   -0.250    0.250    0.250",
                           "A.Q:B.L       -0.289   -0.144   -0.144    0.144    0.144",
                           "A.L:C.L        0.000   -0.250    0.250   -0.250    0.250",
                           "A.Q:C.L       -0.289   -0.144    0.144   -0.144    0.144",
                           "B.L:C.L        0.167    0.167   -0.167   -0.167    0.167",
                           "A.L:B.L:C.L    0.000    0.354   -0.354   -0.354    0.354",
                           "A.Q:B.L:C.L   -0.408    0.204   -0.204   -0.204    0.204"))

    # power.f.ancova.shieh ---------------------------------------------------------------------------------------------
    
    # power.t.contrast -------------------------------------------------------------------------------------------------
    
    # adjust.alpha -----------------------------------------------------------------------------------------------------
    
    # power.t.contrasts ------------------------------------------------------------------------------------------------   

})
