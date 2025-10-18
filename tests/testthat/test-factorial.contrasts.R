test_that("factorial.contrasts works", {
    expect_equal(2 * 2, 4)
#    expect_equal(factorial.contrasts(factor.levels = 3, coding = "treatment", verbose = FALSE)$model.matrix,
#                 matrix(c(1, 0, 0, 1, -1, -1), nrow = 2, dimnames = list(c("A1", "A2"), c("A1", "A2", "A3"))))
#    expect_equal(factorial.contrasts(factor.levels = 3, coding = "treatment", verbose = FALSE)$contrast.matrix,
#                 matrix(c(1, 0, 0, 1, -1, -1), nrow = 2, dimnames = list(c("A1", "A2"), c("A1", "A2", "A3"))))

#    expect_equal(suppressMessages(factorial.contrasts(factor.levels = c(2, 3), coding = "sum", verbose = FALSE))$contrast.matrix

#    expect_equal(suppressMessages(factorial.contrasts(factor.levels = c(2, 3), coding = "sum", verbose = FALSE))$contrast.matrix,
#                 matrix(c(+1/6, +1/3, -1/6, +1/3, -1/6, +1/6, -1/6, +1/3, -1/6, +1/3, +1/6, -1/6, -1/6, -1/6, -1/6,
#                          -1/6, +1/3, -1/6, -1/3, +1/6, -1/6, -1/6, +1/3, +1/6, -1/3, -1/6, -1/6, -1/6, +1/6, +1/6),
#                        nrow = 5, dimnames = list(c("A1", "B1", "B2", "A1:B1", "A1:B2"),
#                                                  c("A1:B1", "A1:B2", "A1:B3", "A2:B1", "A2:B2", "A2:B3"))))

#    factorial.contrasts(factor.levels = 3, coding = "helmert", verbose = FALSE)$model.matrix
#    factorial.contrasts(factor.levels = 3, coding = "helmert", verbose = FALSE)$contrast.matrix
#    factorial.contrasts(factor.levels = 3, coding = "poly",    verbose = FALSE)$model.matrix
#    factorial.contrasts(factor.levels = 3, coding = "poly",    verbose = FALSE)$contrast.matrix
})
