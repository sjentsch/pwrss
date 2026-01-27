test_that("checks.R works", {
    # check.proportion
    expect_null(unlist(lapply(seq(0, 10) / 10, check.proportion)))
    expect_error(check.proportion(-1, 2, 1.1, 0, 1),
                 "Arguments `-1`, `2`, `1.1` do not have valid values \\(must be numeric, length 1, >= 0 and <= 1\\)")
    # check.correlation
    expect_null(unlist(lapply(seq(-10, 10) / 10, check.correlation)))
    expect_error(check.correlation(-1.1, 2, 1.1, 0, 1, -1),
                 "Arguments `-1.1`, `2`, `1.1` do not have valid values \\(must be numeric, length 1, >= -1 and <= 1\\)")
    # check.logical
    expect_null(unlist(lapply(sample(c(TRUE, FALSE), 20, replace = TRUE), check.logical)))
    expect_error(check.logical(1, 0, NA, TRUE, FALSE),
                 "Arguments `1`, `0`, `NA` do not have valid logical values \\(must be TRUE or FALSE only\\)")
    # check.sample.size
    expect_null(unlist(lapply(c(2, sample(10000, 20)), check.sample.size)))
    expect_error(check.sample.size(1, 0, 1.1, NA, Inf, 2, 100),
                 "Arguments `1`, `0`, `1.1`, `NA`, `Inf` do not have valid sample size values \\(must be integer-like, > 1, and finite\\)")
    # check.nonnegative
    expect_null(unlist(lapply(seq(0, 100) / 10, check.nonnegative)))
    expect_error(check.nonnegative(-1, NA, Inf, 0, 1, 1.1, 10),
                 "Arguments `-1`, `NA`, `Inf` do not have valid non-negative values \\(must be numeric, >= 0, and finite\\)")
    # check.positive
    expect_null(unlist(lapply(seq(1, 100) / 10, check.positive)))
    expect_error(check.positive(-1, 0, NA, Inf, 1, 1.1, 10),
                 "Arguments `-1`, `0`, `NA`, `Inf` do not have valid positive values \\(must be numeric, > 0, and finite\\)")
    # check.numeric
    expect_null(unlist(lapply(rnorm(100), check.numeric)))
    expect_null(unlist(lapply(sample(1e5, 100) - 5e4, check.numeric)))    
    expect_error(check.numeric(NA, Inf, seq(3), as.complex(1), 1, 1.1, 10),
                 "Arguments `NA`, `Inf`, `seq\\(3\\)`, `as.complex\\(1\\)` do not have valid numeric values \\(must be numeric, scalar, and finite\\)")

    # check.correlation.matrix
    expect_null(check.correlation.matrix(cor(matrix(rnorm(1000), ncol = 10))))
    expect_error(check.correlation.matrix(diag(5)[-1, ]), "Correlation matrix is not square")
    expect_error(check.correlation.matrix(rbind(diag(5)[-1, ], diag(5)[1, ])), "Correlation matrix is not symmetric")
    expect_error(check.correlation.matrix(cor(matrix(rnorm(400), ncol = 4)) + matrix(rep(2, 16), nrow = 4) - (diag(4) * 2)),
                 "The values in the correlation matrix must be numeric, >= -1 and <= 1")
    expect_error(check.correlation.matrix(diag(5) + matrix(c(1, rep(0, 24)), nrow = 5)),
                 "All values in the main diagonal of the correlation matrix must be 1")
    expect_error(check.correlation.matrix(matrix(c(1, 0.7, 0.3, 0.7, 1, -0.5, 0.3, -0.5, 1), nrow = 3)),
                 "Correlation matrix is not positive definite")
    # neither I nor ChatGPT could not generate test cases for (1) a matrix that is not invertible (determinant of 0 or lower)
    # and (2) a matrix that is not well conditioned (kappa > 1000)
})
