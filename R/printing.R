# helper and formatting functions --------------------------------------------------------------------------------------
.header_ascii <- function(requested) {
  RC <- ifelse(requested == "n",
               "             SAMPLE SIZE CALCULATION              ",
               "                POWER CALCULATION                 ")

  paste0(paste0("+", strrep("-", 50), "+", "\n"),
         paste0("|", RC,              "|", "\n"),
         paste0("+", strrep("-", 50), "+", "\n\n"))

}

.topic_ascii <- function(topic) {
  paste0(paste0(strrep("-", 52), "\n"),
         paste0(topic, "\n"),
         paste0(strrep("-", 52), "\n"))
}

.keyparms_ascii <- function(x, parmlist, digits) {

}

.sign_h0 <- function(alt = c("two.sided", "one.sided"), less = FALSE) {
  alt <- match.arg(alt)

  ifelse(alt == "two.sided", " =", ifelse(less, ">=", "<="))
}

.sign_h1 <- function(alt = c("two.sided", "one.sided"), less = FALSE) {
  alt <- match.arg(alt)

  ifelse(alt == "two.sided", "!=", ifelse(less, " <", " >"))
}

.hypotheses_ascii <- function(h0_text, h1_text) {
  if (length(h0_text) > 1) h0_text <- paste(h0_text, collapse = paste0("\n", strrep(" ", 20)))
  if (length(h1_text) > 1) h1_text <- paste(h1_text, collapse = paste0("\n", strrep(" ", 20)))

  paste0(.topic_ascii("Hypotheses"),
         sprintf("  H0 (Null Claim) : %s\n",   h0_text),
         sprintf("  H1 (Alt. Claim) : %s\n\n", h1_text))
}

.results_ascii <- function(x, digits = 3) {
  if (any(c("n", "n.total", "n.paired") %in% names(x))) {
    n_prefix <- ifelse(utils::hasName(x, "n.total") && !utils::hasName(x, "n"), "Total ", ifelse(utils::hasName(x, "n.paired"), "Paired ", ""))
    n_pad    <- strrep(" ", 9 - nchar(n_prefix))
    n        <- x[[c("n", "n.total", "n.paired")[c("", "Total ", "Paired ") %in% n_prefix]]]
    n_text   <- paste(round(n, digits), collapse = ifelse(length(n) == 2, " and ", ", "))
    n_line   <- sprintf("  %sSample Size%s = %s%s\n", n_prefix, n_pad, n_text,  ifelse(x$requested == "n",     "  <<", ""))
  } else {
    n_line <- ""
  }

  paste0(.topic_ascii("Results"),
         n_line,
         sprintf("  Type 1 Error (alpha) = %.*f\n",     digits, x$alpha),
         sprintf("  Type 2 Error (beta)  = %.*f\n",     digits, 1 - x$power),
         sprintf("  Statistical Power    = %.*f%s\n\n", digits, x$power, ifelse(x$requested == "power", "  <<", "")))
}

.defs_ascii <- function() {

}


# print functions ------------------------------------------------------------------------------------------------------
.print.ascii.pwrss.logistic <- function(x, digits = 3, verbose = 1, ...) {

  cat(.header_ascii(x$requested))
  cat(x$test, "\n\n", sep = "")
  cat("  Method          : ", x$method, "\n", sep = "")
  cat("  Predictor Dist. : ", x$dist, "\n\n", sep = "")

  h0_text <- paste("Odds Ratio", .sign_h0(x$alternative, x$odds.ratio < 1), "1")
  h1_text <- paste("Odds Ratio", .sign_h1(x$alternative, x$odds.ratio < 1), "1")
  cat(.hypotheses_ascii(h0_text, h1_text))

  if (verbose == 2) {
    cat(.topic_ascii("Key Parameters"))
    cat(sprintf("  Base Probability     = %.*f \n", digits, x$base.prob))
    cat(sprintf("  Odds Ratio           = %.*f \n", digits, x$odds.ratio))
    cat(sprintf("  Var. Corr. Factor    = %.*f \n", digits, x$vcf))
    cat(sprintf("  Mean of Alt.         = %.*f \n", digits, x$mean.alternative))
    cat(sprintf("  SD of Alt.           = %.*f \n", digits, x$sd.alternative))
    cat(sprintf("  Mean of Null         = %.*f \n", digits, x$mean.null))
    cat(sprintf("  SD of Null           = %.*f \n", digits, x$sd.null))
    cat(sprintf("  Critical Value       = %s \n\n", paste(round(x$z.alpha, digits), collapse = " and ")))
  }

  cat(.results_ascii(x, digits))

  if (verbose == 2) {
    cat(.topic_ascii("Definitions"))
    cat("  Odds Ratio = [prob/(1-prob)] / [base.prob/(1-base.prob)] \n")
    cat("  prob       : Base probability when predictor = 0 \n")
    cat("  base.prob  : Probability when predictor = 1 \n")
    cat("  beta1      = log(Odds Ratio) \n")
    cat("  beta0      = log[base.prob/(1-base.prob)] \n\n")
  }

} # .print.ascii.pwrss.logistic()




.print.ascii.pwrss.poisson <- function(x, digits = 3, verbose = 1, ...) {

  cat(.header_ascii(x$requested))
  cat(x$test, "\n\n", sep = "")
  cat("  Method          : ", x$method, "\n", sep = "")
  cat("  Predictor Dist. : ", x$dist, "\n\n", sep = "")

  h0_text <- paste("Rate Ratio", .sign_h0(x$alternative, x$rate.ratio < 1), "1")
  h1_text <- paste("Rate Ratio", .sign_h1(x$alternative, x$rate.ratio < 1), "1")
  cat(.hypotheses_ascii(h0_text, h1_text))

  if (verbose == 2) {
    cat(.topic_ascii("Key Parameters"))
    cat(sprintf("  Base Rate            = %.*f \n", digits, x$base.rate))
    cat(sprintf("  Rate Ratio           = %.*f \n", digits, x$rate.ratio))
    cat(sprintf("  Var. Corr. Factor    = %.*f\n", digits, x$vcf))
    cat(sprintf("  Mean of Alt.         = %.*f\n", digits, x$mean.alternative))
    cat(sprintf("  SD of Alt.           = %.*f\n", digits, x$sd.alternative))
    cat(sprintf("  Mean of Null         = %.*f\n", digits, x$mean.null))
    cat(sprintf("  SD of Null           = %.*f\n", digits, x$sd.null))
    cat(sprintf("  Critical Value       = %s\n\n",
                paste(round(x$z.alpha, digits), collapse = " and ")))
  }

  cat(.results_ascii(x, digits))

  if (verbose == 2) {
    cat(.topic_ascii("Definitions"))
    cat("  Base Rate  = exp(beta0) \n")
    cat("  Rate Ratio = exp(beta1) \n\n")
  }

} # .print.ascii.pwrss.poisson()



.print.ascii.pwrss.regression <- function(x, digits = 3, verbose = 1, ...) {

  cat(.header_ascii(x$requested))
  cat(x$test, "\n\n", sep = "")

  if (x$alternative %in% c("one.sided", "two.sided")) {
    h0_text <- paste("beta - null.beta", .sign_h0(x$alternative, x$ncp.alternative < x$ncp.null), ifelse(x$margin == 0, "0", "margin"))
    h1_text <- paste("beta - null.beta", .sign_h1(x$alternative, x$ncp.alternative < x$ncp.null), ifelse(x$margin == 0, "0", "margin"))
  } else if (x$alternative == "two.one.sided" && (x$ncp.alternative > min(x$ncp.null) && x$ncp.alternative < max(x$ncp.null))) {
    h0_text <- c("beta - null.beta <= min(margin) or",  "beta - null.beta >= max(margin)")
    h1_text <- c("beta - null.beta  > min(margin) and", "beta - null.beta  < max(margin)")
  } else if (x$alternative == "two.one.sided" && (x$ncp.alternative < min(x$ncp.null) || x$ncp.alternative > max(x$ncp.null))) {
    h0_text <- c("beta - null.beta >= min(margin) and", "beta - null.beta <= max(margin)")
    h1_text <- c("beta - null.beta  < min(margin) or",  "beta - null.beta  > max(margin)")
  }
  cat(.hypotheses_ascii(h0_text, h1_text))

  if (verbose == 2) {
    cat(.topic_ascii("Key Parameters"))
    cat(sprintf("  Std. Beta Under Alt.   = %.*f \n", digits, x$std.beta))
    cat(sprintf("  Std. Beta Under Null   = %.*f \n", digits, x$std.null.beta))
    cat(sprintf("  Std. Margin            = %s \n", paste(round(x$std.margin, digits), collapse = " and ")))
    cat(sprintf("  Degrees of Freedom     = %.*f \n", 0, x$df))
    cat(sprintf("  Non-centrality of Alt. = %.*f\n", digits, x$ncp.alternative))
    cat(sprintf("  Non-centrality of Null = %s \n", paste(round(x$ncp.null, digits), collapse = " and ")))
    cat(sprintf("  Critical Value         = %s \n\n", paste(round(x$t.alpha, digits), collapse = " and ")))
  }

  cat(.results_ascii(x, digits))

  if (verbose == 2) {
    cat(.topic_ascii("Definitions"))
    cat("  beta                 : Regression coefficient under alt. \n")
    cat("  null.beta            : Regression coefficient under null \n")
    cat("  margin               : Smallest beta - null.beta difference that matters \n\n")
    cat("  Std. Beta Under Alt. = beta * [SD(X) / SD(Y)]\n")
    cat("  Std. Beta Under Null = null.beta * [SD(X) / SD(Y)]\n")
    cat("  Std. Margin          = margin * [SD(X) / SD(Y)] \n\n")
    cat("  SD(X)                : Standard deviation of the predictor \n")
    cat("  SD(Y)                : Standard deviation of the outcome \n\n")
  }

} # .print.ascii.pwrss.regression()


.print.ascii.pwrss.f.regression <- function(x, digits = 3, verbose = 1, ...) {

  cat(.header_ascii(x$requested))
  cat(x$test, "\n\n", sep = "")

  rsq_text <- ifelse(x$k.tested < x$k.total, "Change in R-squared", "R-squared")
  h0_text <- ifelse(x$margin == 0, sprintf("%s = 0", rsq_text), sprintf("0 <= %s <= margin", rsq_text))
  h1_text <- ifelse(x$margin == 0, sprintf("%s > 0", rsq_text), sprintf("%s > margin",       rsq_text))
  cat(.hypotheses_ascii(h0_text, h1_text))

  if (verbose == 2) {
    cat(.topic_ascii("Key Parameters"))
    if (x$k.tested < x$k.total) {
      cat(sprintf("  Change in R-squared    = %.*f \n", digits, x$r.squared.change))
    } else {
      cat(sprintf("  R-squared              = %.*f \n", digits, x$r.squared.change))
    }
    cat(sprintf("  Margin                 = %.*f \n", digits, x$margin))
    cat(sprintf("  Num. Deg. of Freedom   = %.*f\n", 0, x$df1))
    cat(sprintf("  Denom. Deg. of Freedom = %.*f\n", 0, x$df2))
    cat(sprintf("  Non-centrality of Alt. = %.*f \n", digits, x$ncp.alternative))
    cat(sprintf("  Non-centrality of Null = %.*f \n", digits, x$ncp.null))
    cat(sprintf("  Critical Value         = %s \n\n", paste(round(x$f.alpha, digits), collapse = " and ")))
  }

  cat(.results_ascii(x, digits))

  if (verbose == 2) {
    cat(.topic_ascii("Definitions"))
    if (x$k.tested < x$k.total) {
      cat("  Margin : Smallest change in R-squared that matters \n\n")
    } else {
      cat("  Margin : Smallest R-squared that matters \n\n")
    }
  }

} # .print.ascii.pwrss.f.regression()



.print.ascii.pwrss.med <- function(x, digits = 3, verbose = 1, ...) {

  cat(.header_ascii(x$requested))
  method <- switch(x$method, `sobel` = "Sobel", `aroian` = "Aroian", `goodman` = "Goodman", `joint` = "Joint", `monte.carlo` = "Monte Carlo")
  cat(x$test, "\n\n", sep = "")
  cat("  Method            : ", method, "\n\n", sep = "")

  h0_text <- paste("beta[a*b]", .sign_h0(x$alternative, x$std.beta.indirect < 0), "0")
  h1_text <- paste("beta[a*b]", .sign_h1(x$alternative, x$std.beta.indirect < 0), "0")
  cat(.hypotheses_ascii(h0_text, h1_text))

  if (verbose == 2) {
    cat(.topic_ascii("Key Parameters"))
    cat(sprintf("  Std. beta[a]         = %.*f \n", digits, x$std.beta.a))
    cat(sprintf("  Std. beta[b]         = %.*f \n", digits, x$std.beta.b))
    if (x$method %in% c("sobel", "aorian", "goodman")) {
      cat(sprintf("  Std. beta[a*b]       = %.*f \n", digits, x$std.beta.indirect))
      cat(sprintf("  Mean of Alt.         = %.*f\n", digits, x$mean.alternative))
      cat(sprintf("  Mean of Null.        = %s \n", paste(round(x$mean.null, digits), collapse = " and ")))
      cat(sprintf("  Critical Value       = %s \n\n", paste(round(x$z.alpha, digits), collapse = " and ")))
    } else {
      cat(sprintf("  Std. beta[a*b]       = %.*f \n\n", digits, x$std.beta.indirect))
    }
  }

  cat(.results_ascii(x, digits))

  if (verbose == 2) {
    cat(.topic_ascii("Definitions"))
    cat("  beta[a]        : Regression coefficient for path a\n")
    cat("  beta[b]        : Regression coefficient for path b\n")
    cat("  beta[a*b]      : Coefficient for the indirect path a*b\n\n")
    cat("  Std. beta[a]   = beta[a] * [SD(predictor) / SD(mediator)]\n")
    cat("  Std. beta[b]   = beta[b] * [SD(mediator) / SD(outcome)]\n")
    cat("  Std. beta[a*b] = Std. beta[a] * Std. beta[b]\n\n")
    cat("  SD(predictor)  : Standard deviation of the predictor\n")
    cat("  SD(mediator)   : Standard deviation of the mediator\n")
    cat("  SD(outcome)    : Standard deviation of the outcome\n\n")
  }

} # .print.ascii.pwrss.med()



.print.ascii.pwrss.student <- function(x, digits = 3, verbose = 1, ...) {

  cat(.header_ascii(x$requested))
  cat(x$test, "\n\n", sep = "")

  if (x$alternative %in% c("one.sided", "two.sided")) {
    h0_text <- paste("d - null.d", .sign_h0(x$alternative, x$ncp.alternative < x$ncp.null), ifelse(x$margin == 0, "0", "margin"))
    h1_text <- paste("d - null.d", .sign_h1(x$alternative, x$ncp.alternative < x$ncp.null), ifelse(x$margin == 0, "0", "margin"))
  } else if (x$alternative == "two.one.sided" && (x$ncp.alternative > min(x$ncp.null) && x$ncp.alternative < max(x$ncp.null))) {
    h0_text <- c("d - null.d <= min(margin) or",  "d - null.d >= max(margin)")
    h1_text <- c("d - null.d  > min(margin) and", "d - null.d  < max(margin)")
  } else if (x$alternative == "two.one.sided" && (x$ncp.alternative < min(x$ncp.null) || x$ncp.alternative > max(x$ncp.null))) {
    h0_text <- c("d - null.d >= min(margin) and", "d - null.d <= max(margin)")
    h1_text <- c("d - null.d  < min(margin) or",  "d - null.d  > max(margin)")
  }
  cat(.hypotheses_ascii(h0_text, h1_text))

  if (verbose == 2) {
    cat(.topic_ascii("Key Parameters"))
    cat(sprintf("  Cohen's d              = %.*f\n", digits, x$d))
    cat(sprintf("  Cohen's d Under Null   = %.*f\n", digits, x$null.d))
    cat(sprintf("  Margin                 = %s\n", paste(round(x$margin, digits), collapse = " and ")))
    cat(sprintf("  Degrees of Freedom     = %.*f\n", 0, x$df))
    cat(sprintf("  Non-centrality of Alt. = %.*f\n", digits, x$ncp.alternative))
    cat(sprintf("  Non-centrality of Null = %s\n", paste(round(x$ncp.null, digits), collapse = " and ")))
    cat(sprintf("  Critical Value         = %s \n\n", paste(round(x$t.alpha, digits), collapse = " and ")))
  }

  cat(.results_ascii(x, digits))

  if (verbose == 2) {
    cat(.topic_ascii("Definitions"))
    cat("  Margin : Smallest d - null.d difference that matters \n\n")
  }

} # .print.ascii.pwrss.student()



.print.ascii.pwrss.wilcoxon <- function(x, digits = 3, verbose = 1, ...) {

  cat(.header_ascii(x$requested))
  dist <- switch(x$dist,
                 `normal` = "Normal",
                 `uniform` = "Uniform",
                 `double.exponential` = "Double Exponential",
                 `laplace` = "Laplace",
                 `logistic` = "Logistic")
  method <- switch(x$method,
                   `guenther` = "Guenther",
                   `noether` = "Noether")
  cat(x$test, "\n\n", sep = "")
  cat("  Method       : ", method, "\n", sep = "")
  cat("  Distribution : ", dist, "\n\n", sep = "")

  is.less <- ifelse(x$method == "guenther", x$ncp < x$null.ncp, x$mean < x$null.mean)
  is.eqvl <- ifelse(x$method == "guenther", x$ncp > min(x$null.ncp) && x$ncp < max(x$null.ncp),
                                            x$mean > min(x$null.mean) && x$mean < max(x$null.mean))
  if (x$alternative %in% c("one.sided", "two.sided")) {
    h0_text <- paste("d - null.d", .sign_h0(x$alternative, is.less), ifelse(x$margin == 0, "0", "margin"))
    h1_text <- paste("d - null.d", .sign_h1(x$alternative, is.less), ifelse(x$margin == 0, "0", "margin"))
  } else if (x$alternative == "two.one.sided" &&  is.eqvl) {
    h0_text <- c("d - null.d <= min(margin) or",  "d - null.d >= max(margin)")
    h1_text <- c("d - null.d  > min(margin) and", "d - null.d  < max(margin)")
  } else if (x$alternative == "two.one.sided" && !is.eqvl) {
    h0_text <- c("d - null.d >= min(margin) and", "d - null.d <= max(margin)")
    h1_text <- c("d - null.d  < min(margin) or",  "d - null.d  > max(margin)")
  }
  cat(.hypotheses_ascii(h0_text, h1_text))

  if (verbose == 2) {
    cat(.topic_ascii("Key Parameters"))
    cat(sprintf("  Cohen's d              = %.*f\n", digits, x$d))
    cat(sprintf("  Cohen's d Under Null   = %.*f\n", digits, x$null.d))
    cat(sprintf("  Margin                 = %s\n", paste(round(x$margin, digits), collapse = " and ")))
    if (x$method == "guenther") {
      cat(sprintf("  Degrees of Freedom     = %.*f\n", 0, x$df))
      cat(sprintf("  Non-centrality of Alt. = %.*f\n", digits, x$ncp))
      cat(sprintf("  Non-centrality of Null = %s\n", paste(round(x$null.ncp, digits), collapse = " and ")))
      cat(sprintf("  Critical Value         = %s\n\n", paste(round(x$t.alpha, digits), collapse = " and ")))
    } else {
      cat(sprintf("  Mean of Alt.           = %.*f\n", digits, x$mean))
      cat(sprintf("  Mean of Null           = %s\n", paste(round(x$null.mean, digits), collapse = " and ")))
      cat(sprintf("  Critical Value         = %s\n\n", paste(round(x$z.alpha, digits), collapse = " and ")))
    }
  }

  cat(.results_ascii(x, digits))

  if (verbose == 2) {
    cat(.topic_ascii("Definitions"))
    cat("  Margin : Smallest d - null.d difference that matters \n\n")
  }

} # .print.ascii.pwrss.wilcoxon()



.print.ascii.pwrss.gof <- function(x, digits = 3, verbose = 1, ...) {

  cat(.header_ascii(x$requested))
  cat(x$test, "\n\n", sep = "")

  cat(.hypotheses_ascii("P[i,j]  = P0[i,j] for all (i,j)", "P[i,j] != P0[i,j] for some (i,j)"))

  if (verbose == 2) {
    cat(.topic_ascii("Key Parameters"))
    cat(sprintf("  Degrees of Freedom     = %.*f \n", 0, x$df))
    cat(sprintf("  Non-centrality of Alt. = %.*f \n", digits, x$ncp.alternative))
    cat(sprintf("  Non-centrality of Null = %.*f \n", digits, x$ncp.null))
    cat(sprintf("  Critical Value         = %s\n\n",
                paste(round(x$chisq.alpha, digits), collapse = " and ")))
  }

  cat(.results_ascii(x, digits))

  if (verbose == 2) {
    cat(.topic_ascii("Definitions"))
    cat("  For goodness-of-fit, comparisons are P[i] vs P0[i] \n")
    cat("  For independence, comparisons are P[i,j] vs P0[i,j] \n")
    cat("  Independence implies (default) P0[i,j] = P[i,.] * P[.,j] \n\n")
    cat("  P[i,j] : Joint probability for cell (i,j) \n")
    cat("  P[i,.] : Marginal probability for row i (sum over j) \n")
    cat("  P[.,j] : Marginal probability for column j (sum over i) \n\n")
  }

} # .print.ascii.pwrss.gof()



.print.ascii.pwrss.chisq <- function(x, digits = 3, verbose = 1, ...) {

  cat(.header_ascii(x$requested))
  cat(x$test, "\n\n", sep = "")

  cat(.hypotheses_ascii(ifelse(x$ncp.null == 0, "ncp = 0", "0 <= ncp <= null.ncp"),
                        ifelse(x$ncp.null == 0, "ncp > 0", "ncp > null.ncp")))

  if (verbose == 2) {
    cat(.topic_ascii("Key Parameters"))
    cat(sprintf("  Degrees of Freedom     = %.*f\n", 0, x$df))
    cat(sprintf("  Non-centrality of Alt. = %.*f\n", digits, x$ncp.alternative))
    cat(sprintf("  Non-centrality of Null = %.*f\n", digits, x$ncp.null))
    cat(sprintf("  Critical Value         = %s\n\n",
                paste(round(x$chisq.alpha, digits), collapse = " and ")))
  }

  cat(.results_ascii(x, digits))

  if (verbose == 2) {
    cat(.topic_ascii("Definitions"))
    cat("  ncp      : Non-centrality parameter of alt. \n")
    cat("  null.ncp : Non-centrality parameter of null \n\n")
  }

} # .print.ascii.pwrss.chisq()



.print.ascii.pwrss.t <- function(x, digits = 3, verbose = 1, ...) {

  cat(.header_ascii(x$requested))
  cat(x$test, "\n\n", sep = "")

  if (x$alternative %in% c("one.sided", "two.sided")) {
    h0_text <- paste("ncp", .sign_h0(x$alternative, x$ncp.alternative < x$ncp.null), "null.ncp")
    h1_text <- paste("ncp", .sign_h1(x$alternative, x$ncp.alternative < x$ncp.null), "null.ncp")
  } else if (x$alternative == "two.one.sided" && (x$ncp.alternative > min(x$ncp.null) && x$ncp.alternative < max(x$ncp.null))) {
    h0_text <- c("ncp <= min(null.ncp) or",  "ncp >= max(null.ncp)")
    h1_text <- c("ncp  > min(null.ncp) and", "ncp  < max(null.ncp)")
  } else if (x$alternative == "two.one.sided" && (x$ncp.alternative < min(x$ncp.null) || x$ncp.alternative > max(x$ncp.null))) {
    h0_text <- c("ncp >= min(null.ncp) and", "ncp <= max(null.ncp)")
    h1_text <- c("ncp  < min(null.ncp) or",  "ncp  > max(null.ncp)")
  }
  cat(.hypotheses_ascii(h0_text, h1_text))

  if (verbose == 2) {
    cat(.topic_ascii("Key Parameters"))
    if (isFALSE(is.infinite(x$df))) {
      cat(sprintf("  Degrees of Freedom     = %.*f\n", 0, x$df))
    }
    cat(sprintf("  Non-centrality of Alt. = %.*f\n", digits, x$ncp.alternative))
    cat(sprintf("  Non-centrality of Null = %s\n", paste(round(x$ncp.null, digits), collapse = " and ")))
    cat(sprintf("  Critical Value         = %s\n\n", paste(round(x$t.alpha, digits), collapse = " and ")))
  }

  cat(.results_ascii(x, digits))

  if (verbose == 2) {
    cat(.topic_ascii("Definitions"))
    cat("  ncp      : Non-centrality parameter of Alt. \n")
    cat("  null.ncp : Non-centrality parameter of Null \n\n")
  }

} # .print.ascii.pwrss.t()



.print.ascii.pwrss.z <- function(x, digits = 3, verbose = 1, ...) {

  cat(.header_ascii(x$requested))
  cat(x$test, "\n\n", sep = "")

  if (x$alternative %in% c("one.sided", "two.sided")) {
    h0_text <- paste("mean", .sign_h0(x$alternative, x$mean.alternative < x$mean.null), "null.mean")
    h1_text <- paste("mean", .sign_h1(x$alternative, x$mean.alternative < x$mean.null), "null.mean")
  } else if (x$alternative == "two.one.sided" && (x$mean.alternative > min(x$mean.null) && x$mean.alternative < max(x$mean.null))) {
    h0_text <- c("mean <= min(null.mean) or",  "mean >= max(null.mean)")
    h1_text <- c("mean  > min(null.mean) and", "mean  < max(null.mean)")
  } else if (x$alternative == "two.one.sided" && (x$mean.alternative < min(x$mean.null) || x$mean.alternative > max(x$mean.null))) {
    h0_text <- c("mean >= min(null.mean) and", "mean <= max(null.mean)")
    h1_text <- c("mean  < min(null.mean) or",  "mean  > max(null.mean)")
  }
  cat(.hypotheses_ascii(h0_text, h1_text))

  if (verbose == 2) {
    cat(.topic_ascii("Key Parameters"))
    cat(sprintf("  Mean of Alt.         = %.*f\n", digits, x$mean.alternative))
    cat(sprintf("  Mean of Null         = %s\n", paste(round(x$mean.null, digits), collapse = " and ")))
    cat(sprintf("  Critical Value       = %s\n\n", paste(round(x$z.alpha, digits), collapse = " and ")))
  }

  cat(.results_ascii(x, digits))

  if (verbose == 2) {
    cat(.topic_ascii("Definitions"))
    cat("  mean      : Mean of alt. \n")
    cat("  null.mean : Mean of null \n\n")
  }

} # .print.ascii.pwrss.z()



.print.ascii.pwrss.binom <- function(x, digits = 3, verbose = 1, ...) {

  cat(.header_ascii(x$requested))
  cat(x$test, "\n\n", sep = "")

  if (x$alternative %in% c("one.sided", "two.sided")) {
    h0_text <- paste("prob", .sign_h0(x$alternative, x$prob.alt < x$prob.null), "null.prob")
    h1_text <- paste("prob", .sign_h1(x$alternative, x$prob.alt < x$prob.null), "null.prob")
  } else if (x$alternative == "two.one.sided" && (x$prob.alt > min(x$prob.null) && x$prob.alt < max(x$prob.null))) {
    h0_text <- c("prob <= min(null.prob) or",  "prob >= max(null.prob)")
    h1_text <- c("prob  > min(null.prob) and", "prob  < max(null.prob)")
  } else if (x$alternative == "two.one.sided" && (x$prob.alt < min(x$prob.null) || x$prob.alt > max(x$prob.null))) {
    h0_text <- c("prob >= min(null.prob) and", "prob <= max(null.prob)")
    h1_text <- c("prob  < min(null.prob) or",  "prob  > max(null.prob)")
  }
  cat(.hypotheses_ascii(h0_text, h1_text))

  if (verbose == 2) {
    cat(.topic_ascii("Key Parameters"))
    cat(sprintf("  Size                   = %.*f\n", 0, x$size))
    cat(sprintf("  Probability Under Alt. = %.*f\n", digits, x$prob.alt))
    cat(sprintf("  Probability Under Null = %s\n", paste(round(x$prob.null, digits), collapse = " and ")))
    cat(sprintf("  Critical Value         = %s\n\n", paste(round(x$binom.alpha, 0), collapse = " and ")))
  }

  cat(.results_ascii(x, digits))

  if (verbose == 2) {
    cat(.topic_ascii("Definitions"))
    cat("  prob      : Probability under alt. \n")
    cat("  null.prob : Probability under null \n\n")
  }

} # .print.ascii.pwrss.binom()



.print.ascii.pwrss.f <- function(x, digits = 3, verbose = 1, ...) {

  cat(.header_ascii(x$requested))
  cat(x$test, "\n\n", sep = "")

  cat(.hypotheses_ascii(ifelse(x$ncp.null == 0, "ncp = 0", "0 <= ncp <= null.ncp"),
                        ifelse(x$ncp.null == 0, "ncp > 0", "ncp > null.ncp")))

  if (verbose == 2) {
    cat(.topic_ascii("Key Parameters"))
    cat(sprintf("  Num. Deg. of Freedom   = %.*f \n", 0, x$df1))
    cat(sprintf("  Denom. Deg. of Freedom = %.*f \n", 0, x$df2))
    cat(sprintf("  Non-centrality of Alt. = %.*f \n", digits, x$ncp.alternative))
    cat(sprintf("  Non-centrality of Null = %.*f \n", digits, x$ncp.null))
    cat(sprintf("  Critical Value         = %s \n\n", paste(round(x$f.alpha, digits), collapse = " and ")))
  }

  cat(.results_ascii(x, digits))

  if (verbose == 2) {
    cat(.topic_ascii("Definitions"))
    cat("  ncp      : Non-centrality parameter of alt. \n")
    cat("  null.ncp : Non-centrality parameter of null \n\n")
  }

} # .print.ascii.pwrss.f()




.print.ascii.pwrss.ancova <- function(x, digits = 3, verbose = 1, ...) {

  cat(.header_ascii(x$requested))
  cat(x$test, "\n\n", sep = "")

  cat(.hypotheses_ascii(ifelse(x$null.ncp == 0, "eta.squared = 0", "0 <= eta.squared <= null.eta.squared"),
                        ifelse(x$null.ncp == 0, "eta.squared > 0", "eta.squared > eta.squared")))

  if (verbose == 2) {
    cat(.topic_ascii("Key Parameters"))
    cat(sprintf("  Design                 = %s \n", x$effect))
    cat(sprintf("  Num. Deg. of Freedom   = %.*f \n", 0, x$df1))
    cat(sprintf("  Denom. Deg. of Freedom = %.*f \n", 0, x$df2))
    cat(sprintf("  Non-centrality of Alt. = %.*f \n", digits, x$ncp))
    cat(sprintf("  Non-centrality of Null = %.*f \n", digits, x$null.ncp))
    cat(sprintf("  Critical Value         = %s \n\n", paste(round(x$f.alpha, digits), collapse = " and ")))
  }

  cat(.results_ascii(x, digits))

  if (verbose == 2) {
    cat(.topic_ascii("Definitions"))
    if (x$null.ncp != 0) cat("  null.eta.squared : (Partial) Eta-squared under null \n")
    cat("  eta.squared      : (Partial) Eta-squared under alt. \n\n")
  }

} # .print.ascii.pwrss.ancova()




.print.ascii.pwrss.contrast <- function(x, digits = 3, verbose = 1, ...) {

  cat(.header_ascii(x$requested))
  cat(x$test, "\n\n", sep = "")

  cat(.hypotheses_ascii("psi  = 0", "psi != 0"))

  if (verbose == 2) {
    cat(.topic_ascii("Key Parameters"))
    cat(sprintf("  Contrast Est. (psi)    = %.*f \n", digits, x$psi))
    cat(sprintf("  Standardized psi (d)   = %.*f \n", digits, x$d))
    cat(sprintf("  Degrees of Freedom     = %.*f \n", 0, x$df))
    cat(sprintf("  Non-centrality of Alt. = %.*f \n", digits, x$ncp))
    cat(sprintf("  Non-centrality of Null = %.*f \n", digits, x$null.ncp))
    cat(sprintf("  Critical Value         = %s \n\n", paste(round(x$t.alpha, digits), collapse = " and ")))
  }

  cat(.results_ascii(x, digits))

  if (verbose == 2) {
    cat(.topic_ascii("Definitions"))
    cat("  psi : Contrast estimate, sum(contrast[i] * mu[i]) \n")
    cat("  d   : Standardized contrast estimate \n\n")
  }

} # .print.ascii.pwrss.contrast()



.print.ascii.pwrss.contrasts <- function(x, data = NULL, digits = 3, verbose = 1, ...) {

  cat(.header_ascii(x$requested))
  cat(x$test, "\n\n", sep = "")

  cat(.hypotheses_ascii("psi  = 0", "psi != 0"))

  if (verbose == 2) {
    cat(.topic_ascii("Key Parameters"))
    adjust.alpha <- switch(x$adjust.alpha,
                           `none` = "None",
                           `fdr` = "False Discovery Rate",
                           `hochberg` = "Hochberg (1988)",
                           `BH` = "Benjamini & Hochberg (1995)",
                           `BY` = "Benjamini & Yakutieli (2001)",
                           `holm` = "Holm (1979)",
                           `hommel` = "Hommel (1988)",
                           `bonferroni` = "Bonferroni",
                           `tukey` = "Tukey")
    cat(sprintf("  Alpha Adjustment       = %s \n", adjust.alpha))
    cat(sprintf("  Adjusted Alpha         = %.*f \n", digits, x$alpha))
    cat(sprintf("  Non-centrality of Null = %.*f \n\n", digits, x$null.ncp))
  }

  cat(.topic_ascii("Results"))
  if (!is.null(x$data)) {
    print(x$data, row.names = FALSE)
    cat("\n")
  }

  if (verbose == 2) {
    cat(.topic_ascii("Definitions"))
    cat("  psi : Contrast estimate, sum(contrast[i] * mu[i])\n")
    cat("  d   : Standardized contrast estimate \n")
    cat("  ncp : Non-centrality parameter under alt. \n\n")
  }

} # .print.ascii.pwrss.contrasts()




.print.ascii.pwrss.fisher <- function(x, digits = 3, verbose = 1, ...) {

  cat(.header_ascii(x$requested))
  method <- switch(x$method,
                   `z` = "Normal Approximation",
                   `exact` = "Fisher's Exact")
  cat(x$test, "\n\n", sep = "")
  cat("  Method          : ", method, "\n\n", sep = "")

  if (x$alternative %in% c("one.sided", "two.sided")) {
    h0_text <- paste("prob1 - prob2", .sign_h0(x$alternative, x$delta < x$margin), ifelse(x$margin == 0, "0", "margin"))
    h1_text <- paste("prob1 - prob2", .sign_h1(x$alternative, x$delta < x$margin), ifelse(x$margin == 0, "0", "margin"))
  } else if (x$alternative == "two.one.sided" && (x$delta > min(x$margin) && x$delta < max(x$ncp.null))) {
    h0_text <- c("prob1 - prob2 <= min(margin) or",  "prob1 - prob2 >= max(margin)")
    h1_text <- c("prob1 - prob2  > min(margin) and", "prob1 - prob2  < max(margin)")
  } else if (x$alternative == "two.one.sided" && (x$delta < min(x$margin) || x$delta > max(x$x$margin))) {
    h0_text <- c("prob1 - prob2 >= min(margin) and", "prob1 - prob2 <= max(margin)")
    h1_text <- c("prob1 - prob2  < min(margin) or",  "prob1 - prob2  > max(margin)")
  }
  cat(.hypotheses_ascii(h0_text, h1_text))

  if (verbose == 2) {
    cat(.topic_ascii("Key Parameters"))
    cat(sprintf("  prob1 - prob2        = %.*f\n", digits, x$delta))
    cat(sprintf("  Odds Ratio           = %.*f\n", digits, x$odds.ratio))
    if (x$method == "z") {
      cat(sprintf("  Mean of Alt.         = %.*f\n", digits, x$mean.alternative))
      cat(sprintf("  Mean of Null         = %s\n", paste(round(x$mean.null, digits), collapse = " and ")))
      cat(sprintf("  Critical Value       = %s\n\n", paste(round(x$z.alpha, digits), collapse = " and ")))
    } else {
      cat("\n")
    }
  }

  cat(.results_ascii(x, digits))

  if (verbose == 2) {
    cat(.topic_ascii("Definitions"))
    cat("  prob1       : Probability of success in the first group \n")
    cat("  prob2       : Probability of success in the second group \n")
    if (any(x$margin != 0))
      cat("  margin      : Smallest prob1 - prob2 that matters \n")
    cat("  Odds Ratio  : Odds(prob1) / Odds(prob2) \n")
    cat("  Odds(prob1) : prob1 / (1 - prob1) \n")
    cat("  Odds(prob2) : prob2 / (1 - prob2) \n\n")
  }

} # .print.ascii.pwrss.fisher()




.print.ascii.pwrss.mcnemar <- function(x, digits = 3, verbose = 1, ...) {

  cat(.header_ascii(x$requested))
  method <- switch(x$method,
                   `z` = "Normal Approximation",
                   `exact` = "McNemar's Exact")
  cat(x$test, "\n\n", sep = "")
  cat("  Method          : ", method, "\n\n", sep = "")

  h0_text <- paste("prob10 - prob01", .sign_h0(x$alternative, x$delta < 0), "0")
  h1_text <- paste("prob10 - prob01", .sign_h1(x$alternative, x$delta < 0), "0")
  cat(.hypotheses_ascii(h0_text, h1_text))

  if (verbose == 2) {
    cat(.topic_ascii("Key Parameters"))
    cat(sprintf("  Odds Ratio           = %.*f\n", digits, x$odds.ratio))
    cat(sprintf("  prob10 - prob01      = %.*f\n", digits, x$delta))
    if (x$method == "exact") {
      cat(sprintf("  Size of Disc. Pairs  = %.*f \n", 0, x$size))
      cat(sprintf("  prob10 || DP for Alt. = %.*f \n", digits, x$prob.alternative))
      cat(sprintf("  prob10 || DP for Null = %s \n", paste(round(x$prob.null, digits), collapse = " and ")))
      cat(sprintf("  Critical Value       = %s \n\n", paste(round(x$binom.alpha, digits), collapse = " and ")))
    } else {
      cat(sprintf("  Mean of Alt.        = %.*f\n", digits, x$mean.alternative))
      cat(sprintf("  Mean of Null        = %s\n", paste(round(x$mean.null, digits), collapse = " and ")))
      cat(sprintf("  Critical Value      = %s\n\n", paste(round(x$z.alpha, digits), collapse = " and ")))
    }
  }

  cat(.results_ascii(x, digits))

  if (verbose == 2) {
    cat(.topic_ascii("Definitions"))
    cat("  prob10      : Joint prob. of observing {1,0} \n")
    cat("  prob01      : Joint prob. of observing {0,1} \n")
    cat("  Odds Ratio  : prob10 / prob01 \n")
    cat("  prob10 | DP : Conditional prob. of observing {1,0} \n                among DP, prob10 / (prob10 + prob01) \n")
    cat("  DP          : Discordant pairs \n\n")
  }

} # .print.ascii.pwrss.mcnemar()


.print.ascii.pwrss.oneprop <- function(x, digits = 3, verbose = 1, ...) {

  cat(.header_ascii(x$requested))
  method <- switch(x$method,
                   `z` = "Normal Approximation",
                   `exact` = "Exact")
  cat(x$test, "\n\n", sep = "")
  if (x$method == "exact") {
    cat("  Method                 : ", method, "\n\n", sep = "")
  } else {
    stderr <- switch(x$std.err,
                     `alternative` = "Alternative",
                     `null` = "Null")
    cat("  Method                 : ", method, "\n", sep = "")
    cat("  Continuity Correction  : ", x$correct, "\n", sep = "")
    cat("  Arcsine Transformation : ", x$arcsine, "\n", sep = "")
    cat("  Standard Error         : Calculated From ", stderr, "\n\n", sep = "")
  }

  if (x$alternative %in% c("one.sided", "two.sided")) {
    h0_text <- paste("prob - null.prob", .sign_h0(x$alternative, x$delta < 0), "0")
    h1_text <- paste("prob - null.prob", .sign_h1(x$alternative, x$delta < 0), "0")
  } else if (x$alternative == "two.one.sided" && (x$delta[1] > 0 && x$delta[2] < 0)) {
    h0_text <- c("prob - min(null.prob) <= 0 or",  "prob - max(null.prob) >= 0")
    h1_text <- c("prob - min(null.prob)  > 0 and", "prob - max(null.prob)  < 0")
  } else if (x$alternative == "two.one.sided" && (x$delta[1] < 0 || x$delta[2] > 0)) {
    h0_text <- c("prob - min(null.prob) >= 0 and", "prob - max(null.prob) <= 0")
    h1_text <- c("prob - min(null.prob)  < 0 or",  "prob - max(null.prob)  > 0")
  }
  cat(.hypotheses_ascii(h0_text, h1_text))

  if (verbose == 2) {
    cat(.topic_ascii("Key Parameters"))
    delta.text <- paste(round(x$delta, digits), collapse = " and ")
    cat(sprintf("  prob - null.prob      = %s\n", delta.text))
    or.text <- paste(round(x$odds.ratio, digits), collapse = " and ")
    cat(sprintf("  Odds Ratio            = %s\n", or.text))
    if (x$method == "exact") {
      cat(sprintf("  Size                  = %.*f\n", 0, x$size))
      cat(sprintf("  Prob. Under Alt       = %.*f\n", digits, x$prob.alternative))
      cat(sprintf("  Prob. Under Null      = %s\n", paste(round(x$prob.null, digits), collapse = " and ")))
      cat(sprintf("  Critical Value        = %s\n\n", paste(round(x$binom.alpha, digits), collapse = " and ")))
    } else {
      cat(sprintf("  Mean of Alt.          = %.*f\n", digits, x$mean.alternative))
      cat(sprintf("  Mean of Null          = %s\n", paste(round(x$mean.null, digits), collapse = " and ")))
      cat(sprintf("  Critical Value        = %s\n\n", paste(round(x$z.alpha, digits), collapse = " and ")))
    }
  }

  cat(.results_ascii(x, digits))

  if (verbose == 2) {
    cat(.topic_ascii("Definitions"))
    cat("  Odds Ratio      : Odds(prob) / Odds(null.prob) \n")
    cat("  Odds(prob)      : prob / (1 - prob) \n")
    cat("  Odds(null.prob) : null.prob / (1 - null.prob) \n\n")
  }

} # .print.ascii.pwrss.oneprop()




.print.ascii.pwrss.steiger <- function(x, digits = 3, verbose = 1, ...) {

  cat(.header_ascii(x$requested))
  cat(x$test, "\n\n", sep = "")
  cat("  Common Index    : ", x$common, "\n\n", sep = "")

  h0_text <- paste("rho12 -", ifelse(x$common, "rho13", "rho34"), .sign_h0(x$alternative, x$delta < 0), "0")
  h1_text <- paste("rho12 -", ifelse(x$common, "rho13", "rho34"), .sign_h1(x$alternative, x$delta < 0), "0")
  cat(.hypotheses_ascii(h0_text, h1_text))

  if (verbose == 2) {
    cat(.topic_ascii("Key Parameters"))
    if (x$common) {
      cat(sprintf("  rho12 - rho13        = %.*f\n", digits, x$delta))
    } else {
      cat(sprintf("  rho12 - rho34        = %.*f\n", digits, x$delta))
    }
    cat(sprintf("  Cohen's q            = %.*f\n", digits, x$q))
    cat(sprintf("  Mean of Alt.         = %.*f\n", digits, x$mean.alternative))
    cat(sprintf("  SD of Alt.           = %.*f\n", digits, x$sd.alternative))
    cat(sprintf("  Mean of Null         = %.*f\n", digits, x$mean.null))
    cat(sprintf("  SD of Null           = %.*f\n", digits, x$sd.null))
    cat(sprintf("  Critical Value       = %s\n\n", paste(round(x$z.alpha, digits), collapse = " and ")))
  }

  cat(.results_ascii(x, digits))

  if (verbose == 2) {
    cat(.topic_ascii("Definitions"))
    if (x$common) {
      cat("  rho12 : Correlation between variable V1 and V2 \n")
      cat("  rho13 : Correlation between variable V1 and V3 \n\n")
    } else {
      cat("  rho12 : Correlation between variable V1 and V2 \n")
      cat("  rho34 : Correlation between variable V3 and V4 \n\n")
    }
  }

} # .print.ascii.pwrss.steiger()




.print.ascii.pwrss.twocors <- function(x, digits = 3, verbose = 1, ...) {

  cat(.header_ascii(x$requested))
  cat(x$test, "\n\n")
  
  h0_text <- paste(ifelse(x$design == "one.sample", "rho - null.rho", "rho1 - rho2"), .sign_h0(x$alternative, x$delta < 0), "0")
  h1_text <- paste(ifelse(x$design == "one.sample", "rho - null.rho", "rho1 - rho2"), .sign_h1(x$alternative, x$delta < 0), "0")
  cat(.hypotheses_ascii(h0_text, h1_text))

  if (verbose == 2) {
    cat(.topic_ascii("Key Parameters"))
    if (x$design %in% c("independent", "paired")) {
      cat(sprintf("  rho1 - rho2          = %.*f\n", digits, x$delta))
    } else {
      cat(sprintf("  rho - null.rho       = %.*f\n", digits, x$delta))
    }
    cat(sprintf("  Cohen's q            = %.*f\n", digits, x$q))
    cat(sprintf("  Mean of Alt.         = %.*f\n", digits, x$mean.alternative))
    cat(sprintf("  SD of Alt.           = %.*f\n", digits, x$sd.alternative))
    cat(sprintf("  Mean of Null         = %.*f\n", digits, x$mean.null))
    cat(sprintf("  SD of Null           = %.*f\n", digits, x$sd.null))
    cat(sprintf("  Critical Value       = %s\n\n", paste(round(x$z.alpha, digits), collapse = " and ")))
  }

  cat(.results_ascii(x, digits))

  if (verbose == 2) {
    cat(.topic_ascii("Definitions"))
    if (x$design %in% c("independent", "paired")) {
      cat("  rho1 : Correlation in group 1 \n")
      cat("  rho2 : Correlation in group 2 \n\n")
    } else {
      cat("  rho      : Correlation under alt. \n")
      cat("  null.rho : Correlation under null \n\n")
    }
  }

} # .print.ascii.pwrss.twocors()
