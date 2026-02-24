#' Conversion from R-squared to Cohen's f
#'
#' @description
#' Helper function to convert between Cohen's f and R-squared.
#'
#' @param r.squared.full    R-squared for the full model.
#' @param r.squared.reduced R-squared for the reduced model.
#' @param verbose           \code{1} by default (returns results), if \code{0}
#'                          no output is printed on the console.
#'
#' @return
#'   \item{f.squared}{Cohen's f-squared.}
#'   \item{f}{Cohen's f.}
#'   \item{r.squared.full}{R-squared for the full model.}
#'   \item{r.squared.reduced}{R-squared for the reduced model.}
#'
#' @references
#'   Cohen, J. (1988). Statistical power analysis for the behavioral sciences
#'   (2nd ed.). Lawrence Erlbaum Associates.
#'
#'   Selya, A. S., Rose, J. S., Dierker, L. C., Hedeker, D., & Mermelstein,
#'   R. J. (2012). A practical guide to calculating Cohen's f2, a measure of
#'   local effect size, from PROC MIXED. *Frontiers in Psychology, 3*, Article
#'   111. https://doi.org/10.3389/fpsyg.2012.00111
#'
#' @examples
#'
#'   rsq.to.f(r.squared.full = 0.02) # small
#'   rsq.to.f(r.squared.full = 0.13) # medium
#'   rsq.to.f(r.squared.full = 0.26) # large
#'
#' @export rsq.to.f
rsq.to.f <- function(r.squared.full, r.squared.reduced = 0, verbose = 0) {

  check.nonnegative(r.squared.full, r.squared.reduced)
  verbose <- ensure_verbose(verbose)

  if (r.squared.full < r.squared.reduced)
    stop("Expecting `r.squared.full` > `r.squared.reduced`.", call. = FALSE)

  f.squared <- (r.squared.full - r.squared.reduced) / (1 - r.squared.full)

  if (verbose > 0)
    print(c(f.squared = f.squared, f = sqrt(f.squared), r.squared.full = r.squared.full, r.squared.reduced = r.squared.reduced))

  invisible(list(f.squared = f.squared, f = sqrt(f.squared),
                 r.squared.full = r.squared.full,
                 r.squared.reduced = r.squared.reduced))
} # rsq.to.f


#' Conversion from Cohen's f to R-squared
#'
#' @description
#' Helper function to convert between Cohen's f and R-squared.
#'
#'
#' @param f              Cohen's f.
#' @param r.squared.full R-squared for the full model.
#' @param verbose        \code{1} by default (returns results), if \code{0} no
#'                       output is printed on the console.
#'
#' @return
#'   \item{f}{Cohen's f.}
#'   \item{f.squared}{Cohen's f-squared.}
#'   \item{r.squared.full}{R-squared for the full model.}
#'   \item{r.squared.reduced}{R-squared for the reduced model.}
#'
#' @references
#'   Cohen, J. (1988). Statistical power analysis for the behavioral sciences
#'   (2nd ed.). Lawrence Erlbaum Associates.
#'
#'   Selya, A. S., Rose, J. S., Dierker, L. C., Hedeker, D., & Mermelstein,
#'   R. J. (2012). A practical guide to calculating Cohen's f2, a measure of
#'   local effect size, from PROC MIXED. *Frontiers in Psychology, 3*, Article
#'   111. https://doi.org/10.3389/fpsyg.2012.00111
#'
#' @examples
#'
#'   f.to.rsq(f = 0.14) # small
#'   f.to.rsq(f = 0.39) # medium
#'   f.to.rsq(f = 0.59) # large
#'
#' @export f.to.rsq
f.to.rsq <- function(f, r.squared.full = NULL, verbose = 0) {

  check.nonnegative(f)
  verbose <- ensure_verbose(verbose)

  f.squared <- f ^ 2
  if (is.null(r.squared.full)) {
    r.squared.full <- f.squared / (1 + f.squared)
    r.squared.reduced <- 0
  } else {
    change.r.squared <- f.squared * (1 - r.squared.full)
    r.squared.reduced <- r.squared.full - change.r.squared
  }

  if (verbose > 0)
    print(c(f.squared = f.squared, f = sqrt(f.squared), r.squared.full = r.squared.full, r.squared.reduced = r.squared.reduced))

  invisible(list(f.squared = f.squared, f = sqrt(f.squared),
                 r.squared.full = r.squared.full,
                 r.squared.reduced = r.squared.reduced))
} # f.to.rsq


############################
# linear regression f test #
############################

#' Power Analysis for Linear Regression: R-squared or R-squared Change (F-Test)
#'
#' @description
#' Calculates power or sample size (only one can be NULL at a time) to test
#' R-squared deviation from 0 (zero) in linear regression or to test R-squared
#' change between two linear regression models. The test of R-squared change is
#' often used to evaluate incremental contribution of a set of predictors in
#' hierarchical linear regression.
#'
#' Formulas are validated using Monte Carlo simulation, G*Power, and tables in
#' the PASS documentation.
#'
#' @details
#' * NB: The \code{pwrss.f.regression} function and its alias
#'   \code{pwrss.f.reg()} are deprecated, but they will remain available as a
#'   wrapper for the \code{power.f.regression()} function during a transition
#'   period.
#'
#' @aliases pwrss.f.reg power.f.reg pwrss.f.regression power.f.regression
#'
#'
#' @param r.squared.change R-squared (or R-squared change).
#' @param margin           margin - ignorable R-squared (or R-squared change).
#' @param k.total          integer; total number of predictors.
#' @param k.tested         integer; number of predictors in the subset of
#'                         interest. By default \code{k.tested = k.total},
#'                         which implies that one is interested in the
#'                         contribution of all predictors, and tests whether
#'                         R-squared value is different from 0 (zero).
#' @param n                integer; sample size.
#' @param power            statistical power, defined as the probability of
#'                         correctly rejecting a false null hypothesis, denoted
#'                         as \eqn{1 - \beta}.
#' @param alpha            type 1 error rate, defined as the probability of
#'                         incorrectly rejecting a true null hypothesis,
#'                         denoted as \eqn{\alpha}.
#' @param ceiling          logical; whether sample size should be rounded up.
#'                         \code{TRUE} by default.
#' @param verbose          \code{1} by default (returns test, hypotheses, and
#'                         results), if \code{2} a more detailed output is
#'                         given (plus key parameters and definitions), if
#'                         \code{0} no output is printed on the console.
#' @param utf              logical; whether the output should show Unicode
#'                         characters (if encoding allows for it).
#'                         \code{FALSE} by default.
#'
#' @return
#'   \item{parms}{list of parameters used in calculation.}
#'   \item{test}{type of the statistical test (F-Test).}
#'   \item{df1}{numerator degrees of freedom.}
#'   \item{df2}{denominator degrees of freedom.}
#'   \item{ncp}{non-centrality parameter for the alternative.}
#'   \item{null.ncp}{non-centrality parameter for the null.}
#'   \item{f.alpha}{critical value.}
#'   \item{power}{statistical power \eqn{(1-\beta)}.}
#'   \item{n}{sample size.}
#'
#' @references
#'   Bulus, M., & Polat, C. (2023). pwrss R paketi ile istatistiksel guc
#'   analizi \[Statistical power analysis with pwrss R package\]. *Ahi Evran
#'   Universitesi Kirsehir Egitim Fakultesi Dergisi, 24*(3), 2207-2328.
#'   https://doi.org/10.29299/kefad.1209913
#'
#'   Cohen, J. (1988). Statistical power analysis for the behavioral sciences
#'   (2nd ed.). Lawrence Erlbaum Associates.
#'
#' @examples
#'
#' # in the outcome (R-squared = 0.15).
#' power.f.regression(r.squared = 0.15,
#'                    k.total = 3, # total number of predictors
#'                    power = 0.80)
#'
#' # adding two more variables will increase R-squared
#' # from 0.15 (with 3 predictors) to 0.25 (with 3 + 2 predictors)
#' power.f.regression(r.squared.change = 0.10, # R-squared change
#'                    k.total = 5, # total number of predictors
#'                    k.tested = 2, # predictors to be tested
#'                    power = 0.80)
#'
#' @export power.f.regression
power.f.regression <- function(r.squared.change = NULL,
                               margin = 0,
                               k.total,
                               k.tested = k.total,
                               n = NULL, power = NULL, alpha = 0.05,
                               ceiling = TRUE, verbose = 1, utf = FALSE) {

  func.parms <- clean.parms(as.list(environment()))

  # r.squared.change is checked below
  check.numeric(margin)
  check.positive(k.total, k.tested)
  if (!is.null(n)) check.sample.size(n)
  if (!is.null(power)) check.proportion(power)
  check.proportion(alpha)
  check.logical(ceiling, utf)
  verbose <- ensure_verbose(verbose)
  requested <- check.n_power(n, power)

  if (!is.numeric(r.squared.change) || !is.finite(r.squared.change) || r.squared.change <= 0 || r.squared.change >= 1)
    stop("Value for `r.squared.change` must be a finite number that is larger than 0 and smaller than 1.", call. = FALSE)
  if (k.tested >  k.total)
    stop("`k.tested` cannot be greater than `k.total`.", call. = FALSE)

  pwr.f.reg <- function(r.squared.change, margin, k.total, k.tested, n, alpha) {

    df1 <- k.tested
    df2 <- n - k.total - 1

    f.squared <- rsq.to.f(r.squared.full = r.squared.change, verbose = 0)$f.squared
    null.f.squared <- rsq.to.f(r.squared.full = margin, verbose = 0)$f.squared

    lambda <- f.squared * n
    null.lambda <- null.f.squared * n


    f.alpha <- stats::qf(alpha, df1 = df1, df2 = df2, ncp = null.lambda, lower.tail = FALSE)
    power <- stats::pf(f.alpha, df1 = df1, df2 = df2, ncp = lambda, lower.tail = FALSE)

    list(power = power, lambda = lambda, null.lambda = null.lambda,
         df1 = df1, df2 = df2, f.alpha = f.alpha)

  } # pwr

  ss.f.reg <- function(r.squared.change, margin, k.total, k.tested, power, alpha) {

    n <- try(silent = TRUE,
             suppressWarnings({
               stats::uniroot(function(n) {
                 power - pwr.f.reg(r.squared.change = r.squared.change, margin = margin,
                                   k.total = k.total, k.tested = k.tested,
                                   n = n, alpha = alpha)$power
               }, interval = c(k.total + 2, 1e10))$root
             }) # supressWarnings
    ) # try

    if (inherits(n, "try-error") || n == 1e10) stop("Design is not feasible.", call. = FALSE)

    n

  } # ss.f.reg()


  if (requested == "n") {

    n <- ss.f.reg(r.squared.change = r.squared.change, margin = margin,
                  k.total = k.total, k.tested =  k.tested,
                  power = power, alpha = alpha)

    if (ceiling) n <- ceiling(n)

  }

  # calculate power (if requested == "power") or update it (if requested == "n")
  pwr.obj <- pwr.f.reg(r.squared.change = r.squared.change, margin = margin,
                       k.total = k.total, k.tested = k.tested, n = n, alpha = alpha)

  power <- pwr.obj$power
  df1 <- pwr.obj$df1
  df2 <- pwr.obj$df2
  lambda <- pwr.obj$lambda
  null.lambda <- pwr.obj$null.lambda
  f.alpha <- pwr.obj$f.alpha

  if (verbose > 0) {

    test <- ifelse(k.tested == k.total, "Linear Regression (F-Test)", "Hierarchical Linear Regression (F-Test)")

    print.obj <- list(requested = requested,
                      test = test,
                      r.squared.change = r.squared.change,
                      k.total = k.total,
                      k.tested = k.tested,
                      margin = margin,
                      n = n,
                      df1 = df1,
                      df2 = df2,
                      ncp.alternative = lambda,
                      ncp.null = null.lambda,
                      f.alpha = f.alpha,
                      alpha = alpha,
                      power = power)

    .print.pwrss.f.regression(print.obj, verbose = verbose, utf = utf)

  } # verbose

  invisible(structure(list(parms = func.parms,
                           test = "F",
                           df1 = df1,
                           df2 = df2,
                           ncp = lambda,
                           null.ncp = null.lambda,
                           f.alpha = f.alpha,
                           power = power,
                           n = n),
                      class = c("pwrss", "f", "regression")))


} # pwrss.f.regression()

#' @export power.f.reg
power.f.reg <- power.f.regression

#' @export pwrss.f.regression
pwrss.f.regression <- function(r2 = NULL, f2 = NULL,
                        k = 1, m = k, alpha = 0.05,
                        n = NULL, power = NULL, verbose = TRUE) {

  verbose <- ensure_verbose(verbose)

  if (all(check.not_null(r2, f2))) {
    stop("Effect size conflict for the alternative. Specify only either `r2` or `f2`.", call. = FALSE)
  } else if (check.not_null(f2)) {
    r2 <- f.to.rsq(f = sqrt(f2), verbose = 0)$r.squared.full
  }

  pwrss.f.reg.obj <- power.f.regression(r.squared.change = r2, margin = 0,
                                        k.total = k, k.tested = m,
                                        n = n, power = power, alpha = alpha,
                                        ceiling = TRUE, verbose = verbose)

  # cat("This function will be removed in the future. \n Please use power.f.regression() function. \n")

  return(invisible(pwrss.f.reg.obj))

} # pwrss.f.regression

#' @export pwrss.f.reg
pwrss.f.reg <- pwrss.f.regression


#####################
# linear regression #
#####################

# if the predictor is binary provide sd.predictor = sqrt(p * (1 - p))
# p = proportion of subjects in treatment group
# use defaults if beta is standardized

#' Power Analysis for Linear Regression: Single Coefficient (T-Test)
#'
#' @description
#' Calculates power or sample size (only one can be NULL at a time) to test a
#' single coefficient in multiple linear regression. The predictor is assumed
#' to be continuous by default. However, one can calculate power or sample size
#' for a binary predictor (such as treatment and control groups in an
#' experimental design) by specifying \code{sd.predictor = sqrt(p*(1-p))} where
#' \code{p} is the proportion of subjects in one of the groups. The sample size
#' in each group would be \code{n*p} and \code{n*(1-p)}.
#' \code{power.t.regression()}\code{pwrss.t.regression()} are the same
#' functions, as well as \code{power.t.reg()} and \code{pwrss.t.reg()}.
#'
#' Minimal effect and equivalence tests are implemented in line with Hodges and
#' Lehmann (1954), Kim and Robinson (2019), Phillips (1990), and Dupont and
#' Plummer (1998).
#'
#' Formulas are validated using Monte Carlo simulation, G*Power, tables in the
#' PASS documentation, and tables in Bulus (2021).
#'
#' @details
#' * NB: The \code{pwrss.z.regression()} function and its alias
#'   \code{pwrss.z.reg()} are deprecated, but they will remain available as a
#'   wrapper for the \code{power.t.regression()} function during a transition
#'   period.
#'
#'
#' @aliases power.t.regression pwrss.t.regression pwrss.z.regression
#'          pwrss.t.reg pwrss.z.reg power.t.reg
#'
#' @param beta         regression coefficient. One can use standardized
#'                     regression coefficient, but should keep
#'                     \code{sd.predictor = 1} and \code{sd.outcome = 1} or
#'                     leave them out as they are default specifications.
#' @param null.beta    regression coefficient under null hypothesis (typically
#'                     zero). One can use standardized regression coefficient,
#'                     but should keep \code{sd.predictor = 1} and
#'                     \code{sd.outcome = 1} or leave them out as they are
#'                     default specifications.
#' @param margin       margin - ignorable \code{beta} - \code{null.beta}
#'                     difference.
#' @param sd.predictor standard deviation of the predictor. For a binary
#'                     predictor, \code{sd.predictor = sqrt(p * (1 - p))} where
#'                     \code{p} is the proportion of subjects in one of the
#'                     groups.
#' @param sd.outcome   standard deviation of the outcome.
#' @param k.total      integer; total number of predictors, including the
#'                     predictor of interest.
#' @param r.squared    model R-squared. The default is
#'                     \code{r.squared = (beta * sd.predictor / sd.outcome) ^ 2}
#'                     assuming a linear regression with one predictor. Thus,
#'                     an \code{r.squared} below this value will throw a
#'                     warning.
#'                     To consider other covariates in the model provide a value
#'                     greater than the default \code{r.squared} along with the
#'                     argument \code{k.total > 1}.
#' @param n            integer; sample size.
#' @param power        statistical power, defined as the probability of
#'                     correctly rejecting a false null hypothesis, denoted as
#'                     \eqn{1 - \beta}.
#' @param alpha        type 1 error rate, defined as the probability of
#'                     incorrectly rejecting a true null hypothesis, denoted as
#'                     \eqn{\alpha}.
#' @param alternative  character; the direction or type of the hypothesis test:
#'                     "two.sided", "one.sided", or "two.one.sided".
#' @param ceiling      logical; whether sample size should be rounded up.
#'                     \code{TRUE} by default.
#' @param verbose      \code{1} by default (returns test, hypotheses, and
#'                     results), if \code{2} a more detailed output is given
#'                     (plus key parameters and definitions), if \code{0} no
#'                     output is printed on the console.
#' @param utf          logical; whether the output should show Unicode
#'                     characters (if encoding allows for it). \code{FALSE} by
#'                     default.
#'
#' @return
#'   \item{parms}{list of parameters used in calculation.}
#'   \item{test}{type of the statistical test (T-Test).}
#'   \item{df}{degrees of freedom.}
#'   \item{ncp}{non-centrality parameter for the alternative.}
#'   \item{null.ncp}{non-centrality parameter for the null.}
#'   \item{t.alpha}{critical value(s).}
#'   \item{power}{statistical power \eqn{(1-\beta)}.}
#'   \item{n}{sample size.}
#'
#' @references
#'   Bulus, M. (2021). Sample size determination and optimal design of
#'   randomized / non-equivalent pretest-post-test control-group designs.
#'   *Adiyaman University Journal of Educational Sciences, 11*(1), 48-69.
#'   https://doi.org/10.17984/adyuebd.941434
#'
#'   Hodges Jr, J. L., & Lehmann, E. L. (1954). Testing the approximate
#'   validity of statistical hypotheses. *Journal of the Royal Statistical
#'   Society Series B: Statistical Methodology, 16*(2), 261-268.
#'   https://doi.org/10.1111/j.2517-6161.1954.tb00169.x
#'
#'   Kim, J. H., & Robinson, A. P. (2019). Interval-based hypothesis testing
#'   and its applications to economics and finance. *Econometrics, 7*(2), 21.
#'   https://doi.org/10.1111/10.3390/econometrics7020021
#'
#'   Phillips, K. F. (1990). Power of the two one-sided tests procedure in
#'   bioequivalence. *Journal of Pharmacokinetics and Biopharmaceutics, 18*(2),
#'   137-144. https://doi.org/10.1007/bf01063556
#'
#'   Dupont, W. D., and Plummer, W. D. (1998). Power and sample size
#'   calculations for studies involving linear regression. *Controlled Clinical
#'   Trials, 19*(6), 589-601.
#'   https://doi.org/10.1007/10.1016/s0197-2456(98)00037-3
#'
#' @examples
#'
#' # continuous predictor x (and 4 covariates)
#' power.t.regression(beta = 0.20,
#'             k.total = 5,
#'             r.squared = 0.30,
#'             power = 0.80)
#'
#' # binary predictor x (and 4 covariates)
#' p <- 0.50 # proportion of subjects in one group
#' power.t.regression(beta = 0.20,
#'             sd.predictor = sqrt(p*(1-p)),
#'             k.total = 5,
#'             r.squared = 0.30,
#'             power = 0.80)
#'
#' # non-inferiority test with binary predictor x (and 4 covariates)
#' p <- 0.50 # proportion of subjects in one group
#' power.t.regression(beta = 0.20, # Cohen's d
#'             margin = -0.05, # non-inferiority margin in Cohen's d unit
#'             alternative = "one.sided",
#'             sd.predictor = sqrt(p*(1-p)),
#'             k.total = 5,
#'             r.squared = 0.30,
#'             power = 0.80)
#'
#' # superiority test with binary predictor x (and 4 covariates)
#' p <- 0.50 # proportion of subjects in one group
#' power.t.regression(beta = 0.20, # Cohen's d
#'             margin = 0.05, # superiority margin in Cohen's d unit
#'             alternative = "one.sided",
#'             sd.predictor = sqrt(p*(1-p)),
#'             k.total = 5,
#'             r.squared = 0.30,
#'             power = 0.80)
#'
#' # equivalence test with binary predictor x (and 4 covariates)
#' p <- 0.50 # proportion of subjects in one group
#' power.t.regression(beta = 0, # Cohen's d
#'             margin = c(-0.05, 0.05), # equivalence bounds in Cohen's d unit
#'             alternative = "two.one.sided",
#'             sd.predictor = sqrt(p*(1 - p)),
#'             k.total = 5,
#'             r.squared = 0.30,
#'             power = 0.80)
#'
#' @export power.t.regression
power.t.regression <- function(beta, null.beta = 0, margin = 0,
                               sd.predictor = 1, sd.outcome = 1,
                               r.squared = (beta * sd.predictor / sd.outcome) ^ 2,
                               k.total = 1,
                               n = NULL, power = NULL, alpha = 0.05,
                               alternative = c("two.sided", "one.sided", "two.one.sided"),
                               ceiling = TRUE, verbose = 1, utf = FALSE) {

  alternative <- tolower(match.arg(alternative))
  func.parms <- clean.parms(as.list(environment()))

  check.numeric(beta, null.beta)
  margin <- check.margins(margin, check.numeric, alternative)
  check.positive(sd.predictor, sd.outcome, k.total)
  if (!is.null(n)) check.sample.size(n)
  if (!is.null(power)) check.proportion(power)
  check.proportion(alpha)
  check.logical(ceiling, utf)
  verbose <- ensure_verbose(verbose)
  requested <- check.n_power(n, power)

  if (!is.numeric(r.squared) || r.squared > 1 || r.squared < 0)
    stop("Incorrect value for `r.squared`, specify `r.squared` explicitly or modify `beta`, `sd.predictor`, `sd.outcome`.", call. = FALSE)
  if (r.squared > 0 && r.squared < (beta * sd.predictor / sd.outcome) ^ 2)
    warning("`r.squared` is possibly larger.", call. = FALSE)

  pwr.t.reg <- function(beta, null.beta, margin,
                        sd.outcome, sd.predictor,
                        n, k.total, r.squared,
                        alpha, alternative) {

    df <- n - k.total - 1
    lambda <- (beta - null.beta)  / ((sd.outcome / sd.predictor) * sqrt((1 - r.squared) / n))
    null.lambda <- margin / ((sd.outcome / sd.predictor) * sqrt((1 - r.squared) / n))

    pwr.obj <- power.t.test(ncp = lambda, null.ncp = null.lambda, df = df,
                            alpha = alpha, alternative = alternative,
                            plot = FALSE, verbose = 0)
    power <- pwr.obj$power
    t.alpha <- pwr.obj$t.alpha

    list(power = power, lambda = lambda, null.lambda = null.lambda, df = df, t.alpha = t.alpha)

  } # pwr.t.reg()


  ss.t.reg <- function(beta, null.beta, margin,
                       sd.outcome, sd.predictor,
                       power, k.total, r.squared,
                       alpha, alternative) {

    n <- try(silent = TRUE,
             suppressWarnings({
               stats::uniroot(function(n) {
                 power - pwr.t.reg(beta = beta, null.beta = null.beta, margin = margin,
                                   sd.outcome = sd.outcome, sd.predictor = sd.predictor,
                                   n = n, k.total = k.total, r.squared = r.squared,
                                   alpha = alpha, alternative =  alternative)$power
               }, interval = c(k.total + 2, 1e10))$root
             }) # supressWarnings
    ) # try

    if (inherits(n, "try-error") || n == 1e10) stop("Design is not feasible.", call. = FALSE)

    n

  } # ss.t.reg()


  if (requested == "n") {

    n <- ss.t.reg(beta = beta, null.beta = null.beta, margin = margin,
                  sd.outcome = sd.outcome, sd.predictor = sd.predictor,
                  power = power, k.total = k.total, r.squared = r.squared,
                  alpha = alpha, alternative =  alternative)

    if (ceiling) n <- ceiling(n)

  }

  # calculate power (if requested == "power") or update it (if requested == "n")
  pwr.obj <- pwr.t.reg(beta = beta, null.beta = null.beta, margin = margin,
                       sd.outcome = sd.outcome, sd.predictor = sd.predictor,
                       n = n, k.total = k.total, r.squared = r.squared,
                       alpha = alpha, alternative =  alternative)

  power <- pwr.obj$power
  lambda <- pwr.obj$lambda
  null.lambda <- pwr.obj$null.lambda
  df <- pwr.obj$df
  t.alpha <- pwr.obj$t.alpha

  std.beta <- beta * (sd.predictor / sd.outcome)
  std.null.beta <- null.beta * (sd.predictor / sd.outcome)
  std.margin <- margin * (sd.predictor / sd.outcome)

  if (verbose > 0) {

    print.obj <- list(requested = requested,
                      test = "Linear Regression Coefficient (T-Test)",
                      alternative = alternative,
                      std.beta = std.beta,
                      std.null.beta = std.null.beta,
                      std.margin = std.margin,
                      margin = margin,
                      n = n,
                      df = df,
                      ncp.alternative = lambda,
                      ncp.null = null.lambda,
                      t.alpha = t.alpha,
                      alpha = alpha,
                      power = power)

    .print.pwrss.t.regression(print.obj, verbose = verbose, utf = utf)

  } # verbose

  invisible(structure(list(parms = func.parms,
                           test = "t",
                           std.beta = std.beta,
                           std.null.beta = std.null.beta,
                           std.margin = std.margin,
                           df = df,
                           t.alpha = t.alpha,
                           ncp = lambda,
                           null.ncp = null.lambda,
                           power = power,
                           n = n),
                      class = c("pwrss", "t", "regression")))
} # power.t.regression()

#' @export power.t.reg
power.t.reg <- power.t.regression

#' @export pwrss.t.regression
pwrss.t.regression <- function(beta1 = 0.25, beta0 = 0, margin = 0,
                               sdx = 1, sdy = 1,
                               k = 1, r2 = (beta1 * sdx / sdy) ^ 2,
                               alpha = 0.05, n = NULL, power = NULL,
                               alternative = c("not equal", "less", "greater",
                                               "non-inferior", "superior", "equivalent"),
                               verbose = TRUE) {

  verbose <- ensure_verbose(verbose)
  alternative <- tolower(match.arg(alternative))
  if (alternative %in% c("less", "greater", "non-inferior", "superior")) alternative <- "one.sided"
  if (alternative == "not equal") alternative <- "two.sided"
  if (alternative == "equivalent") {
    alternative <- "two.one.sided"
    if (length(margin) == 1) margin <- c(min(-margin, margin), max(-margin, margin))
  }

  pwrss.t.reg.obj <- power.t.regression(beta = beta1, null.beta = beta0, margin = margin,
                                        sd.predictor = sdx, sd.outcome = sdy,
                                        r.squared = r2, k.total = k,
                                        n = n, power = power, alpha = alpha,
                                        alternative = alternative,
                                        ceiling = TRUE, verbose = verbose)

  # cat("This function will be removed in the future. \n Please use power.t.regression() function. \n")

  return(invisible(pwrss.t.reg.obj))

} # pwrss.t.regression

#' @export pwrss.t.reg
pwrss.t.reg <- pwrss.t.regression


# defunct
#' @export pwrss.z.regression
#' @export pwrss.z.reg
pwrss.z.regression <- pwrss.z.reg <- function(...) {
  stop("This function is no longer available. Please use `power.t.regression()`.", call. = FALSE)
}
