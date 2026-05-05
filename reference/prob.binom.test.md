# Find Probability (Non-Centrality) for the Generic Binomial Test

Finds probability (non-centrality) for the generic binomial test with
(optional) Type 1 and Type 2 error plots.

## Usage

``` r
prob.binom.test(
  power = 0.8,
  size,
  prob = NULL,
  req.sign = "+",
  null.prob = 0.5,
  alpha = 0.05,
  alternative = c("two.sided", "one.sided", "two.one.sided"),
  plot = TRUE,
  verbose = 1,
  utf = FALSE
)
```

## Arguments

- power:

  statistical power \\(1-\beta)\\.

- size:

  number of trials (zero or more).

- prob:

  probability of success on each trial under alternative.

- req.sign:

  whether 'prob' is expected to be greater '+1', less than '-1', or
  within '0' the null.prob' bounds.

- null.prob:

  probability of success on each trial under null.

- alpha:

  type 1 error rate, defined as the probability of incorrectly rejecting
  a true null hypothesis, denoted as \\\alpha\\.

- alternative:

  character; the direction or type of the hypothesis test: "two.sided",
  "one.sided", or "two.one.sided". For non-inferiority or superiority
  tests, add or subtract the margin from the null hypothesis value and
  use alternative = "one.sided".

- plot:

  logical; `FALSE` switches off Type 1 and Type 2 error plot. `TRUE` by
  default.

- verbose:

  `1` by default (returns test, hypotheses, and results), if `2` a more
  detailed output is given (plus key parameters and definitions), if `0`
  no output is printed on the console.

- utf:

  logical; whether the output should show Unicode characters (if
  encoding allows for it). `FALSE` by default.

## Value

- size:

  number of trials (zero or more).

- prob:

  probability of success on each trial under alternative.

- null.prob:

  probability of success on each trial under null.

- binom.alpha:

  critical value(s).

- power:

  statistical power \\(1-\beta)\\.

## Examples

``` r
# one-sided
prob.binom.test(size = 200, prob = NULL, req.sign = "+", power = 0.80, null.prob = 0.5,
                 alpha = 0.05, alternative = "one.sided")

#> +--------------------------------------------------+
#> |                POWER CALCULATION                 |
#> +--------------------------------------------------+
#> 
#> Generic Binomial Test
#> 
#> ----------------------------------------------------
#> Hypotheses
#> ----------------------------------------------------
#>   H0 (Null)        : prob <= null.prob
#>   H1 (Alternative) : prob  > null.prob
#> 
#> ----------------------------------------------------
#> Results
#> ----------------------------------------------------
#>   Type 1 Error (alpha) = 0.038
#>   Type 2 Error (beta)  = 0.200
#>   Statistical Power    = 0.800  <<
#> 

# two-sided
prob.binom.test(size = 200, prob = NULL, req.sign = "+", power = 0.80, null.prob = 0.5,
                 alpha = 0.05, alternative = "two.sided")

#> +--------------------------------------------------+
#> |                POWER CALCULATION                 |
#> +--------------------------------------------------+
#> 
#> Generic Binomial Test
#> 
#> ----------------------------------------------------
#> Hypotheses
#> ----------------------------------------------------
#>   H0 (Null)        : prob  = null.prob
#>   H1 (Alternative) : prob != null.prob
#> 
#> ----------------------------------------------------
#> Results
#> ----------------------------------------------------
#>   Type 1 Error (alpha) = 0.040
#>   Type 2 Error (beta)  = 0.200
#>   Statistical Power    = 0.800  <<
#> 

# equivalence
prob.binom.test(size = 200, prob = NULL, req.sign = "0", power = 0.80, null.prob = c(0.4, 0.6),
                 alpha = 0.05, alternative = "two.one.sided")

#> +--------------------------------------------------+
#> |                POWER CALCULATION                 |
#> +--------------------------------------------------+
#> 
#> Generic Binomial Test
#> 
#> ----------------------------------------------------
#> Hypotheses
#> ----------------------------------------------------
#>   H0 (Null)        : prob <= min(null.prob) or
#>                      prob >= max(null.prob)
#>   H1 (Alternative) : prob  > min(null.prob) and
#>                      prob  < max(null.prob)
#> 
#> ----------------------------------------------------
#> Results
#> ----------------------------------------------------
#>   Type 1 Error (alpha) = 0.049
#>   Type 2 Error (beta)  = 0.229
#>   Statistical Power    = 0.771  <<
#> 
```
