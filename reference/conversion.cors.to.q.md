# Conversion from Correlation Difference to Cohen's q

Helper function to convert correlation difference to Cohen's q (and vice
versa). `cor.to.z()` function applies Fisher's z transformation.

## Usage

``` r
cor.to.z(rho, verbose = 1)

  cors.to.q(rho1, rho2, verbose = 1)

  q.to.cors(q, rho1 = NULL, rho2 = NULL, verbose = 1)
```

## Arguments

- rho:

  correlation.

- rho1:

  first correlation.

- rho2:

  second correlation.

- q:

  Cohen's q effect size.

- verbose:

  `1` by default (returns test, hypotheses, and results), if `2` a more
  detailed output is given (plus key parameters and defintions), if `0`
  no output is printed on the console.

## Value

- rho1:

  first correlation.

- rho2:

  second correlation.

- delta:

  correlation difference: rho1 - rho2.

- q:

  Cohen's q effect size.

## Examples

``` r
q.to.cors(q = 0.10, rho1 = 0.50)
#>           q       delta        rho1        rho2 
#>  0.10000000 -0.07120268  0.50000000  0.57120268 
q.to.cors(q = 0.30, rho1 = 0.50)
#>          q      delta       rho1       rho2 
#>  0.3000000 -0.1907068  0.5000000  0.6907068 
q.to.cors(q = 0.50, rho1 = 0.50)
#>          q      delta       rho1       rho2 
#>  0.5000000 -0.2815365  0.5000000  0.7815365 

cors.to.q(rho2 = 0.5712027, rho1 = 0.50)
#>          q      delta       rho1       rho2 
#> -0.1000000 -0.0712027  0.5000000  0.5712027 
cors.to.q(rho2 = 0.6907068, rho1 = 0.50)
#>          q      delta       rho1       rho2 
#> -0.3000000 -0.1907068  0.5000000  0.6907068 
cors.to.q(rho2 = 0.7815365, rho1 = 0.50)
#>          q      delta       rho1       rho2 
#> -0.5000001 -0.2815365  0.5000000  0.7815365 
```
