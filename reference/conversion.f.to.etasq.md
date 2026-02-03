# Conversion from Cohen's f to Eta-squared

Helper function to convert between Cohen's f and Eta-squared.

## Usage

``` r
f.to.etasq(f, verbose = 1)

etasq.to.f(eta.squared, verbose = 1)
```

## Arguments

- f:

  Cohen's f.

- eta.squared:

  (Partial) Eta-squared.

- verbose:

  `1` by default (returns test, hypotheses, and results), if `2` a more
  detailed output is given (plus key parameters and defintions), if `0`
  no output is printed on the console.

## Value

- f:

  Cohen's f.

- eta.squared:

  (Partial) Eta-squared.

## References

Cohen, J. (1988). Statistical power analysis for the behavioral sciences
(2nd ed.). Lawrence Erlbaum Associates.

## Examples

``` r
f.to.etasq(f = 0.10) # small
#> eta.squared   f.squared           f 
#>  0.00990099  0.01000000  0.10000000 
f.to.etasq(f = 0.25) # medium
#> eta.squared   f.squared           f 
#>  0.05882353  0.06250000  0.25000000 
f.to.etasq(f = 0.40) # large
#> eta.squared   f.squared           f 
#>    0.137931    0.160000    0.400000 
```
