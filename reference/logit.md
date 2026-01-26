# Convert Probability to Logit

`logit()` converts a probability to the corresponding logit value.

`prob_from_logit()` converts a logit value to the corresponding
probability.

## Usage

``` r
logit(x)

prob_from_logit(x)
```

## Arguments

- x:

  a probability from which to derive the logit, or a logit from which to
  derive the probability.

## Value

`logit` returns the derived logit; `prob_from_logit` returns the derived
probability.

## Details

The `logit` is defined as the log of the odds or \\\log(p / q)\\, where
\\q = 1 - p\\.

The probability may be calculated from the `logit` by the inverse
transformation: -

\$\$p = \exp(\displaystyle \frac{logit}{1 + logit})\$\$

## See also

[`exp`](https://rdrr.io/r/base/Log.html),
[`log`](https://rdrr.io/r/base/Log.html),
[`plogis`](https://rdrr.io/r/stats/Logistic.html) and
[`qlogis`](https://rdrr.io/r/stats/Logistic.html) and the *Note* section
of [`Logistic`](https://rdrr.io/r/stats/Logistic.html) in the
[stats](https://rdrr.io/r/stats/stats-package.html) package.

Other odds-logit:
[`odds()`](https://mark-eis.github.io/BitsnBobs/reference/odds.md)

## Examples

``` r
logit(0.5)
#> [1] 0
prob_from_logit(0)
#> [1] 0.5

(s <- seq(-10, 10, by = 2))
#>  [1] -10  -8  -6  -4  -2   0   2   4   6   8  10
s[1:6] + s[11:6]
#> [1] 0 0 0 0 0 0

(s <- prob_from_logit(s))
#>  [1] 4.539787e-05 3.353501e-04 2.472623e-03 1.798621e-02 1.192029e-01
#>  [6] 5.000000e-01 8.807971e-01 9.820138e-01 9.975274e-01 9.996646e-01
#> [11] 9.999546e-01
s[1:6] + s[11:6]
#> [1] 1 1 1 1 1 1

(s <- logit(s))
#>  [1] -10  -8  -6  -4  -2   0   2   4   6   8  10

rm(s)
```
