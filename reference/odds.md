# Convert Probability to Odds

`odds()` converts a probability to the corresponding odds.

`prob_from_odds()` converts odds to the corresponding proability.

## Usage

``` r
odds(x)

prob_from_odds(x)
```

## Arguments

- x:

  a probability from which to derive the odds, or odds from which to
  derive the probability.

## Value

`odds` returns the derived odds; `prob_from_odds` returns the derived
probability.

## Details

The `odds` are defined as \\\log(p / q)\\ where \\q = 1 - p\\.

The probability may be calculated from the odds by the inverse
transformation: -

\$\$p = \displaystyle \frac{odds}{1 + odds}\$\$

## See also

Other odds-logit:
[`logit()`](https://mark-eis.github.io/BitsnBobs/reference/logit.md)

## Examples

``` r
odds(0.5)
#> [1] 1
prob_from_odds(1)
#> [1] 0.5

(s <- seq(from = 0, to = 1, by = 0.125))
#> [1] 0.000 0.125 0.250 0.375 0.500 0.625 0.750 0.875 1.000
(s <- odds(s))
#> [1] 0.0000000 0.1428571 0.3333333 0.6000000 1.0000000 1.6666667 3.0000000
#> [8] 7.0000000       Inf
prob_from_odds(s)
#> [1] 0.000 0.125 0.250 0.375 0.500 0.625 0.750 0.875 1.000

rm(s) 
```
