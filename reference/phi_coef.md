# Phi Correlation Coefficient of Association Between Paired Binary Variables

The *phi* correlation coefficient (or mean square contingency
coefficient and denoted by \\\phi\\ or \\r\phi\\) is a measure of
association between two naturally dichotomous variables.

## Usage

``` r
phi_coef(x)
```

## Arguments

- x:

  a square `matrix` containing the observations of two binary variables
  as a two-by-two table of counts.

## Value

A numeric between -1 and 1, the \\\phi\\ correlation coefficient.

## Details

For a two-by-two contingency table \\n\_{11}\\ \\n\_{12}\\ \\n\_{21}\\
\\n\_{22}\\ the \\\phi\\ correlation coefficient is given by: -

\$\$\displaystyle \phi = \frac{n\_{11}n\_{22} - n\_{12}n\_{21}}
{\sqrt{(n\_{11} + n\_{21})(n\_{12} + n\_{22})(n\_{11} +
n\_{12})(n\_{21} + n\_{22})}}\$\$

or equivalently, the determinant of the matrix divided by the
(principal) square root of the product of its four marginal sums.

## References

Yule, G.U. (1912). On the Methods of Measuring Association Between Two
Attributes. *J Royal Stat Soc*. **75** (6): 579–652.
[](https://doi.org/10.1177/10.2307/2340126)[doi:10.2307/2340126](https://doi.org/10.2307/2340126)
.

## See also

[`matrix`](https://rdrr.io/r/base/matrix.html),
[`mcnemar.test`](https://rdrr.io/r/stats/mcnemar.test.html)

Other correl_coef:
[`cor_coef.test()`](https://mark-eis.github.io/BitsnBobs/reference/cor_coef.test.md),
[`phi_coef.test()`](https://mark-eis.github.io/BitsnBobs/reference/phi_coef.test.md)

## Examples

``` r
## Example from Wikipedia
twobytwo <- matrix(c(6, 1, 2, 3), nrow = 2, dimnames = rep(list(c("Cat", "Dog")), 2) |>
              setNames(c("Actual", "Predicted")))
addmargins(twobytwo)
#>       Predicted
#> Actual Cat Dog Sum
#>    Cat   6   2   8
#>    Dog   1   3   4
#>    Sum   7   5  12

phi_coef(twobytwo)
#> [1] 0.4780914

## Example from Statology
twobytwo <- matrix(c(4, 8, 9, 4), nrow = 2, dimnames =
              list(Gender = c("Male", "Female"), Party = c("Dem", "Rep")))
addmargins(twobytwo)
#>         Party
#> Gender   Dem Rep Sum
#>   Male     4   9  13
#>   Female   8   4  12
#>   Sum     12  13  25

phi_coef(twobytwo)
#> [1] -0.3589744

rm(twobytwo)
```
