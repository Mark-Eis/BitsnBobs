# Test For Association/Correlation Between Paired Binary Variables

This function calculates a p-value for the signifinace of a *phi*
correlation coefficient (or mean square contingency coefficient,
\\\phi\\ or \\r\phi\\), the measure of association between two binary
variables, and calculates a confidence interval.

## Usage

``` r
phi_coef.test(
  x,
  alternative = c("two.sided", "less", "greater"),
  conf.level = 0.95
)
```

## Arguments

- x:

  a square `matrix` containing the observations of two binary variables
  as a two-by-two table of counts.

- alternative:

  a `character` string specifying the alternative hypothesis, must be
  one of `"two.sided"` (default), `"greater"` or `"less"`. You can
  specify just the initial letter.

- conf.level:

  numeric between 0 and 1, the confidence level required; default
  `0.95`.

## Value

A list with class `"htest"` containing the following components: -

- statistic:

  the value of the test statistic.

- parameter:

  the number of (paired) observations.

- p.value:

  the p-value of the test.

- conf.int:

  confidence interval of the \\\phi\\ correlation coefficient (95% or
  other specified level).

- estimate:

  the \\\phi\\ correlation coefficient.

- null.value:

  the value of the association measure under the null hypothesis, always
  0.

- alternative:

  a character string describing the alternative hypothesis.

- method:

  the character string "Phi correlation coefficient with confidence
  interval".

- data.name:

  a character string giving the name of the data.

## Details

The *phi* coefficient is calculated using
[`phi_coef`](https://mark-eis.github.io/BitsnBobs/reference/phi_coef.md).
For derivation of the standard error and confidence interval, see Bishop
*et al.* (2003), and Bonett (2021). See also `ci.phi()`, the *confidence
interval for a phi correlation* in the [reference
manual](https://cran.r-project.org/web/packages/statpsych/statpsych.pdf)
for package [statpsych](https://CRAN.R-project.org/package=statpsych).

## References

Yule, G.U. (1912). On the Methods of Measuring Association Between Two
Attributes. *J Royal Stat Soc*. **75** (6): 579–652.
[](https://doi.org/10.1177/10.2307/2340126)[doi:10.2307/2340126](https://doi.org/10.2307/2340126)
.

Bishop, Y.M.M., Fienberg, S.E., Holland, P.W. (1975). *Discrete
Multivariate Analysis.* MIT Press. (See Ch.11.) [ISBN
978-0-387-72805-6](https://download.e-bookshelf.de/download/0000/0020/95/L-G-0000002095-0002339867.pdf).

Bonett, Douglas G. (2021). [Statistical Methods for
Psychologists](https://dgbonett.sites.ucsc.edu/statistical-methods-for-psychologists/),
Volume 3: Introduction to Introduction to Categorical Data Analysis.
University of California, Santa Cruz. (See 3.4 Measures of Association
for 2 × 2 Tables.)

## See also

[`matrix`](https://rdrr.io/r/base/matrix.html),
[`mcnemar.test`](https://rdrr.io/r/stats/mcnemar.test.html)

Other correl_coef:
[`cor_coef.test()`](https://mark-eis.github.io/BitsnBobs/reference/cor_coef.test.md),
[`phi_coef()`](https://mark-eis.github.io/BitsnBobs/reference/phi_coef.md)

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

phi_coef.test(twobytwo)
#> 
#>  Phi correlation coefficient with confidence interval
#> 
#> data:  twobytwo
#> z = 1.6528, n = 12, p-value = 0.09837
#> alternative hypothesis: true phi is not equal to 0
#> 95 percent confidence interval:
#>  -0.08885493  1.04503781
#> sample estimates:
#>       phi 
#> 0.4780914 
#> 
phi_coef.test(twobytwo, alternative = "less")
#> 
#>  Phi correlation coefficient with confidence interval
#> 
#> data:  twobytwo
#> z = 1.6528, n = 12, p-value = 0.9508
#> alternative hypothesis: true phi is less than 0
#> 95 percent confidence interval:
#>  -1.0000000  0.9538878
#> sample estimates:
#>       phi 
#> 0.4780914 
#> 
phi_coef.test(twobytwo, alternative = "greater")
#> 
#>  Phi correlation coefficient with confidence interval
#> 
#> data:  twobytwo
#> z = 1.6528, n = 12, p-value = 0.04919
#> alternative hypothesis: true phi is greater than 0
#> 95 percent confidence interval:
#>  0.002295051 1.000000000
#> sample estimates:
#>       phi 
#> 0.4780914 
#> 

## Example from Statology
twobytwo <- matrix(c(4, 8, 9, 4), nrow = 2, dimnames =
              list(Gender = c("Male", "Female"), Party = c("Dem", "Rep")))
addmargins(twobytwo)
#>         Party
#> Gender   Dem Rep Sum
#>   Male     4   9  13
#>   Female   8   4  12
#>   Sum     12  13  25

phi_coef.test(twobytwo)
#> 
#>  Phi correlation coefficient with confidence interval
#> 
#> data:  twobytwo
#> z = -1.919, n = 25, p-value = 0.05499
#> alternative hypothesis: true phi is not equal to 0
#> 95 percent confidence interval:
#>  -0.725613555  0.007664837
#> sample estimates:
#>        phi 
#> -0.3589744 
#> 

## Setting confidence level to 1 - p-value gives upper bound of confidence interval close to zero
pval <- phi_coef.test(twobytwo)$p.value
phi_coef.test(twobytwo, conf.level = 1 - pval)
#> 
#>  Phi correlation coefficient with confidence interval
#> 
#> data:  twobytwo
#> z = -1.919, n = 25, p-value = 0.05499
#> alternative hypothesis: true phi is not equal to 0
#> 94.50144 percent confidence interval:
#>  -7.179487e-01 -1.110223e-16
#> sample estimates:
#>        phi 
#> -0.3589744 
#> 
## Similarly, with one-tailed tests setting confidence level to 1 - p-value/2 conserves the upper
## or lower CI bound with alternative = "less" or alternative = "greater" respectively
phi_coef.test(twobytwo, alternative = "less", conf.level = 1 - pval/2)
#> 
#>  Phi correlation coefficient with confidence interval
#> 
#> data:  twobytwo
#> z = -1.919, n = 25, p-value = 0.02749
#> alternative hypothesis: true phi is less than 0
#> 97.25072 percent confidence interval:
#>  -1.000000e+00 -1.110223e-16
#> sample estimates:
#>        phi 
#> -0.3589744 
#> 
phi_coef.test(twobytwo, alternative = "greater", conf.level = 1 - pval/2)
#> 
#>  Phi correlation coefficient with confidence interval
#> 
#> data:  twobytwo
#> z = -1.919, n = 25, p-value = 0.9725
#> alternative hypothesis: true phi is greater than 0
#> 97.25072 percent confidence interval:
#>  -0.7179487  1.0000000
#> sample estimates:
#>        phi 
#> -0.3589744 
#> 

## Example from statpsych::ci.phi(), which should return: -
##       Estimate         SE         LL        UL
## [1,] 0.1229976 0.05746271 0.01037273 0.2356224

twobytwo <- matrix(c(229, 28, 96, 24), nrow = 2, dimnames = rep(list(c("Zero", "One")), 2))
addmargins(twobytwo)
#>      Zero One Sum
#> Zero  229  96 325
#> One    28  24  52
#> Sum   257 120 377

phi_coef.test(twobytwo)
#> 
#>  Phi correlation coefficient with confidence interval
#> 
#> data:  twobytwo
#> z = 2.1405, n = 377, p-value = 0.03232
#> alternative hypothesis: true phi is not equal to 0
#> 95 percent confidence interval:
#>  0.01037273 0.23562243
#> sample estimates:
#>       phi 
#> 0.1229976 
#> 
## Check standard error as expected
with(phi_coef.test(twobytwo), c(stderr = estimate/statistic))
#> stderr.phi 
#> 0.05746271 

rm(twobytwo, pval)
```
