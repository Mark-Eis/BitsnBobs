# Sample Size Calculaton

Calculate the sample size required from a population to estimate the
prevalence of a dichotomous variable to a given degree of absolute
precision and with a specified level of confidence.

## Usage

``` r
sample_size(Pexp = 0.5, d = 0.05, N = NULL, conf_level = 0.95)
```

## Arguments

- Pexp:

  `numeric` (or `numeric vector` with all values) between 0 and 1
  representing the expected prevalence; default `0.5`.

- d:

  a positive `numeric` (or `numeric vector` with all values) greater
  than zero representing the desired absolute precision; default `0.05`
  .

- N:

  either `NULL` for a large (theoretically infinite) population or a
  positive `integer` (or `integer vector` with all values) greater than
  zero representing the population size; default `NULL`.

- conf_level:

  the confidence level(s) required; default `0.95`.

## Value

A `numeric` (or `numeric vector`) giving the required sample size(s).

## Details

For large study populations, the sample size, `n`, may be calculated as:
-

\$\$n = \displaystyle \frac{z^{2}P\_{exp}(1-P\_{exp})}{d^{2}}\$\$

where \\P\_{exp}\\ is the expected prevalence, `z` is the quantile of
the normal distribution corresponding to the confidence level required
and `d` is the desired absolute precision.

For relatively small study populations of size `N`, the value of `n` may
be adjusted thus: -

\$\$n\_{adj} = \displaystyle \frac{n.N}{n + N}\$\$

## Note

*Sample Size Calculator* at
[www.calculator.net](https://www.calculator.net) has further useful
[information on
derivation](https://www.calculator.net/sample-size-calculator.html).

## References

Thrusfield, M., Christley, R. In *Veterinary Epidemiology* 4th Edn. John
Wiley & Sons Ltd. 2018. (See Ch.13.)
[](https://doi.org/10.1002/9781118280249)[doi:10.1002/9781118280249](https://doi.org/10.1002/9781118280249)
.

## See also

Other sample-size:
[`design_effect()`](https://mark-eis.github.io/BitsnBobs/reference/design_effect.md)

## Examples

``` r
## Infinite population
sample_size()         ## desired absolute precision 0.05 (default)
#> [1] 385
sample_size(d = 0.1)  ## desired absolute precision 0.1
#> [1] 97

## Population = 500, 750 or 1000
sample_size(N = c(500L, 750L, 1000L))
#> [1] 218 255 278

## Expected prevalence = 0.125, 0.25, 0.50, 0.75 or 0.875
## Note symmetry of resulting sample sizes
sample_size(Pexp = c(0.125, 0.25, 0.50, 0.75, 0.875))
#> [1] 169 289 385 289 169

## Desired absolute precision = 1%, 5%, 10%, 20%
sample_size(d = c(0.01, 0.05, 0.1, 0.2))
#> [1] 9604  385   97   25
```
