# Test of Kurtosis

Computes \\G\_{2}\\, the expected population kurtosis of the values in
`x` using
[`kurtosis()`](https://mark-eis.github.io/BitsnBobs/reference/kurtosis.md),
performs a `t`-test of its significance and calculates a confidence
interval.

## Usage

``` r
kurtosis.test(
  x,
  alternative = c("two.sided", "less", "greater"),
  se_method = c("Cramer", "simple"),
  conf.level = 0.95
)

kurt.test(
  x,
  alternative = c("two.sided", "less", "greater"),
  se_method = c("Cramer", "simple"),
  conf.level = 0.95
)

stderr_kurtosis(n, se_method = c("Cramer", "simple"))
```

## Arguments

- x:

  a `numeric` vector.

- alternative:

  a character string specifying the alternative hypothesis, must be one
  of `"two.sided"` (default), `"greater"` or `"less"`. You can specify
  just the initial letter.

- se_method:

  a `character` string specifying the method of calculating the standard
  error; must be one of `"Cramer"` (default), or `"simple"`. You can
  specify just the initial letter.

- conf.level:

  the confidence level required; default `0.95`.

- n:

  an `integer`, the number of observations.

## Value

A list with class `"htest"` containing the following components: -

- statistic:

  the value of the t-statistic.

- parameter:

  the degrees of freedom for the t-statistic.

- p.value:

  the p-value for the test.

- conf.int:

  confidence interval of the kurtosis (95% or other specified level).

- estimate:

  the estimate of kurtosis.

- alternative:

  a character string describing the alternative hypothesis.

- method:

  the character string "Kurtosis with t-test" and the standard error
  method used.

- data.name:

  a character string giving the name of the data.

## Details

The `t`-statistic is given by the estimated population
[`kurtosis`](https://mark-eis.github.io/BitsnBobs/reference/kurtosis.md),
\\G\_{2}\\, divided by its standard error, \\SE\_{G\_{2}}\\, where: -

\$\$SE\_{G\_{2}} = \displaystyle 2(SE\_{G\_{1}}) \sqrt{\frac{(n^{2} -
1)}{(n-3)(n+5)}}\$\$

(see e.g., Joanes and Gill, 1998; Wright and Herrington 2011), or
alternatively its approximation, \\\sqrt (24 / n_x)\\, and the
associated probability is derived from the `t`-distribution with
\\n\_{x}-2\\ degrees of freedom. The `t`-test is conducted according to
Crawley (2012), except that the default here is a two-tailed test. The
corresponding confidence interval is calculated similarly from the
quantiles of the `t`-distribution using both the `alternative` and
`conf.level` arguments.

`kurt.test()` is an alias for `kurtosis.test()`.

## Note

The confidence interval is poorly described in the available literature,
seems somewhat controversial and should be used with caution.

## References

Crawley, Michael J. (2012) *The R Book*. John Wiley & Sons,
Incorporated. ISBN:9780470973929. p.350-352.
[](https://onlinelibrary.wiley.com/doi/book/10.1002/9781118448908)[doi:10.1002/9781118448908](https://doi.org/10.1002/9781118448908)

Joanes, D.N., and Gill, C.A. (1998). Comparing measures of sample
skewness and kurtosis. *Journal of the Royal Statistical Society. Series
D (The Statistician)* **47**(1): 183–189.
[](https://doi.org/10.1111/1467-9884.00122)[doi:10.1111/1467-9884.00122](https://doi.org/10.1111/1467-9884.00122)

Wright, D.B., and Herrington, J.A. (2011). Problematic standard errors
and confidence intervals for skewness and kurtosis. *Behavior Research
Methods* **43**(1): 8-17.
[](https://doi.org/10.3758/s13428-010-0044-x)[doi:10.3758/s13428-010-0044-x](https://doi.org/10.3758/s13428-010-0044-x)

## See also

[`distributions`](https://rdrr.io/r/stats/Distributions.html),
[`TDist`](https://rdrr.io/r/stats/TDist.html),
[`t.test`](https://rdrr.io/r/stats/t.test.html)

Other skewness:
[`kurtosis()`](https://mark-eis.github.io/BitsnBobs/reference/kurtosis.md),
[`skewness()`](https://mark-eis.github.io/BitsnBobs/reference/skewness.md),
[`skewness.test()`](https://mark-eis.github.io/BitsnBobs/reference/skewness.test.md)

## Examples

``` r
 ## Heights of 100 randomly selected male university students, adapted from Spiegel and Stephens
 ## (Theory and Problems of Statistics. 4th edn. McGraw-Hill. 1999. ISBN 9780071755498).
 table(heights)
#> heights
#> 61 64 67 70 73 
#>  5 18 42 27  8 
 kurtosis.test(heights) 
#> 
#>  Kurtosis with t-test (Cramer stderr)
#> 
#> data:  heights
#> t = -0.43724, df = 98, p-value = 0.6629
#> alternative hypothesis: true kurtosis is not equal to 0
#> 95 percent confidence interval:
#>  -1.1583796  0.7400855
#> sample estimates:
#>   kurtosis 
#> -0.2091471 
#> 
 length(heights) |> stderr_kurtosis()
#> [1] 0.4783311
 kurtosis.test(heights, se_method = "simple")
#> 
#>  Kurtosis with t-test (simple stderr)
#> 
#> data:  heights
#> t = -0.42692, df = 98, p-value = 0.6704
#> alternative hypothesis: true kurtosis is not equal to 0
#> 95 percent confidence interval:
#>  -1.1813336  0.7630395
#> sample estimates:
#>   kurtosis 
#> -0.2091471 
#> 
 length(heights) |> stderr_kurtosis(se_method = "simple")
#> [1] 0.4898979

 ## Litter sizes in albino rats (n = 815), data from King (1924; Litter production and
 ## the sex ratio in various strains of rats. The Anatomical Record 27(5), 337-366).
 table(litter_sizes)
#> litter_sizes
#>   1   2   3   4   5   6   7   8   9  10  11  12 
#>   7  33  58 116 125 126 121 107  56  37  25   4 
 kurtosis.test(litter_sizes) 
#> 
#>  Kurtosis with t-test (Cramer stderr)
#> 
#> data:  litter_sizes
#> t = -2.7832, df = 813, p-value = 0.005507
#> alternative hypothesis: true kurtosis is not equal to 0
#> 95 percent confidence interval:
#>  -0.8119700 -0.1403447
#> sample estimates:
#>   kurtosis 
#> -0.4761573 
#> 
 length(litter_sizes) |> stderr_kurtosis()
#> [1] 0.1710811
```
