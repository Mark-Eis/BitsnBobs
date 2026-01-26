# Test of Skewness

Computes \\G\_{1}\\, the expected population skewness of the values in
`x` using
[`skewness()`](https://mark-eis.github.io/BitsnBobs/reference/skewness.md),
performs a `t`-test of its significance and calculates a confidence
interval.

## Usage

``` r
skewness.test(
  x,
  alternative = c("two.sided", "less", "greater"),
  se_method = c("Cramer", "simple"),
  conf.level = 0.95
)

skew.test(
  x,
  alternative = c("two.sided", "less", "greater"),
  se_method = c("Cramer", "simple"),
  conf.level = 0.95
)

stderr_skewness(n, se_method = c("Cramer", "simple"))
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

  confidence interval of the skewness (95% or other specified level).

- estimate:

  the estimate of skewness.

- alternative:

  a character string describing the alternative hypothesis.

- method:

  the character string "Skewness with t-test" and the standard error
  method used.

- data.name:

  a character string giving the name of the data.

## Details

The `t`-statistic is given by the estimated population
[`skewness`](https://mark-eis.github.io/BitsnBobs/reference/skewness.md),
\\G\_{1}\\, divided by its standard error, \\SE\_{G\_{1}}\\, where: -

\$\$SE\_{G\_{1}} = \displaystyle \sqrt{\frac{6n(n -
1)}{(n-2)(n+1)(n+3)}}\$\$

(see e.g., Joanes and Gill, 1998; Wright and Herrington 2011), or
alternatively its approximation, \\\sqrt (6 / n_x)\\, and the associated
probability is derived from the `t`-distribution with \\n\_{x}-2\\
degrees of freedom. The `t`-test is conducted according to Crawley
(2012), except that the default here is a two-tailed test. The
corresponding confidence interval is calculated similarly from the
quantiles of the `t`-distribution using both the `alternative` and
`conf.level` arguments.

`skew.test()` is an alias for `skewness.test()`.

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
[`kurtosis.test()`](https://mark-eis.github.io/BitsnBobs/reference/kurtosis.test.md),
[`skewness()`](https://mark-eis.github.io/BitsnBobs/reference/skewness.md)

## Examples

``` r
 ## Heights of 100 randomly selected male university students, adapted from Spiegel and Stephens
 ## (Theory and Problems of Statistics. 4th edn. McGraw-Hill. 1999. ISBN 9780071755498).
 table(heights)
#> heights
#> 61 64 67 70 73 
#>  5 18 42 27  8 
 skewness.test(heights) 
#> 
#>  Skewness with t-test (Cramer stderr)
#> 
#> data:  heights
#> t = -0.45492, df = 98, p-value = 0.6502
#> alternative hypothesis: true skewness is not equal to 0
#> 95 percent confidence interval:
#>  -0.5888187  0.3692019
#> sample estimates:
#>   skewness 
#> -0.1098084 
#> 
 length(heights) |> stderr_skewness()
#> [1] 0.2413798
 skewness.test(heights, se_method = "simple")
#> 
#>  Skewness with t-test (simple stderr)
#> 
#> data:  heights
#> t = -0.44829, df = 98, p-value = 0.6549
#> alternative hypothesis: true skewness is not equal to 0
#> 95 percent confidence interval:
#>  -0.5959017  0.3762849
#> sample estimates:
#>   skewness 
#> -0.1098084 
#> 
 length(heights) |> stderr_skewness(se_method = "simple")
#> [1] 0.244949

 ## Litter sizes in albino rats (n = 815), data from King (1924; Litter production and
 ## the sex ratio in various strains of rats. The Anatomical Record 27(5), 337-366).
 table(litter_sizes)
#> litter_sizes
#>   1   2   3   4   5   6   7   8   9  10  11  12 
#>   7  33  58 116 125 126 121 107  56  37  25   4 
 skewness.test(litter_sizes) 
#> 
#>  Skewness with t-test (Cramer stderr)
#> 
#> data:  litter_sizes
#> t = 2.0204, df = 813, p-value = 0.04367
#> alternative hypothesis: true skewness is not equal to 0
#> 95 percent confidence interval:
#>  0.004927511 0.341148438
#> sample estimates:
#> skewness 
#> 0.173038 
#> 
 length(litter_sizes) |> stderr_skewness()
#> [1] 0.08564453

 ## Compare a range of distributions, each with the three possible alternative hypotheses
 list(
   uniform = runif(30),
   normal = rnorm(30),
   lognormal = rlnorm(30),
   poisson = rpois(30, lambda = 10),
   negbinom = rnbinom(30, mu = 4, size = 2)
 ) |>
 lapply(\(distrib)
     c("less", "two.sided","greater") |>
     setNames(nm = _) |>
     lapply(\(altern)
         with(skewness.test(distrib, altern),
             data.frame(
                 Lower = conf.int[1],
                 Upper = conf.int[2],
                 Skewness = estimate,
                 t = statistic,
                 df = parameter,
                 p = p.value,
                 sig = starsig(p.value),
                 row.names = NULL
             )
         )
     ) |>
     bind_rows(.id = "Alternative")) |>
 bind_rows(.id = "Distribution")
#>    Distribution Alternative      Lower     Upper    Skewness          t df
#> 1       uniform        less       -Inf 0.7782622  0.05206233  0.1219566 28
#> 2       uniform   two.sided -0.8223871 0.9265118  0.05206233  0.1219566 28
#> 3       uniform     greater -0.6741375       Inf  0.05206233  0.1219566 28
#> 4        normal        less       -Inf 1.0065073  0.28030746  0.6566232 28
#> 5        normal   two.sided -0.5941420 1.1547569  0.28030746  0.6566232 28
#> 6        normal     greater -0.4458924       Inf  0.28030746  0.6566232 28
#> 7     lognormal        less       -Inf 2.1929005  1.46670062  3.4357619 28
#> 8     lognormal   two.sided  0.5922512 2.3411501  1.46670062  3.4357619 28
#> 9     lognormal     greater  0.7405008       Inf  1.46670062  3.4357619 28
#> 10      poisson        less       -Inf 0.4917910 -0.23440887 -0.5491053 28
#> 11      poisson   two.sided -1.1088583 0.6400406 -0.23440887 -0.5491053 28
#> 12      poisson     greater -0.9606087       Inf -0.23440887 -0.5491053 28
#> 13     negbinom        less       -Inf 1.9559176  1.22971770  2.8806268 28
#> 14     negbinom   two.sided  0.3552683 2.1041671  1.22971770  2.8806268 28
#> 15     negbinom     greater  0.5035178       Inf  1.22971770  2.8806268 28
#>               p sig
#> 1  0.5480977460  NS
#> 2  0.9038045080  NS
#> 3  0.4519022540  NS
#> 4  0.7416080086  NS
#> 5  0.5167839828  NS
#> 6  0.2583919914  NS
#> 7  0.9990688501  NS
#> 8  0.0018622998  **
#> 9  0.0009311499 ***
#> 10 0.2936429884  NS
#> 11 0.5872859768  NS
#> 12 0.7063570116  NS
#> 13 0.9962347039  NS
#> 14 0.0075305922  **
#> 15 0.0037652961  **
```
