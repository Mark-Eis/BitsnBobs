# Confidence Interval and t-Test from Pearson's Correlation Coefficient

This function performs a *t*-test for a given Pearson's product-moment
correlation coefficient `r` derived from a sample of size `n` from a
bivariate normal population, and calculates the population confidence
interval.

## Usage

``` r
cor_coef.test(
  r,
  n,
  alternative = c("two.sided", "less", "greater"),
  conf.level = 0.95
)
```

## Arguments

- r:

  numeric providing the value of Pearson's product-moment correlation
  coefficient `r`.

- n:

  integer providing the number of pairs of observations from which `r`
  was derived; minimum 4.

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

  the degrees of freedom for the test statistic.

- p.value:

  the p-value of the test.

- conf.int:

  confidence interval of the correlation coefficient (95% or other
  specified level).

- estimate:

  the correlation coefficient as provided in `r`.

- null.value:

  the value of the association measure under the null hypothesis, always
  0.

- alternative:

  a character string describing the alternative hypothesis.

- method:

  the character string "Pearson's product-moment correlation".

- data.name:

  a character string giving the name of the data.

## Details

The t-statistic is given by the correlation coefficient `r` divided by
its standard error calculated using: -

\$\$SE = \displaystyle \sqrt{\frac{1 - r^2}{df}}\$\$

To calculate the confidence interval, firstly a value \\Z_r\\ is
calculated from `r` using the Fisher transformation (inverse hyperbolic
tangent): -

\$\$Z_r = \displaystyle \frac{1}{2}log_e\left(\frac{1 + r}{1 -
r}\right)\$\$

Log upper and lower bounds (`L` and `U`) are then calculated using: -

\$\$L = \displaystyle Z_r - \frac{Z\_{(1 - alpha/2)}}{\sqrt{n - 3}}\$\$

\$\$U = \displaystyle Z_r + \frac{Z\_{(1 - alpha/2)}}{\sqrt{n - 3}}\$\$

The confidence interval is then calculated using the hyperbolic tangent:
-

\$\$\displaystyle lower = \frac{e^{2L} - 1}{e^{2L} + 1}, upper =
\frac{e^{2U} - 1}{e^{2U} + 1}\$\$

## Note

This function is much like `cor.test` and potentially useful if the
correlation coefficient is available but not the original data.

## See also

[`cor`](https://rdrr.io/r/stats/cor.html),
[`cor.test`](https://rdrr.io/r/stats/cor.test.html),
[`t.test`](https://rdrr.io/r/stats/t.test.html)

Other correl_coef:
[`phi_coef()`](https://mark-eis.github.io/BitsnBobs/reference/phi_coef.md),
[`phi_coef.test()`](https://mark-eis.github.io/BitsnBobs/reference/phi_coef.test.md)

## Examples

``` r
## Data from cor.test() example (Hollander & Wolfe, 1973): -
 x <- c(44.4, 45.9, 41.9, 53.3, 44.7, 44.1, 50.7, 45.2, 60.1)
 y <- c( 2.6,  3.1,  2.5,  5.0,  3.6,  4.0,  5.2,  2.8,  3.8)

 (corxy <- cor(x, y))
#> [1] 0.5711816

 cor_coef.test(corxy, 9)  ## alternative two-sided by default
#> 
#>  Pearson's product-moment correlation
#> 
#> data:  corxy
#> t = 1.8411, df = 7, p-value = 0.1082
#> alternative hypothesis: true correlation is not equal to 0
#> 95 percent confidence interval:
#>  -0.1497426  0.8955795
#> sample estimates:
#>       cor 
#> 0.5711816 
#> 
 cor.test(x, y)  ## Result should be identical
#> 
#>  Pearson's product-moment correlation
#> 
#> data:  x and y
#> t = 1.8411, df = 7, p-value = 0.1082
#> alternative hypothesis: true correlation is not equal to 0
#> 95 percent confidence interval:
#>  -0.1497426  0.8955795
#> sample estimates:
#>       cor 
#> 0.5711816 
#> 

 cor_coef.test(corxy, 9, alternative = "less")
#> 
#>  Pearson's product-moment correlation
#> 
#> data:  corxy
#> t = 1.8411, df = 7, p-value = 0.9459
#> alternative hypothesis: true correlation is less than 0
#> 95 percent confidence interval:
#>  -1.0000000  0.8669786
#> sample estimates:
#>       cor 
#> 0.5711816 
#> 
 cor.test(x, y, alternative = "less")  ## Result should be identical
#> 
#>  Pearson's product-moment correlation
#> 
#> data:  x and y
#> t = 1.8411, df = 7, p-value = 0.9459
#> alternative hypothesis: true correlation is less than 0
#> 95 percent confidence interval:
#>  -1.0000000  0.8669786
#> sample estimates:
#>       cor 
#> 0.5711816 
#> 

 cor_coef.test(corxy, 9, alternative = "greater")
#> 
#>  Pearson's product-moment correlation
#> 
#> data:  corxy
#> t = 1.8411, df = 7, p-value = 0.05409
#> alternative hypothesis: true correlation is greater than 0
#> 95 percent confidence interval:
#>  -0.02223023  1.00000000
#> sample estimates:
#>       cor 
#> 0.5711816 
#> 
 cor.test(x, y, alternative = "greater")  ## Result should be identical
#> 
#>  Pearson's product-moment correlation
#> 
#> data:  x and y
#> t = 1.8411, df = 7, p-value = 0.05409
#> alternative hypothesis: true correlation is greater than 0
#> 95 percent confidence interval:
#>  -0.02223023  1.00000000
#> sample estimates:
#>       cor 
#> 0.5711816 
#> 

 rm(corxy, x, y)
```
