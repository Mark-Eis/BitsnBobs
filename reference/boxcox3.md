# 'Function Factory' for Box-Cox Transformation of Data

Function factory to create functions that take \\\lambda\\ as an
argument for performing the Box-Cox transformation on a given dataset.
Adapted from Wickham (2019) [Section 10.4.4
Exercises](https://adv-r.hadley.nz/function-factories.html#exercises-34).

## Usage

``` r
boxcox3(x, labile_data = TRUE)
```

## Arguments

- x:

  a `numeric vector` containing the data to be transformed.

- labile_data:

  `logical`. If `TRUE`, `data` are represented in the function
  environment as a `quosure`. If `FALSE`, a copy of `data` is saved in
  the function environment. Default `TRUE`.

## Value

Returns a [`function`](https://rdrr.io/r/base/function.html) taking a
single argument \\\lambda\\ that performs the Box-Cox transformation on
data `x`.

## Details

A numeric vector containing the data to be transformed is provided as an
argument to this 'function factory', which returns a function performing
the Box-Cox transformation on those data for any given value of
\\\lambda\\. The Box-Cox transformation takes the following form: -

if \\\lambda \ne 0\\ \$\$y(\lambda) = \displaystyle \frac{y^\lambda -
1}{\lambda}\$\$

if \\\lambda = 0\\ \$\$y(\lambda) = \log(y)\$\$

If `labile_data` is `TRUE`, `data` are represented in the `boxcox3()`
function environment as a
[`quosure`](https://rlang.r-lib.org/reference/topic-quosure.html), and
functions returned by will automatically refer to the current version of
`data` in its original
[`environment`](https://rdrr.io/r/base/environment.html), usually the
calling environment i.e., typically but not necessarily the global
environment. If `labile_data` is `FALSE`, returned functions refer to a
copy of `data` saved in the function environment at the time of
execution of `boxcox3()`, and will not reflect any subsequent changes to
the original `data`.

## References

Wickham, Hadley (2019) *Advanced R 2nd edition*. CRC Press.
[adv-r.hadley.nz](https://adv-r.hadley.nz/index.html)

## See also

Other boxcox:
[`opt_bc()`](https://mark-eis.github.io/BitsnBobs/reference/opt_bc.md)

## Examples

``` r
## Create skewed data
(d <- rlnorm(20))
#>  [1] 3.1531804 0.1617315 0.7808866 0.7833313 0.7537418 0.5748194 1.8757002
#>  [8] 7.8854942 0.1957358 1.6693377 0.1552045 0.5933253 0.9487576 1.7211563
#> [15] 0.4008873 1.5970440 1.4375658 0.2712963 2.0912800 6.6094797
## Calculate skewness using BitsnBobs::skew()
d |> skew()
#> [1] 2.259433
## Box-Cox function for these data
bc_func <- boxcox3(d)

## Box-Cox transform data with various values of lambda
bc_func(-1)
#>  [1]  0.68285989 -5.18308700 -0.28059563 -0.27659912 -0.32671432 -0.73967686
#>  [7]  0.46686577  0.87318487 -4.10892700  0.40096003 -5.44311096 -0.68541616
#> [13] -0.05400997  0.41899525 -1.49446637  0.37384318  0.30437966 -2.68600621
#> [19]  0.52182396  0.84870216
bc_func(0)
#>  [1]  1.14841161 -1.82181766 -0.24732530 -0.24419961 -0.28270545 -0.55369938
#>  [7]  0.62898204  2.06502490 -1.63098940  0.51242695 -1.86301149 -0.52201251
#> [13] -0.05260191  0.54299634 -0.91407483  0.46815442  0.36295126 -1.30454355
#> [19]  0.73777632  1.88850493
bc_func(1)
#>  [1]  2.15318044 -0.83826849 -0.21911337 -0.21666874 -0.24625823 -0.42518061
#>  [7]  0.87570022  6.88549421 -0.80426418  0.66933768 -0.84479547 -0.40667473
#> [13] -0.05124237  0.72115632 -0.59911266  0.59704400  0.43756578 -0.72870366
#> [19]  1.09128001  5.60947965
bc_func(2)
#>  [1]  4.47127343 -0.48692146 -0.19510804 -0.19319607 -0.21593667 -0.33479133
#>  [7]  1.25912566 30.59050945 -0.48084374  0.89334415 -0.48795578 -0.32398256
#> [13] -0.04992948  0.98118953 -0.41964467  0.77527477  0.53329769 -0.46319915
#> [19]  1.68672603 21.34261063
## bc_func(0) same as log(d)
identical(bc_func(0), log(d))
#> [1] TRUE

seq(-3, 3, 1) |>                         ## Create a sequence from -3 to 3
  set_names(\(x) paste("lambda", x)) |>  ## Name sequence vector using rlang::set_names()
  print_lf() |>                          ## Print with line feed
  lapply(bc_func) |>                     ## Box-Cox transform data using each lambda value
  print_lf() |>                          ##   in sequence and print the named list
  map_dbl(skewness) |>                   ## Calculate skewness for each element of the list
  print_lf() |>                          ##   and print the numeric vector
  abs() |>                               ## Absolute skewness...
  which.min()                            ##   ...which lambda gives minimum?
#> lambda -3 lambda -2 lambda -1  lambda 0  lambda 1  lambda 2  lambda 3 
#>        -3        -2        -1         0         1         2         3 
#> 
#> $`lambda -3`
#>  [1]   0.32270091 -78.46096943  -0.36669366  -0.36016013  -0.44508131
#>  [6]  -1.42169651   0.28282204   0.33265352 -44.11626465   0.26167839
#> [11] -88.82574645  -1.26254658  -0.05697956   0.26795742  -4.84049132
#> [16]   0.25150040   0.22113263 -16.36014871   0.29688798   0.33217888
#> 
#> $`lambda -2`
#>  [1]   0.44971107 -18.61528241  -0.31996258  -0.31485266  -0.38008544
#>  [6]  -1.01323779   0.35788394   0.49195896 -12.55056755   0.32057556
#> [11] -20.25683943  -0.92031382  -0.05546851   0.33121674  -2.61118124
#> [16]   0.30396382   0.25805617  -6.29332090   0.38567384   0.48855448
#> 
#> $`lambda -1`
#>  [1]  0.68285989 -5.18308700 -0.28059563 -0.27659912 -0.32671432 -0.73967686
#>  [7]  0.46686577  0.87318487 -4.10892700  0.40096003 -5.44311096 -0.68541616
#> [13] -0.05400997  0.41899525 -1.49446637  0.37384318  0.30437966 -2.68600621
#> [19]  0.52182396  0.84870216
#> 
#> $`lambda 0`
#>  [1]  1.14841161 -1.82181766 -0.24732530 -0.24419961 -0.28270545 -0.55369938
#>  [7]  0.62898204  2.06502490 -1.63098940  0.51242695 -1.86301149 -0.52201251
#> [13] -0.05260191  0.54299634 -0.91407483  0.46815442  0.36295126 -1.30454355
#> [19]  0.73777632  1.88850493
#> 
#> $`lambda 1`
#>  [1]  2.15318044 -0.83826849 -0.21911337 -0.21666874 -0.24625823 -0.42518061
#>  [7]  0.87570022  6.88549421 -0.80426418  0.66933768 -0.84479547 -0.40667473
#> [13] -0.05124237  0.72115632 -0.59911266  0.59704400  0.43756578 -0.72870366
#> [19]  1.09128001  5.60947965
#> 
#> $`lambda 2`
#>  [1]  4.47127343 -0.48692146 -0.19510804 -0.19319607 -0.21593667 -0.33479133
#>  [7]  1.25912566 30.59050945 -0.48084374  0.89334415 -0.48795578 -0.32398256
#> [13] -0.04992948  0.98118953 -0.41964467  0.77527477  0.53329769 -0.46319915
#> [19]  1.68672603 21.34261063
#> 
#> $`lambda 3`
#>  [1]  10.11688143  -0.33192319  -0.17460930  -0.17311393  -0.19059307
#>  [6]  -0.27002324   1.86639493 163.10935477  -0.33083362   1.21730793
#> [11]  -0.33208712  -0.26370960  -0.04866144   1.36623915  -0.31185771
#> [16]   1.02444661   0.65695561  -0.32667738   2.71537095  95.91219364
#> 
#> 
#>   lambda -3   lambda -2   lambda -1    lambda 0    lambda 1    lambda 2 
#> -2.35360901 -2.07423564 -1.52537771  0.07559137  2.25943296  2.96348609 
#>    lambda 3 
#>  3.23187437 
#> 
#> lambda 0 
#>        4 

## Usually, lambda 0 has least absolute skewness as data were sampled from lognormal distribution

rm(d, bc_func)
```
