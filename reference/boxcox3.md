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
#>  [1] 0.24658623 1.29087083 0.08739968 0.99444420 1.86181668 3.15318044
#>  [7] 0.16173151 0.78088663 0.78333126 0.75374177 0.57481939 1.87570022
#> [13] 7.88549421 0.19573582 1.66933768 0.15520453 0.59332527 0.94875763
#> [19] 1.72115632 0.40088734
## Calculate skewness using BitsnBobs::skew()
d |> skew()
#> [1] 3.16996
## Box-Cox function for these data
bc_func <- boxcox3(d)

## Box-Cox transform data with various values of lambda
bc_func(-1)
#>  [1]  -3.055376440   0.225329154 -10.441688958  -0.005586835   0.462890192
#>  [6]   0.682859887  -5.183086998  -0.280595625  -0.276599123  -0.326714319
#> [11]  -0.739676861   0.466865767   0.873184867  -4.108927002   0.400960027
#> [16]  -5.443110962  -0.685416164  -0.054009971   0.418995248  -1.494466372
bc_func(0)
#>  [1] -1.400043517  0.255317055 -2.437263611 -0.005571287  0.621552721
#>  [6]  1.148411606 -1.821817661 -0.247325302 -0.244199607 -0.282705449
#> [11] -0.553699384  0.628982042  2.065024895 -1.630989402  0.512426950
#> [16] -1.863011492 -0.522012515 -0.052601910  0.542996343 -0.914074827
bc_func(1)
#>  [1] -0.753413767  0.290870833 -0.912600316 -0.005555796  0.861816681
#>  [6]  2.153180437 -0.838268489 -0.219113372 -0.216668740 -0.246258229
#> [11] -0.425180605  0.875700223  6.885494207 -0.804264183  0.669337681
#> [16] -0.844795471 -0.406674730 -0.051242372  0.721156318 -0.599112655
bc_func(2)
#>  [1] -0.469597615  0.333173754 -0.496180648 -0.005540362  1.233180677
#>  [6]  4.471273434 -0.486921459 -0.195108037 -0.193196068 -0.215936672
#> [11] -0.334791332  1.259125664 30.590509448 -0.480843745  0.893344147
#> [16] -0.487955777 -0.323982562 -0.049929481  0.981189535 -0.419644668
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
#>  [1] -2.189835e+01  1.783695e-01 -4.989524e+02 -5.618106e-03  2.816836e-01
#>  [6]  3.227009e-01 -7.846097e+01 -3.666937e-01 -3.601601e-01 -4.450813e-01
#> [11] -1.421697e+00  2.828220e-01  3.326535e-01 -4.411626e+01  2.616784e-01
#> [16] -8.882575e+01 -1.262547e+00 -5.697956e-02  2.679574e-01 -4.840491e+00
#> 
#> $`lambda -2`
#>  [1]  -7.723039034   0.199942540 -64.956123103  -0.005602442   0.355756527
#>  [6]   0.449711074 -18.615282412  -0.319962578  -0.314852660  -0.380085442
#> [11]  -1.013237790   0.357883945   0.491958961 -12.550567555   0.320575555
#> [16] -20.256839433  -0.920313822  -0.055468509   0.331216739  -2.611181240
#> 
#> $`lambda -1`
#>  [1]  -3.055376440   0.225329154 -10.441688958  -0.005586835   0.462890192
#>  [6]   0.682859887  -5.183086998  -0.280595625  -0.276599123  -0.326714319
#> [11]  -0.739676861   0.466865767   0.873184867  -4.108927002   0.400960027
#> [16]  -5.443110962  -0.685416164  -0.054009971   0.418995248  -1.494466372
#> 
#> $`lambda 0`
#>  [1] -1.400043517  0.255317055 -2.437263611 -0.005571287  0.621552721
#>  [6]  1.148411606 -1.821817661 -0.247325302 -0.244199607 -0.282705449
#> [11] -0.553699384  0.628982042  2.065024895 -1.630989402  0.512426950
#> [16] -1.863011492 -0.522012515 -0.052601910  0.542996343 -0.914074827
#> 
#> $`lambda 1`
#>  [1] -0.753413767  0.290870833 -0.912600316 -0.005555796  0.861816681
#>  [6]  2.153180437 -0.838268489 -0.219113372 -0.216668740 -0.246258229
#> [11] -0.425180605  0.875700223  6.885494207 -0.804264183  0.669337681
#> [16] -0.844795471 -0.406674730 -0.051242372  0.721156318 -0.599112655
#> 
#> $`lambda 2`
#>  [1] -0.469597615  0.333173754 -0.496180648 -0.005540362  1.233180677
#>  [6]  4.471273434 -0.486921459 -0.195108037 -0.193196068 -0.215936672
#> [11] -0.334791332  1.259125664 30.590509448 -0.480843745  0.893344147
#> [16] -0.487955777 -0.323982562 -0.049929481  0.981189535 -0.419644668
#> 
#> $`lambda 3`
#>  [1]  -0.328335460   0.383679798  -0.333110793  -0.005524986   1.817909798
#>  [6]  10.116881427  -0.331923192  -0.174609295  -0.173113926  -0.190593069
#> [11]  -0.270023236   1.866394933 163.109354770  -0.330833623   1.217307926
#> [16]  -0.332087121  -0.263709604  -0.048661441   1.366239150  -0.311857710
#> 
#> 
#>   lambda -3   lambda -2   lambda -1    lambda 0    lambda 1    lambda 2 
#> -4.08779986 -3.36358910 -2.00565188 -0.04650773  3.16995968  4.27696463 
#>    lambda 3 
#>  4.44244689 
#> 
#> lambda 0 
#>        4 

## Usually, lambda 0 has least absolute skewness as data were sampled from lognormal distribution

rm(d, bc_func)
```
