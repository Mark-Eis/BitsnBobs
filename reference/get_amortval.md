# Individual Values from the Output of amort.period or amort.table()

Functions to return individual values from the output of
`amort.period()` or `amort.table()` in package
[FinancialMath](https://CRAN.R-project.org/package=FinancialMath).

## Usage

``` r
get_amortval(x, val = "Eff Rate")

get_amortint(x, until)
```

## Arguments

- x:

  either a matrix of input variables, calculated unknown variables, and
  amortization figures for a given period, as output by `amort.period()`
  or a list of two components, `Schedule` (a data frame of an
  amortization schedule) and `Other`, (a matrix of the input variables
  and other calculated variables) as output by `amort.table()`.

- val:

  a `character` string giving the exact name of the value to be
  returned.

- until:

  a `numeric` signifying the period up to and including which total
  interest should be summated.

## Value

A numeric value.

## Details

`get_amortval()` returns individual values from the output of
[`amort.period()`](https://rdrr.io/pkg/FinancialMath/man/amort.period.html)
or the `Other` list element of the output of
[`amort.table()`](https://rdrr.io/pkg/FinancialMath/man/amort.table.html)
in package
[FinancialMath](https://CRAN.R-project.org/package=FinancialMath).

`get_amortint()` returns the total value of interest payments from the
`Schedule` list element of the output of
[`amort.table()`](https://rdrr.io/pkg/FinancialMath/man/amort.table.html),
up to and including the period given in `until`.

## See also

[`amort.period()`](https://rdrr.io/pkg/FinancialMath/man/amort.period.html),
[`amort.table()`](https://rdrr.io/pkg/FinancialMath/man/amort.table.html)

Other amort:
[`eff_rate()`](https://mark-eis.github.io/BitsnBobs/reference/eff_rate.md),
[`j()`](https://mark-eis.github.io/BitsnBobs/reference/j.md)

## Examples

``` r
(apd <- try(
    FinancialMath::amort.period(Loan = 200099, n = 15 * 12, i = 0.0184, ic = 12, pf = 12, t = 60)
))
#>             Amortization
#> Loan        2.000990e+05
#> PMT         1.272965e+03
#> Eff Rate    1.855600e-02
#> i^(12)      1.840000e-02
#> Periods     1.800000e+02
#> Years       1.500000e+01
#> At Time 60: 6.000000e+01
#> Int Paid    2.154122e+02
#> Princ Paid  1.057553e+03
#> Balance     1.394286e+05

## Default value is "Eff Rate"
get_amortval(apd)
#> [1] 0.018556
get_amortval(apd, "PMT")  
#> [1] 1272.965
get_amortval(apd, "Balance")  
#> [1] 139428.6

atb <- try(FinancialMath::amort.table(Loan = 200099, n = 15 * 12, i = 0.0184, ic = 12, pf = 12))
atb$Other
#>                     Details
#> Loan           2.000990e+05
#> Total Paid     2.291337e+05
#> Total Interest 2.903468e+04
#> Eff Rate       1.855597e-02
#> i^(12)         1.840000e-02

## Default value is "Eff Rate"
get_amortval(atb)
#> [1] 0.01855597
get_amortval(atb, "Total Paid")
#> [1] 229133.7
get_amortval(atb, "Total Interest")
#> [1] 29034.68

get_amortint(atb, 60)
#> [1] 15707.5

rm(apd, atb)
```
