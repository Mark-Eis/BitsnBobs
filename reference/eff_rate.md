# Effective Rate of Interest from a Nominal Rate and its Conversion Frequency or the Inverse

`eff_rate()` calculates the effective rate of annual interest from a
nominal rate and its interest conversion frequency.

`nom_rate()` calculates the nominal rate of interest for a given
interest conversion frequency from an effective rate.

`cc_rate()` calculates the continuous compounding rate of interest from
an effective rate.

## Usage

``` r
eff_rate(nom_rate, con_fq)

nom_rate(eff_rate, con_fq)

cc_rate(eff_rate)
```

## Arguments

- nom_rate:

  numeric, the nominal interest rate convertible `con_fq` times per
  year.

- con_fq, :

  integer, the interest conversion frequency per year.

- eff_rate:

  numeric, the effective rate of interest.

## Value

A numeric: `eff_rate()` returns the effective rate of interest;
`nom_rate()` returns the nominal interest rate convertible `con_fq`
times per year.

## Details

Effective Rate of Interest (er): —

\$\$er = \displaystyle \left(1 + \frac{nr}{ic} \right)^{ic} - 1\$\$

Nominal Rate of Interest (nr): —

\$\$nr = \displaystyle \left(\left(1 + er \right)^{\frac{1}{ic}} - 1
\right).ic\$\$

Continuous Compounding Rate of Interest (ccr): —

\$\$ccr = \displaystyle e^{er} - 1\$\$

where \\er\\ is the effective rate of interest, \\nr\\ is the nominal
rate of interest, and \\ic\\ is the interest conversion frequency. See
[`amort.period()`](https://rdrr.io/pkg/FinancialMath/man/amort.period.html)
in package
[FinancialMath](https://CRAN.R-project.org/package=FinancialMath) for
further details.

## References

Investopedia: Effective Annual Interest Rate: Definition, Formula, and
Example.
[www.investopedia.com/terms/e/effectiveinterest](https://www.investopedia.com/terms/e/effectiveinterest.asp)

Investopedia: Continuous Compounding Definition and Formula.
[www.investopedia.com/terms/c/continuouscompounding](https://www.investopedia.com/terms/c/continuouscompounding.asp)

## See also

[`amort.period()`](https://rdrr.io/pkg/FinancialMath/man/amort.period.html),
[`amort.table()`](https://rdrr.io/pkg/FinancialMath/man/amort.table.html)

Other amort:
[`get_amortval()`](https://mark-eis.github.io/BitsnBobs/reference/get_amortval.md),
[`j()`](https://mark-eis.github.io/BitsnBobs/reference/j.md)

## Examples

``` r
## A lender calculates interest daily using a simple annual rate of 5.7480%,
## based on a 365-day year.

## Calculate effective rate: -
eff_rate(0.057480, 365)
#> [1] 0.05915929

## Calculate nominal rates at monthly and daily interest conversion frequencies: -
## Monthly
nom_rate(0.05915929, 12)
#> [1] 0.05761333
## Daily—same as the original simple annual rate
nom_rate(0.05915929, 365)
#> [1] 0.05748

## Example from https://www.investopedia.com/terms/e/effectiveinterest.asp
eff_rate(0.10, c(2, 4, 12, 365)) |>
    setNames(c("Semiannual", "Quarterly", "Monthly", "Daily"))
#> Semiannual  Quarterly    Monthly      Daily 
#>  0.1025000  0.1038129  0.1047131  0.1051558 

## Limit of compounding (ibid.)
eff_rate(0.1, c(365, 365 * 24, 365 * 24 * 60, 365 * 24 * 60 * 60)) |>
    setNames(c("Daily", "Hourly", "Minutely", "Secondly"))
#>     Daily    Hourly  Minutely  Secondly 
#> 0.1051558 0.1051703 0.1051709 0.1051709 
## Continuous compounding
cc_rate(0.1)
#> [1] 0.1051709
```
