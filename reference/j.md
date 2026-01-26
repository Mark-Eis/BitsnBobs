# Amount of Interest from an Effective Rate, a Payment Frequency and a Balance

`j()` calculates j, the amount of interest payable from an effective
rate, a payment frequency and a balance.

`j2eff_rate()` calculates the effective interest rate from the amount of
interest payable, a payment frequency and a balance.

## Usage

``` r
j(eff_rate, pay_fq, bal = 1)

j2eff_rate(j, pay_fq, bal = 1)
```

## Arguments

- eff_rate:

  numeric, the effective annual rate of interest.

- pay_fq, :

  `integer`, the payment frequency per year.

- bal:

  numeric, the balance of the loan; default `1`.

- j:

  numeric, the interest payment.

## Value

A numeric: `j()` returns the calculated interest payment; `j2eff_rate()`
returns the effective rate of interest.

## Details

Interest payable (j): —

\$\$j = \displaystyle \left(\left(1 + er \right)^{\frac{1}{pf}} - 1
\right).bal\$\$

Effective rate (er): —

\$\$er = \displaystyle \left(1 + \frac{j}{bal} \right)^{pf} - 1\$\$

where \\j\\ is the interest payment, \\er\\ is the effective rate,
\\pf\\ is the payment frequency, and \\bal\\ is the balance. See
[`amort.period()`](https://rdrr.io/pkg/FinancialMath/man/amort.period.html)
in package
[FinancialMath](https://CRAN.R-project.org/package=FinancialMath) for
further details.

## See also

[`amort.period()`](https://rdrr.io/pkg/FinancialMath/man/amort.period.html),
[`amort.table()`](https://rdrr.io/pkg/FinancialMath/man/amort.table.html)

Other amort:
[`eff_rate()`](https://mark-eis.github.io/BitsnBobs/reference/eff_rate.md),
[`get_amortval()`](https://mark-eis.github.io/BitsnBobs/reference/get_amortval.md)

## Examples

``` r
j(0.05915929, 12, 4000)
#> [1] 19.20444

j2eff_rate(19.20444, 12, 4000)
#> [1] 0.05915928
```
