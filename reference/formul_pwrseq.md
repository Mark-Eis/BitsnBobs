# Expand Term in Right Hand Side of a Formula as a Power Sequence

Create a power sequence from a term in a
[`formula`](https://rdrr.io/r/stats/formula.html), `base_fla`, and the
maximum power number, `n`, optionally including other formula terms.

## Usage

``` r
formul_pwrseq(base_fla, n, ...)
```

## Arguments

- base_fla:

  the a term in the formula to be expanded as a power sequence.

- n:

  a non-negative integer or number coercible into a positive integer.

- ...:

  additional terms in the formula.

## Value

A `formula`.

## See also

[`formula`](https://rdrr.io/r/stats/formula.html).

Other powerseq:
[`power_seq()`](https://mark-eis.github.io/BitsnBobs/reference/power_seq.md)

## Examples

``` r
formul_pwrseq(y ~ x, 5)
#> y ~ x + I(x^2L) + I(x^3L) + I(x^4L) + I(x^5L)
#> <environment: 0x55945ce20aa8>
formul_pwrseq(y ~ log(x), 5)
#> y ~ log(x) + I(log(x)^2L) + I(log(x)^3L) + I(log(x)^4L) + I(log(x)^5L)
#> <environment: 0x55945ce20aa8>
formul_pwrseq(y ~ A, 3, B, C, D)
#> y ~ A + I(A^2L) + I(A^3L) + B + C + D
#> <environment: 0x55945ce20aa8>
```
