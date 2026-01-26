# Vectorised Min and Max Operators

Vectorised infix functions implementing
[`pmin()`](https://rdrr.io/r/base/Extremes.html) and
[`pmax()`](https://rdrr.io/r/base/Extremes.html).

## Usage

``` r
x %:<% y

x %>:% y
```

## Arguments

- x, y:

  numeric or character arguments (see Note).

## Value

A vector of length the longest of the input vectors as returned by
[`pmin()`](https://rdrr.io/r/base/Extremes.html) and
[`pmax()`](https://rdrr.io/r/base/Extremes.html), or of length zero if
one of the inputs had zero length.

## Details

The vectorised infix functions `%:<%` and `%>:%` may be useful in
implementing [`pmin()`](https://rdrr.io/r/base/Extremes.html) and,
[`pmax()`](https://rdrr.io/r/base/Extremes.html) and was inspired by the
null coalescing operator [`%||%`](https://rdrr.io/r/base/Control.html).

## Note

‘Numeric’ arguments are vectors of type integer and numeric, and logical
(coerced to integer). For historical reasons, `NULL` is accepted as
equivalent to `integer(0)`.

`pmax` and `pmin` will also work on classed S3 or S4 objects with
appropriate methods for comparison, `is.na` and `rep` (if recycling of
arguments is needed).

## See also

[`%||%`](https://rlang.r-lib.org/reference/op-null-default.html),
[`pmin()`](https://rdrr.io/r/base/Extremes.html) and
[`pmax()`](https://rdrr.io/r/base/Extremes.html).

Other utils:
[`const()`](https://mark-eis.github.io/BitsnBobs/reference/const.md),
[`endstop()`](https://mark-eis.github.io/BitsnBobs/reference/endstop.md),
[`marker()`](https://mark-eis.github.io/BitsnBobs/reference/marker.md),
[`revmat()`](https://mark-eis.github.io/BitsnBobs/reference/revmat.md)

## Examples

``` r
1:10 %:<% 10:1
#>  [1] 1 2 3 4 5 5 4 3 2 1
c(1:9, NA) %:<% c(NA, 9:1)
#>  [1] NA  2  3  4  5  5  4  3  2 NA

1:10 %>:% 10:1
#>  [1] 10  9  8  7  6  6  7  8  9 10
c(1:9, NA) %>:% c(NA, 9:1)
#>  [1] NA  9  8  7  6  6  7  8  9 NA
```
