# Reverse a Matrix by Rows, Columns or Both

Reverses the order of entire matrix rows, columns or both.

## Usage

``` r
revmat(x, type = c("byrow", "bycolumn", "byboth"))
```

## Arguments

- x:

  a `matrix`.

- type:

  a character string specifying whether to reverse matrix rows, columns
  or both; must be one of `"byrow"` (default), `"bycolumn"` or
  `"byboth"`. You can specify just the initial letter.

## Value

A `matrix` of the same dimensions as `x`.

## Details

Depending on the argument `type`, this function reverses the order of
entire rows, columns or both of a matrix, including any dimnames if
present.

## See also

[`matrix`](https://rdrr.io/r/base/matrix.html),
[`t`](https://rdrr.io/r/base/t.html)

Other utils:
[`const()`](https://mark-eis.github.io/BitsnBobs/reference/const.md),
[`endstop()`](https://mark-eis.github.io/BitsnBobs/reference/endstop.md),
[`marker()`](https://mark-eis.github.io/BitsnBobs/reference/marker.md),
[`op-min-max`](https://mark-eis.github.io/BitsnBobs/reference/op-min-max.md)

## Examples

``` r
 m <- matrix(1:9, nrow = 3, byrow = TRUE, dimnames = list(paste0("x", 1:3), paste0("y", 1:3)))

 m
#>    y1 y2 y3
#> x1  1  2  3
#> x2  4  5  6
#> x3  7  8  9
 revmat(m)
#>    y1 y2 y3
#> x3  7  8  9
#> x2  4  5  6
#> x1  1  2  3
 revmat(m, "bycol")
#>    y3 y2 y1
#> x1  3  2  1
#> x2  6  5  4
#> x3  9  8  7
 revmat(m, "byboth")
#>    y3 y2 y1
#> x3  9  8  7
#> x2  6  5  4
#> x1  3  2  1

 rm(m)
```
