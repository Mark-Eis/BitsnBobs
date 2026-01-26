# Vectorised conditional sign change

Vectorised function that changes sign of elements of a numeric vector,
dependent on values of a logical argument of the same length.

## Usage

``` r
swapsign(x, negate)
```

## Arguments

- x:

  A numeric vector.

- negate:

  A logical vector of the same length as `x`.

## Value

A numeric vector, length of `x`.

## Examples

``` r
swapsign(1:10, rep(c(FALSE, TRUE), 5))
#>  [1]   1  -2   3  -4   5  -6   7  -8   9 -10
```
