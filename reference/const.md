# Make a Constant with Active Binding

A constant is an object the value of which cannot be altered once
assigned.

## Usage

``` r
const(name, value)
```

## Arguments

- name:

  a symbol, the name to be assigned to the constant.

- value:

  any valid R object, including a function

## Value

Returns argument `value` invisibly (via `invisible(value)`).

## Details

See reference.

## References

[Siqi Zhang, 2019: Make a Constant in R with Active
Binding](https://iqis.netlify.app/post/2019/07/22/how-to-make-a-constant-in-r/).

## See also

[`lockBinding()`](https://rdrr.io/r/base/bindenv.html)

Other utils:
[`endstop()`](https://mark-eis.github.io/BitsnBobs/reference/endstop.md),
[`marker()`](https://mark-eis.github.io/BitsnBobs/reference/marker.md),
[`op-min-max`](https://mark-eis.github.io/BitsnBobs/reference/op-min-max.md),
[`revmat()`](https://mark-eis.github.io/BitsnBobs/reference/revmat.md)

## Examples

``` r
(const(bar, "irish"))
#> [1] "irish"

try(bar <- "bavarian")
#> Error in try(bar <- "bavarian") : 
#>   cannot change value of locked binding for 'bar'
```
