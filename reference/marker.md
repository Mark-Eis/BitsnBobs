# Inlinable Marker Function for Function Development

This inlinable function outputs a user modifiable message identifying
its enclosing function using
[`sys.call`](https://rdrr.io/r/base/sys.parent.html) then returns its
first argument, if any.

## Usage

``` r
marker(rval = NULL, msg = "in Mimiland")
```

## Arguments

- rval:

  value to be returned; default `NULL`.

- msg:

  ending of the output message. Since this argument follows `rval`,
  which normally would only ever be supplied inline, its name cannot be
  omitted unless either it follows a comma or `marker()` is inlined;
  default `"in Mimiland"`.

## Value

The first argument `rval` if any, otherwise `NULL`.

## Details

Mainly useful for the side effect of the message, which can be used to
distinguish among versions of the function under development. Can only
be used within a function and may be inlined in a
`<`[`piped sequence`](https://rdrr.io/r/base/pipeOp.html)`>` using `|>`,
see examples.

## See also

[`sys.call()`](https://rdrr.io/r/base/sys.parent.html)

Other utils:
[`const()`](https://mark-eis.github.io/BitsnBobs/reference/const.md),
[`endstop()`](https://mark-eis.github.io/BitsnBobs/reference/endstop.md),
[`op-min-max`](https://mark-eis.github.io/BitsnBobs/reference/op-min-max.md),
[`revmat()`](https://mark-eis.github.io/BitsnBobs/reference/revmat.md)

## Examples

``` r
f1 <- function() marker()
f1()
#> f1 running in Mimiland 

f2 <- function() marker(msg = "in BitsnBobs")
f2()
#> f2 running in BitsnBobs 

f3 <- function() marker(, "in BitsnBobs")
f3()
#> f3 running in BitsnBobs 

## Use inline in "piped" sequence
f4 <- function(x) x |> marker() |> sqrt()
f4(9)
#> f4 running in Mimiland 
#> [1] 3

f5 <- function(x) x |> marker(msg = "inlined in BitsnBobs") |> exp()
f5(1)
#> f5 running inlined in BitsnBobs 
#> [1] 2.718282

f6 <- function(x) x |> marker("inlined in BitsnBobs") |> log()
f4(1) |> f5() |> f6()
#> f6 running inlined in BitsnBobs 
#> f5 running inlined in BitsnBobs 
#> f4 running in Mimiland 
#> [1] 1

rm_objects(f, 1:6)
#> Objects matching "f…" found in (unnamed) environment: –
#>   f1 f2 f3 f4 f5 f6 
#> Objects matching "f…" remaining in (unnamed) environment: –
#>   All gone! 
```
