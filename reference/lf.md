# Pipe-Friendly Line Feeds and Printing

`lf()` outputs one or more line feeds during a piped sequence.

## Usage

``` r
lf(x, n = 1)

print_lf(x, n = 1)
```

## Arguments

- x:

  Object to be piped.

- n:

  Number of line feeds; default `1`.

## Value

Invisibly returns its first argument.

## Details

`print_lf()` prints an object in a piped sequence then outputs one or
more line feeds.

An object passed as argument in a piped sequence is printed and/or one
or more line feeds are output during a piped sequence using
[`cat()`](https://rdrr.io/r/base/cat.html). This can be useful to
separate lines of printed output, see examples.

## See also

[`cat`](https://rdrr.io/r/base/cat.html).

Other print:
[`catapult()`](https://mark-eis.github.io/BitsnBobs/reference/catapult.md),
[`print_all()`](https://mark-eis.github.io/BitsnBobs/reference/print_all.md)

## Examples

``` r
obj <- "Lorem ipsum dolor sit amet"
obj |> lf()               # line feed, object returned invisibly
#> 
obj |> lf(3)              # three line feeds, object returned invisibly
#> 
#>  
#>  
(obj |> lf(3))            # three line feeds, returned object rendered visible
#> 
#>  
#>  
#> [1] "Lorem ipsum dolor sit amet"
obj |> lf(3) |> paste("consectetur adipiscing elit", sep = ", ")
#> 
#>  
#>  
#> [1] "Lorem ipsum dolor sit amet, consectetur adipiscing elit"

obj |> print() |> lf(3)   # line feeds are unexpectedly before printed output.
#> 
#>  
#>  
#> [1] "Lorem ipsum dolor sit amet"

## Use print_lf() instead
obj |> print_lf()         # object printed with line feed and returned invisibly
#> [1] "Lorem ipsum dolor sit amet"
#> 
obj |> print_lf(3)        # object printed with three line feeds and returned invisibly
#> [1] "Lorem ipsum dolor sit amet"
#> 
#>  
#>  
(obj |> print_lf(3))      # Ditto, then rendered visible
#> [1] "Lorem ipsum dolor sit amet"
#> 
#>  
#>  
#> [1] "Lorem ipsum dolor sit amet"
obj |> print_lf(3) |> paste("consectetur adipiscing elit", sep = ", ")
#> [1] "Lorem ipsum dolor sit amet"
#> 
#>  
#>  
#> [1] "Lorem ipsum dolor sit amet, consectetur adipiscing elit"

rm(obj)
```
