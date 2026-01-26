# Catapult Class for Consistent Printing

Creates an object of class `"catapult"` with a built-in title string
used for printing.

## Usage

``` r
catapult(object = vector(), lead = "Catapult", revert = FALSE, ...)

# S3 method for class 'catapult'
print(x, ...)
```

## Arguments

- object:

  Object to be converted to `"catapult"` class.

- lead:

  a `character` string giving the title to be printed.

- revert:

  `logical` indicating whether the `"revert"` attribute should be set to
  `TRUE` or `FALSE`. By default, when `FALSE`, the corresponding print
  method `print.catapult()` will invoke further print methods for the
  underlying inherited `object`; if `TRUE`, `print.catapult()` will
  return to the calling function after printing `lead`.

- ...:

  additional named arguments to be forwarded to print methods of classes
  inherited from `object`, for example as required for formatting and
  printing a `data frame`.

- x:

  an object used to select a method.

## Value

An object of class `"catapult"` inheriting class(es) from `object`.

## Details

`catapult()` converts an object to class `"catapult"`, inheriting from
its existing class(es).

## Note

Deprecated, use instead ParaAnita `announce()`.

## See also

[`cat()`](https://rdrr.io/r/base/cat.html),
[`print()`](https://rdrr.io/r/base/print.html).

Other print:
[`lf()`](https://mark-eis.github.io/BitsnBobs/reference/lf.md),
[`print_all()`](https://mark-eis.github.io/BitsnBobs/reference/print_all.md)

## Examples

``` r
catapult()
#> ___________
#> Catapult: -
#> 
#> logical(0)
(cpt <- catapult("x", lead = "Lorem ipsum dolor sit amet"))
#> _____________________________
#> Lorem ipsum dolor sit amet: -
#> 
#> [1] "x"
.class2(cpt)
#> [1] "catapult"  "character"
attr(cpt, "revert")
#> [1] FALSE

(cpt <- catapult("x", lead = "Lorem ipsum dolor sit amet", TRUE))
#> _____________________________
#> Lorem ipsum dolor sit amet: -
#> 
.class2(cpt)
#> [1] "catapult"  "character"
attr(cpt, "revert")
#> [1] TRUE

rm(cpt)
```
