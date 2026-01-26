# Generic Iterative And Recursive Function Handlers

Iterate or recur a function until the result converges.

## Usage

``` r
iterate(x, func, tolerance = getOption("digits"))

recursive(x, func, tolerance = getOption("digits"))
```

## Arguments

- x:

  an initial estimate to be passed to the iterative function.

- func:

  the iterative function, which should have just one argument
  representing the previous best estimate.

- tolerance:

  the number of decimal places to which the revised estimate is tested
  against its predecessor for convergence; default
  [`options`](https://rdrr.io/r/base/options.html)`("digits")`.

## Value

Result of iteration or recursion of the function.

## Details

Iterates or recurs the function provided until the result converges at a
given tolerance, assessed by testing changes in the value (rounded to a
given number of number of decimal places) at each iteration or
recurrence.

Although the iterative or recursive function should have just one
argument, it may contain additional constant values or data, see
examples.

`iterate` is similar to `recursive` but uses a while loop rather than
recursion.

## See also

[`options`](https://rdrr.io/r/base/options.html),
[`options`](https://rdrr.io/r/base/options.html)`("digits")`.

## Examples

``` r
## Newton's method for square root 1000 using a shorthand form anonymous function
iterate(30, \(y)((y + 1000 / y) / 2))
#> [1] 31.62278
recursive(30, \(y)((y + 1000 / y) / 2))
#> [1] 31.62278

## Newton's method for any square root using a function defined within a function
newtroot <- function(x, est = x / 2) {
   func <- function(y) (y + x / y) / 2
   iterate(est, func)
}

newtroot(1000)
#> [1] 31.62278
newtroot(1000, 30)
#> [1] 31.62278

newtroot <- function(x, est = x / 2) {
   func <- function(y) (y + x / y) / 2
   recursive(est, func)
}

newtroot(1000)
#> [1] 31.62278
newtroot(1000, 30)
#> [1] 31.62278

## More directly using a shorthand form anonymous function within a function
newtroot <- function(x, est = x / 2)
   iterate(est, \(y)((y + x / y) / 2))

newtroot(1000)
#> [1] 31.62278

newtroot <- function(x, est = x / 2)
   recursive(est, \(y)((y + x / y) / 2))

newtroot(1000)
#> [1] 31.62278

##  Build in greater precision using tolerance argument
##  - albeit not seen without changing options("digits")
newtroot <- function(x, est = x / 2)
   iterate(est, \(y)((y + x / y) / 2), tolerance = 15)

newtroot(1000)
#> [1] 31.62278

newtroot <- function(x, est = x / 2)
   recursive(est, \(y)((y + x / y) / 2), tolerance = 15)

newtroot(1000)
#> [1] 31.62278

##  Build in less precision using tolerance argument
##  - not easily seen without changing options("digits")
newtroot <- function(x, est = x / 2)
   iterate(est, \(y)((y + x / y) / 2), tolerance = 0)

newtroot(1000)
#> [1] 31.62278

##  - More easily seen without changing options("digits")
newtroot <- function(x, est = x / 2)
   recursive(est, \(y)((y + x / y) / 2), tolerance = 0)

newtroot(1000)
#> [1] 31.64186

## Changing options("digits") also gives greater precision by default
dig <- options(digits = 15)

newtroot <- function(x, est = x / 2)
   iterate(est, \(y)((y + x / y) / 2))

newtroot(0.001, .03)
#> [1] 0.0316227766016838

newtroot <- function(x, est = x / 2)
   recursive(est, \(y)((y + x / y) / 2))

newtroot(0.001, .03)
#> [1] 0.0316227766016838

options(digits = dig$digits)
rm(dig)
```
