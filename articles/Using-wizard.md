# Using the wizard function

Copyright (c) 2026 Mark Eisler

## 1. Introduction

The
[`wizard()`](https://mark-eis.github.io/BitsnBobs/reference/wizard.md)
function extracts and sorts unique values of a selected column from a
data frame, and optionally pastes into a character string.

For our examples, we will use the `mtcars` data.

## 2. Using a symbol argument

`wizard(data, col, .collapse = NULL)` uses a symbol for its
`<data-masking>` argument `col`.

``` r

mtcars |> wizard(cyl)
#> [1] 4 6 8
```

This shows sorted unique values of the `cyl` column of `mtcars`.

If there is a potentially confounding variable of the same name in the
global environment,

``` r

(cyl <- c(100:110))
#>  [1] 100 101 102 103 104 105 106 107 108 109 110
```

…nevertheless, `<data-masking>` ensures that the `col` argument of
[`wizard()`](https://mark-eis.github.io/BitsnBobs/reference/wizard.md)
correctly refers to the `cyl` column of the `mtcars` data.

``` r

mtcars |> wizard(cyl)
#> [1] 4 6 8
```

But say there’s a similar variable `Cyl` in the global environment,

``` r

Cyl <- "Bad Luck!"
```

…and we confuse `cyl` with `Cyl`, we obtain an unexpected and puzzling
result: -

``` r

mtcars |> wizard(Cyl)
#> [1] "Bad Luck!"
```

## 3. Using the `.data` pronoun

Using the `.data` pronoun[^1] from the **rlang** package circumvents
this problem by throwing a meaningful error message: -

``` r

mtcars |> wizard(.data$cyl)
#> [1] 4 6 8

try(mtcars |> wizard(.data$Cyl))
#> Error in .data$Cyl : Column `Cyl` not found in `.data`.
```

## 4. Symbolise and inject

The col argument might be saved as a character variable, for example: -

``` r

(col_var <- "cyl")
#> [1] "cyl"
```

…which needs converting to a symbol and injecting[^2] using the
injection operator `!!` from the **rlang** package: -

``` r

mtcars |> wizard(!!sym(col_var))
#> [1] 4 6 8
```

Again, if we confuse `cyl` with `Cyl`, we obtain an unexpected and
confusing result: -

``` r

(col_var <- "Cyl")
#> [1] "Cyl"

mtcars |> wizard(!!sym(col_var))
#> [1] "Bad Luck!"
```

Using `data_sym()` from **rlang** circumvents this problem by prefacing
the symbol with the data pronoun: -

``` r

data_sym(col_var)
#> .data$Cyl

try(mtcars |> wizard(!!data_sym(col_var)))
#> Error in .data$Cyl : Column `Cyl` not found in `.data`.
```

## 5. Examples using `lapply()`

(We could also use
[`purrr::map()`](https://purrr.tidyverse.org/reference/map.html), but it
has no particular advantage here so **base**
[`lapply()`](https://rdrr.io/r/base/lapply.html) is preferable.)

``` r

mtcars |> lapply(wizard, data = mtcars)
#> $mpg
#>  [1] 10.4 13.3 14.3 14.7 15.0 15.2 15.5 15.8 16.4 17.3 17.8 18.1 18.7 19.2 19.7
#> [16] 21.0 21.4 21.5 22.8 24.4 26.0 27.3 30.4 32.4 33.9
#> 
#> $cyl
#> [1] 4 6 8
#> 
#> $disp
#>  [1]  71.1  75.7  78.7  79.0  95.1 108.0 120.1 120.3 121.0 140.8 145.0 146.7
#> [13] 160.0 167.6 225.0 258.0 275.8 301.0 304.0 318.0 350.0 351.0 360.0 400.0
#> [25] 440.0 460.0 472.0
#> 
#> $hp
#>  [1]  52  62  65  66  91  93  95  97 105 109 110 113 123 150 175 180 205 215 230
#> [20] 245 264 335
#> 
#> $drat
#>  [1] 2.76 2.93 3.00 3.07 3.08 3.15 3.21 3.23 3.54 3.62 3.69 3.70 3.73 3.77 3.85
#> [16] 3.90 3.92 4.08 4.11 4.22 4.43 4.93
#> 
#> $wt
#>  [1] 1.513 1.615 1.835 1.935 2.140 2.200 2.320 2.465 2.620 2.770 2.780 2.875
#> [13] 3.150 3.170 3.190 3.215 3.435 3.440 3.460 3.520 3.570 3.730 3.780 3.840
#> [25] 3.845 4.070 5.250 5.345 5.424
#> 
#> $qsec
#>  [1] 14.50 14.60 15.41 15.50 15.84 16.46 16.70 16.87 16.90 17.02 17.05 17.30
#> [13] 17.40 17.42 17.60 17.82 17.98 18.00 18.30 18.52 18.60 18.61 18.90 19.44
#> [25] 19.47 19.90 20.00 20.01 20.22 22.90
#> 
#> $vs
#> [1] 0 1
#> 
#> $am
#> [1] 0 1
#> 
#> $gear
#> [1] 3 4 5
#> 
#> $carb
#> [1] 1 2 3 4 6 8
```

Here we use [`setNames()`](https://rdrr.io/r/stats/setNames.html) to
obtain an eponymously named character vector of the first four `mtcar`
names: -

``` r

(carnames <- setNames(nm = names(mtcars))[1:4])
#>    mpg    cyl   disp     hp 
#>  "mpg"  "cyl" "disp"   "hp"
```

…and use it in three equivalent versions where
[`lapply()`](https://rdrr.io/r/base/lapply.html) uses the
`.data pronoun` ,`data_sym()` or `data_syms()` in an anonymous function:
-

``` r

carnames |> lapply(\(x) wizard(mtcars, .data[[x]]))
#> $mpg
#>  [1] 10.4 13.3 14.3 14.7 15.0 15.2 15.5 15.8 16.4 17.3 17.8 18.1 18.7 19.2 19.7
#> [16] 21.0 21.4 21.5 22.8 24.4 26.0 27.3 30.4 32.4 33.9
#> 
#> $cyl
#> [1] 4 6 8
#> 
#> $disp
#>  [1]  71.1  75.7  78.7  79.0  95.1 108.0 120.1 120.3 121.0 140.8 145.0 146.7
#> [13] 160.0 167.6 225.0 258.0 275.8 301.0 304.0 318.0 350.0 351.0 360.0 400.0
#> [25] 440.0 460.0 472.0
#> 
#> $hp
#>  [1]  52  62  65  66  91  93  95  97 105 109 110 113 123 150 175 180 205 215 230
#> [20] 245 264 335

carnames |> lapply(\(x) wizard(mtcars, !!data_sym(x)))
#> $mpg
#>  [1] 10.4 13.3 14.3 14.7 15.0 15.2 15.5 15.8 16.4 17.3 17.8 18.1 18.7 19.2 19.7
#> [16] 21.0 21.4 21.5 22.8 24.4 26.0 27.3 30.4 32.4 33.9
#> 
#> $cyl
#> [1] 4 6 8
#> 
#> $disp
#>  [1]  71.1  75.7  78.7  79.0  95.1 108.0 120.1 120.3 121.0 140.8 145.0 146.7
#> [13] 160.0 167.6 225.0 258.0 275.8 301.0 304.0 318.0 350.0 351.0 360.0 400.0
#> [25] 440.0 460.0 472.0
#> 
#> $hp
#>  [1]  52  62  65  66  91  93  95  97 105 109 110 113 123 150 175 180 205 215 230
#> [20] 245 264 335

carnames |> data_syms() |> lapply(\(x) wizard(mtcars, !!x))
#> $mpg
#>  [1] 10.4 13.3 14.3 14.7 15.0 15.2 15.5 15.8 16.4 17.3 17.8 18.1 18.7 19.2 19.7
#> [16] 21.0 21.4 21.5 22.8 24.4 26.0 27.3 30.4 32.4 33.9
#> 
#> $cyl
#> [1] 4 6 8
#> 
#> $disp
#>  [1]  71.1  75.7  78.7  79.0  95.1 108.0 120.1 120.3 121.0 140.8 145.0 146.7
#> [13] 160.0 167.6 225.0 258.0 275.8 301.0 304.0 318.0 350.0 351.0 360.0 400.0
#> [25] 440.0 460.0 472.0
#> 
#> $hp
#>  [1]  52  62  65  66  91  93  95  97 105 109 110 113 123 150 175 180 205 215 230
#> [20] 245 264 335
```

If one of the names is incorrect: -

``` r

carnames["cyl"] <- "Cyl"
carnames
#>    mpg    cyl   disp     hp 
#>  "mpg"  "Cyl" "disp"   "hp"
```

…these all provide meaningful error messages as previously shown: -

``` r

try(carnames |> lapply(\(x) wizard(mtcars, .data[[x]])))
#> Error in .data[["Cyl"]] : Column `Cyl` not found in `.data`.

try(carnames |> lapply(\(x) wizard(mtcars, !!data_sym(x))))
#> Error in .data$Cyl : Column `Cyl` not found in `.data`.

try(carnames |> data_syms() |> lapply(\(x) wizard(mtcars, !!x)))
#> Error in .data$Cyl : Column `Cyl` not found in `.data`.
```

Without the `.data` pronoun, the error might be easily overlooked: -

``` r

carnames |> lapply(\(x) wizard(mtcars, !!sym(x)))
#> $mpg
#>  [1] 10.4 13.3 14.3 14.7 15.0 15.2 15.5 15.8 16.4 17.3 17.8 18.1 18.7 19.2 19.7
#> [16] 21.0 21.4 21.5 22.8 24.4 26.0 27.3 30.4 32.4 33.9
#> 
#> $cyl
#> [1] "Bad Luck!"
#> 
#> $disp
#>  [1]  71.1  75.7  78.7  79.0  95.1 108.0 120.1 120.3 121.0 140.8 145.0 146.7
#> [13] 160.0 167.6 225.0 258.0 275.8 301.0 304.0 318.0 350.0 351.0 360.0 400.0
#> [25] 440.0 460.0 472.0
#> 
#> $hp
#>  [1]  52  62  65  66  91  93  95  97 105 109 110 113 123 150 175 180 205 215 230
#> [20] 245 264 335

carnames |> syms() |> lapply(\(x) wizard(mtcars, !!x))
#> $mpg
#>  [1] 10.4 13.3 14.3 14.7 15.0 15.2 15.5 15.8 16.4 17.3 17.8 18.1 18.7 19.2 19.7
#> [16] 21.0 21.4 21.5 22.8 24.4 26.0 27.3 30.4 32.4 33.9
#> 
#> $cyl
#> [1] "Bad Luck!"
#> 
#> $disp
#>  [1]  71.1  75.7  78.7  79.0  95.1 108.0 120.1 120.3 121.0 140.8 145.0 146.7
#> [13] 160.0 167.6 225.0 258.0 275.8 301.0 304.0 318.0 350.0 351.0 360.0 400.0
#> [25] 440.0 460.0 472.0
#> 
#> $hp
#>  [1]  52  62  65  66  91  93  95  97 105 109 110 113 123 150 175 180 205 215 230
#> [20] 245 264 335
```

## 6. References

1.  [**rlang** .data and .env
    pronouns](https://rlang.r-lib.org/reference/dot-data.html)
2.  [**rlang** symbolise and
    inject](https://rlang.r-lib.org/reference/topic-metaprogramming.html#symbolise-and-inject)

[^1]: [**rlang** .data and .env
    pronouns](https://rlang.r-lib.org/reference/dot-data.html)

[^2]: [**rlang** symbolise and
    inject](https://rlang.r-lib.org/reference/topic-metaprogramming.html#symbolise-and-inject)
