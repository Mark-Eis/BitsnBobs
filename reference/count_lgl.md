# Count Combinations and Totals for Logical Columns in Data Frame

Functions to count the number of ocurrences of unique combinations of
values across all logical columns in a data frame and the total number
of `TRUE` values for each logical column.

## Usage

``` r
count_lgl(df, .newcol = n, .arrange_by = NULL)

sum_lgl(df, wt = NULL)
```

## Arguments

- df:

  a data frame, or a data frame extension (e.g. a tibble).

- .newcol:

  quoted name to be assigned to the new count column; default `n`.

- .arrange_by:

  \<[`data-masking`](https://rlang.r-lib.org/reference/args_data_masking.html)\>
  quoted name of a column for ordering results; default `NULL`.

- wt:

  frequency weights. Can be `NULL` or the name of a numeric variable
  column; default `NULL`.

  - If `NULL` (the default), counts the number of rows with `TRUE`
    values for each logical column.

  - If a variable, computes sum(wt) for rows with `TRUE` values for each
    logical column.

## Value

`count_lgl()` returns a data frame comprising unique combinations of the
logical columns of `df` and an additional column of the counts of
ocurrences of each of these combinations. `sum_lgl()` returns a named
numeric vector of totals for each logical column.

## Details

Function `count_lgl()` counts the number of ocurrences of unique
combinations of values across all logical columns in a data frame using
[`summarise`](https://dplyr.tidyverse.org/reference/summarise.html) in
package
[dplyr](https://dplyr.tidyverse.org/reference/dplyr-package.html).

Function `sum_lgl()` counts the total number of `TRUE` values for each
logical column in a data frame using matrix multiplication.

These functions may be useful for counting combinations or tallying
totals of keywords flagged as `TRUE` in logical columns added to a data
frame using
[`kwd_cols`](https://mark-eis.github.io/BitsnBobs/reference/kwd_cols.md).

## See also

[`summarise`](https://dplyr.tidyverse.org/reference/summarise.html).

Other logical-cols:
[`kwd_cols()`](https://mark-eis.github.io/BitsnBobs/reference/kwd_cols.md),
[`list_lgl()`](https://mark-eis.github.io/BitsnBobs/reference/list_lgl.md)

## Examples

``` r
## Following on from kwd_cols() examples… 
car_names <- data.frame(Response = rownames(mtcars)) |>
  kwd_cols(, c("Mazda", "Merc", "Toyota", "Volkswagen", "X", "450"))
car_names
#>               Response Mazda  Merc Toyota Volkswagen     X   450
#> 1            Mazda RX4  TRUE FALSE  FALSE      FALSE  TRUE FALSE
#> 2        Mazda RX4 Wag  TRUE FALSE  FALSE      FALSE  TRUE FALSE
#> 3           Datsun 710 FALSE FALSE  FALSE      FALSE FALSE FALSE
#> 4       Hornet 4 Drive FALSE FALSE  FALSE      FALSE FALSE FALSE
#> 5    Hornet Sportabout FALSE FALSE  FALSE      FALSE FALSE FALSE
#> 6              Valiant FALSE FALSE  FALSE      FALSE FALSE FALSE
#> 7           Duster 360 FALSE FALSE  FALSE      FALSE FALSE FALSE
#> 8            Merc 240D FALSE  TRUE  FALSE      FALSE FALSE FALSE
#> 9             Merc 230 FALSE  TRUE  FALSE      FALSE FALSE FALSE
#> 10            Merc 280 FALSE  TRUE  FALSE      FALSE FALSE FALSE
#> 11           Merc 280C FALSE  TRUE  FALSE      FALSE FALSE FALSE
#> 12          Merc 450SE FALSE  TRUE  FALSE      FALSE FALSE  TRUE
#> 13          Merc 450SL FALSE  TRUE  FALSE      FALSE FALSE  TRUE
#> 14         Merc 450SLC FALSE  TRUE  FALSE      FALSE FALSE  TRUE
#> 15  Cadillac Fleetwood FALSE FALSE  FALSE      FALSE FALSE FALSE
#> 16 Lincoln Continental FALSE FALSE  FALSE      FALSE FALSE FALSE
#> 17   Chrysler Imperial FALSE FALSE  FALSE      FALSE FALSE FALSE
#> 18            Fiat 128 FALSE FALSE  FALSE      FALSE FALSE FALSE
#> 19         Honda Civic FALSE FALSE  FALSE      FALSE FALSE FALSE
#> 20      Toyota Corolla FALSE FALSE   TRUE      FALSE FALSE FALSE
#> 21       Toyota Corona FALSE FALSE   TRUE      FALSE FALSE FALSE
#> 22    Dodge Challenger FALSE FALSE  FALSE      FALSE FALSE FALSE
#> 23         AMC Javelin FALSE FALSE  FALSE      FALSE FALSE FALSE
#> 24          Camaro Z28 FALSE FALSE  FALSE      FALSE FALSE FALSE
#> 25    Pontiac Firebird FALSE FALSE  FALSE      FALSE FALSE FALSE
#> 26           Fiat X1-9 FALSE FALSE  FALSE      FALSE  TRUE FALSE
#> 27       Porsche 914-2 FALSE FALSE  FALSE      FALSE FALSE FALSE
#> 28        Lotus Europa FALSE FALSE  FALSE      FALSE FALSE FALSE
#> 29      Ford Pantera L FALSE FALSE  FALSE      FALSE FALSE FALSE
#> 30        Ferrari Dino FALSE FALSE  FALSE      FALSE FALSE FALSE
#> 31       Maserati Bora FALSE FALSE  FALSE      FALSE FALSE FALSE
#> 32          Volvo 142E FALSE FALSE  FALSE      FALSE FALSE FALSE

(carname_counts <- count_lgl(car_names, .arrange_by = desc(n)))
#>   Mazda  Merc Toyota Volkswagen     X   450  n
#> 1 FALSE FALSE  FALSE      FALSE FALSE FALSE 20
#> 2 FALSE  TRUE  FALSE      FALSE FALSE FALSE  4
#> 3 FALSE  TRUE  FALSE      FALSE FALSE  TRUE  3
#> 4  TRUE FALSE  FALSE      FALSE  TRUE FALSE  2
#> 5 FALSE FALSE   TRUE      FALSE FALSE FALSE  2
#> 6 FALSE FALSE  FALSE      FALSE  TRUE FALSE  1
sum_lgl(car_names)
#>      Mazda       Merc     Toyota Volkswagen          X        450 
#>          2          7          2          0          3          3 
sum_lgl(carname_counts, wt = "n")
#>      Mazda       Merc     Toyota Volkswagen          X        450 
#>          2          7          2          0          3          3 

car_names |> count_lgl(.newcol = subtotals) |> print_lf() |> sum_lgl(wt = "subtotals")
#>   Mazda  Merc Toyota Volkswagen     X   450 subtotals
#> 1 FALSE FALSE  FALSE      FALSE FALSE FALSE        20
#> 2 FALSE FALSE  FALSE      FALSE  TRUE FALSE         1
#> 3 FALSE FALSE   TRUE      FALSE FALSE FALSE         2
#> 4 FALSE  TRUE  FALSE      FALSE FALSE FALSE         4
#> 5 FALSE  TRUE  FALSE      FALSE FALSE  TRUE         3
#> 6  TRUE FALSE  FALSE      FALSE  TRUE FALSE         2
#> 
#>      Mazda       Merc     Toyota Volkswagen          X        450 
#>          2          7          2          0          3          3 

rm(car_names, carname_counts)
```
