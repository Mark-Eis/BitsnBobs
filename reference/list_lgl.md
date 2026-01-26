# Split Data Frame into a List Based on Values in Logical Columns

Split a data frame into a named `list` of `tibble` data frames on the
basis that each new data frame forming an element of the `list`
comprises rows of the original data frame that contained `TRUE` values
in a particular `logical` column.

## Usage

``` r
list_lgl(.data, ...)
```

## Arguments

- .data:

  a data frame, or a data frame extension (e.g. a
  [`tibble`](https://tibble.tidyverse.org/reference/tibble-package.html)).

- ...:

  \<[`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)\>
  The selection of columns in `data` to be included in the tibbles
  comprising elements of the returned list.

## Value

A named list of
[tibble](https://tibble.tidyverse.org/reference/tibble-package.html)
data frames containing selected rows and columns of the original data
frame, having length equal to the number of logical columns in the
original data frame and names the same as the corresponding logical
columns.

## Details

Columns of `data` to be included in `tibble`s comprising the list
returned may be selected using the `...` argument with the
\<[`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)\>
syntax of package
[dplyr](https://dplyr.tidyverse.org/reference/dplyr-package.html),
including use of **selection helpers**. If this argument is omitted, all
non-logical columns will be selected using the tidy selection predicate
function
[`where`](https://tidyselect.r-lib.org/reference/where.html)`(\(x) !`[`is.logical`](https://rdrr.io/r/base/logical.html)`(x))`.

The length of the `list` returned is equal to the number of `logical`
columns in the original data frame. Each list element is named the same
as the corresponding logical column in `data` from which rows with
`TRUE` values were selected for inclusion in its `tibble`.

`list_lgl()` may be used to tease out information in a character column
of a data frame containing selected keywords previously flagged in
logical columns using
[`kwd_cols`](https://mark-eis.github.io/BitsnBobs/reference/kwd_cols.md),
see examples. Finally, the named list may be converted back to a single
data frame using
[`bind_rows`](https://dplyr.tidyverse.org/reference/bind_rows.html), see
examples.

## See also

[`bind_rows`](https://dplyr.tidyverse.org/reference/bind_rows.html),
[`select`](https://dplyr.tidyverse.org/reference/select.html),
[`split`](https://rdrr.io/r/base/split.html),
\<[`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)\>,
[`tibble`](https://tibble.tidyverse.org/reference/tibble-package.html).

Other logical-cols:
[`count_lgl()`](https://mark-eis.github.io/BitsnBobs/reference/count_lgl.md),
[`kwd_cols()`](https://mark-eis.github.io/BitsnBobs/reference/kwd_cols.md)

## Examples

``` r
## Following on from kwd_cols() examples… 
car_names <- data.frame(Response = rownames(mtcars))
kwd <- c("Mazda", "Merc", "Toyota", "Volkswagen", "X", "450")
kwd_cols(car_names)<- kwd
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

list_lgl(car_names)
#> $Mazda
#>        Response
#> 1     Mazda RX4
#> 2 Mazda RX4 Wag
#> 
#> $Merc
#>       Response
#> 8    Merc 240D
#> 9     Merc 230
#> 10    Merc 280
#> 11   Merc 280C
#> 12  Merc 450SE
#> 13  Merc 450SL
#> 14 Merc 450SLC
#> 
#> $Toyota
#>          Response
#> 20 Toyota Corolla
#> 21  Toyota Corona
#> 
#> $Volkswagen
#> [1] Response
#> <0 rows> (or 0-length row.names)
#> 
#> $X
#>         Response
#> 1      Mazda RX4
#> 2  Mazda RX4 Wag
#> 26     Fiat X1-9
#> 
#> $`450`
#>       Response
#> 12  Merc 450SE
#> 13  Merc 450SL
#> 14 Merc 450SLC
#> 
list_lgl(car_names, Response, X)
#> $Mazda
#>        Response    X
#> 1     Mazda RX4 TRUE
#> 2 Mazda RX4 Wag TRUE
#> 
#> $Merc
#>       Response     X
#> 8    Merc 240D FALSE
#> 9     Merc 230 FALSE
#> 10    Merc 280 FALSE
#> 11   Merc 280C FALSE
#> 12  Merc 450SE FALSE
#> 13  Merc 450SL FALSE
#> 14 Merc 450SLC FALSE
#> 
#> $Toyota
#>          Response     X
#> 20 Toyota Corolla FALSE
#> 21  Toyota Corona FALSE
#> 
#> $Volkswagen
#> [1] Response X       
#> <0 rows> (or 0-length row.names)
#> 
#> $X
#>         Response    X
#> 1      Mazda RX4 TRUE
#> 2  Mazda RX4 Wag TRUE
#> 26     Fiat X1-9 TRUE
#> 
#> $`450`
#>       Response     X
#> 12  Merc 450SE FALSE
#> 13  Merc 450SL FALSE
#> 14 Merc 450SLC FALSE
#> 
list_lgl(car_names, last_col())
#> $Mazda
#>     450
#> 1 FALSE
#> 2 FALSE
#> 
#> $Merc
#>      450
#> 8  FALSE
#> 9  FALSE
#> 10 FALSE
#> 11 FALSE
#> 12  TRUE
#> 13  TRUE
#> 14  TRUE
#> 
#> $Toyota
#>      450
#> 20 FALSE
#> 21 FALSE
#> 
#> $Volkswagen
#> [1] 450
#> <0 rows> (or 0-length row.names)
#> 
#> $X
#>      450
#> 1  FALSE
#> 2  FALSE
#> 26 FALSE
#> 
#> $`450`
#>     450
#> 12 TRUE
#> 13 TRUE
#> 14 TRUE
#> 
list_lgl(car_names, contains("o"))
#> $Mazda
#>        Response Toyota Volkswagen
#> 1     Mazda RX4  FALSE      FALSE
#> 2 Mazda RX4 Wag  FALSE      FALSE
#> 
#> $Merc
#>       Response Toyota Volkswagen
#> 8    Merc 240D  FALSE      FALSE
#> 9     Merc 230  FALSE      FALSE
#> 10    Merc 280  FALSE      FALSE
#> 11   Merc 280C  FALSE      FALSE
#> 12  Merc 450SE  FALSE      FALSE
#> 13  Merc 450SL  FALSE      FALSE
#> 14 Merc 450SLC  FALSE      FALSE
#> 
#> $Toyota
#>          Response Toyota Volkswagen
#> 20 Toyota Corolla   TRUE      FALSE
#> 21  Toyota Corona   TRUE      FALSE
#> 
#> $Volkswagen
#> [1] Response   Toyota     Volkswagen
#> <0 rows> (or 0-length row.names)
#> 
#> $X
#>         Response Toyota Volkswagen
#> 1      Mazda RX4  FALSE      FALSE
#> 2  Mazda RX4 Wag  FALSE      FALSE
#> 26     Fiat X1-9  FALSE      FALSE
#> 
#> $`450`
#>       Response Toyota Volkswagen
#> 12  Merc 450SE  FALSE      FALSE
#> 13  Merc 450SL  FALSE      FALSE
#> 14 Merc 450SLC  FALSE      FALSE
#> 

## Convert back to single data frame
car_names |>
  list_lgl() |>
  bind_rows(.id = "Group")
#>     Group       Response
#> 1   Mazda      Mazda RX4
#> 2   Mazda  Mazda RX4 Wag
#> 3    Merc      Merc 240D
#> 4    Merc       Merc 230
#> 5    Merc       Merc 280
#> 6    Merc      Merc 280C
#> 7    Merc     Merc 450SE
#> 8    Merc     Merc 450SL
#> 9    Merc    Merc 450SLC
#> 10 Toyota Toyota Corolla
#> 11 Toyota  Toyota Corona
#> 12      X      Mazda RX4
#> 13      X  Mazda RX4 Wag
#> 14      X      Fiat X1-9
#> 15    450     Merc 450SE
#> 16    450     Merc 450SL
#> 17    450    Merc 450SLC

rm(kwd, car_names)
```
