# Add Logical Columns to Data Frame Flagging Presence of Keywords

Function to add logical columns indicating whether or not specified
keywords are present in a character column of a data frame.

## Usage

``` r
kwd_cols(data, .look_in = Response, value)

kwd_cols(data, .look_in = Response) <- value

lgl_cols(data)
```

## Arguments

- data:

  a data frame, or a data frame extension (e.g. a tibble).

- .look_in:

  \<[`data-masking`](https://rlang.r-lib.org/reference/args_data_masking.html)\>
  quoted name of the character column in which to look for keywords;
  default `Response`.

- value:

  a `character vector` containing the keywords to be identified.

## Value

`kwd_cols()` returns a modified data frame with logical columns
indicating the presence of keywords in `.look_in`. The infix form of the
function `kwd_cols<-()` modifies and returns the original data frame.
`lgl_cols()` returns a character vector of the names of all logical
columns in `data`.

## Details

The character column of `data` identified by `.look_in` is searched for
keywords provided in `value` using
[`str_detect`](https://stringr.tidyverse.org/reference/str_detect.html).
The keyword search is case insensitive. If no character column
identified by `look_in` is present in `data`, an error message will be
given.

The logical columns resulting from use of `kwd_cols()` and the infix
form `kwd_cols()<-` may be analysed and the number of `TRUE` values
counted using the functions
[`count_lgl`](https://mark-eis.github.io/BitsnBobs/reference/count_lgl.md)
and
[`sum_lgl`](https://mark-eis.github.io/BitsnBobs/reference/count_lgl.md).

The function `lgl_cols()` returns the names of all logical columns in
`data`.

## See also

[`detective()`](https://mark-eis.github.io/BitsnBobs/reference/detective.md),
[`str_detect`](https://stringr.tidyverse.org/reference/str_detect.html).

Other logical-cols:
[`count_lgl()`](https://mark-eis.github.io/BitsnBobs/reference/count_lgl.md),
[`list_lgl()`](https://mark-eis.github.io/BitsnBobs/reference/list_lgl.md)

## Examples

``` r
(car_names <- data.frame(Response = rownames(mtcars)))
#>               Response
#> 1            Mazda RX4
#> 2        Mazda RX4 Wag
#> 3           Datsun 710
#> 4       Hornet 4 Drive
#> 5    Hornet Sportabout
#> 6              Valiant
#> 7           Duster 360
#> 8            Merc 240D
#> 9             Merc 230
#> 10            Merc 280
#> 11           Merc 280C
#> 12          Merc 450SE
#> 13          Merc 450SL
#> 14         Merc 450SLC
#> 15  Cadillac Fleetwood
#> 16 Lincoln Continental
#> 17   Chrysler Imperial
#> 18            Fiat 128
#> 19         Honda Civic
#> 20      Toyota Corolla
#> 21       Toyota Corona
#> 22    Dodge Challenger
#> 23         AMC Javelin
#> 24          Camaro Z28
#> 25    Pontiac Firebird
#> 26           Fiat X1-9
#> 27       Porsche 914-2
#> 28        Lotus Europa
#> 29      Ford Pantera L
#> 30        Ferrari Dino
#> 31       Maserati Bora
#> 32          Volvo 142E

kwd <- c("Mazda", "Merc", "Toyota", "Volkswagen", "X", "450")
car_names |> kwd_cols(value = kwd)
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
car_names    ## Original data frame unchanged
#>               Response
#> 1            Mazda RX4
#> 2        Mazda RX4 Wag
#> 3           Datsun 710
#> 4       Hornet 4 Drive
#> 5    Hornet Sportabout
#> 6              Valiant
#> 7           Duster 360
#> 8            Merc 240D
#> 9             Merc 230
#> 10            Merc 280
#> 11           Merc 280C
#> 12          Merc 450SE
#> 13          Merc 450SL
#> 14         Merc 450SLC
#> 15  Cadillac Fleetwood
#> 16 Lincoln Continental
#> 17   Chrysler Imperial
#> 18            Fiat 128
#> 19         Honda Civic
#> 20      Toyota Corolla
#> 21       Toyota Corona
#> 22    Dodge Challenger
#> 23         AMC Javelin
#> 24          Camaro Z28
#> 25    Pontiac Firebird
#> 26           Fiat X1-9
#> 27       Porsche 914-2
#> 28        Lotus Europa
#> 29      Ford Pantera L
#> 30        Ferrari Dino
#> 31       Maserati Bora
#> 32          Volvo 142E

kwd_cols(car_names)<- kwd
car_names    ## Original data frame modified by infix version
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

lgl_cols(car_names)
#> [1] "Mazda"      "Merc"       "Toyota"     "Volkswagen" "X"         
#> [6] "450"       
identical(kwd, lgl_cols(car_names))
#> [1] TRUE
rm(car_names, kwd)
```
