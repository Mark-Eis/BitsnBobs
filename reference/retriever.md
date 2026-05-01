# 'Function Factory' for Bespoke Data Frame Retrieval and Replacement

`retriever()` creates bespoke functions for retrieval of rows of a data
frame identified using a specified index column.

`remplacer()` creates bespoke replacement functions for modifying rows
of a data frame identified using a specified index column.

## Usage

``` r
retriever(data, index, labile_data = TRUE)

remplacer(index)
```

## Arguments

- data:

  a [`data frame`](https://rdrr.io/r/base/data.frame.html), or a data
  frame extension (e.g. a `tibble`).

- index:

  \<[`data-masking`](https://rlang.r-lib.org/reference/args_data_masking.html)\>
  quoted name of the index column to be searched.

- labile_data:

  `logical`. If `TRUE`, `data` are represented in the function
  environment as a `quosure`. If `FALSE`, a copy of `data` is saved in
  the function environment. Default `TRUE`.

## Value

For `retriever()`, a [`function`](https://rdrr.io/r/base/function.html)
for retrieval of specified columns of `data` in rows matching a
specified value in the `index` column, having the following arguments: –

- key:

  Value to be matched in the `index` column.

- ...:

  \<[`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)\>
  character columns to return. If none are provided, all columns are
  returned.

For `remplacer()`, a replacement `function` for modifying a value in a
specified column of `data` in rows matching a specified value in the
index column, having the following arguments: –

- data:

  A [`data frame`](https://rdrr.io/r/base/data.frame.html), or a data
  frame extension (e.g. a `tibble`).

- key:

  Value to be matched in the `index` column.

- replace:

  \<[`data-masking`](https://rlang.r-lib.org/reference/args_data_masking.html)\>
  quoted name of the column of the value to be replaced.

- value:

  The replacement value of the same type as `replace`.

## Details

The `key` value supplied as an argument to functions derived from both
`retriever()` and `remplacer()` must be exact matches with values in the
`index` column for rows to be retrieved or replaced. For pattern
matching, use
[`detective()`](https://mark-eis.github.io/BitsnBobs/reference/detective.md).

If `labile_data` is `TRUE`, `data` are represented in the function
environment as a
[`quosure`](https://rlang.r-lib.org/reference/topic-quosure.html), and
functions returned by `retriever()` will automatically refer to the
current version of `data` in its original
[`environment`](https://rdrr.io/r/base/environment.html), usually the
calling environment i.e., typically but not necessarily the global
environment. If `labile_data` is `FALSE`, returned functions refer to a
copy of `data` saved in the function environment at the time of
execution of `retriever()`, and will not reflect any subsequent changes
to the original `data`.

## Note

The original idea was for `remplacer()` to create bespoke replacement
functions using `data` captured in the function environment as with
`retriever()`. However, there is no immediately apparent way to do this
as the object to be "replaced" must be the first argument for
replacement functions.

## See also

[`data frame`](https://rdrr.io/r/base/data.frame.html)

Other detective:
[`detective()`](https://mark-eis.github.io/BitsnBobs/reference/detective.md),
[`wizard()`](https://mark-eis.github.io/BitsnBobs/reference/wizard.md)

## Examples

``` r

## Create "retrieval" function with labile_data TRUE
retrieve_starwars <- retriever(starwars, name)

## `data` represented as "quosure" in function environment 
environment(retrieve_starwars)$data
#> <quosure>
#> expr: ^starwars
#> env:  0x559d69651c80

## Retrieve selected columns for a row specified using the index
retrieve_starwars("Luke Skywalker", ends_with("color"), homeworld)
#> # A tibble: 1 × 5
#>   name           hair_color skin_color eye_color homeworld
#>   <chr>          <chr>      <chr>      <chr>     <chr>    
#> 1 Luke Skywalker blond      fair       blue      Tatooine 

## Create "retrieval" function with labile_data FALSE
retrieve_original_starwars <- retriever(starwars, name, FALSE)

## Copy of `data` saved in function environment 
environment(retrieve_original_starwars)$data
#> # A tibble: 87 × 14
#>    name  homeworld height  mass hair_color skin_color eye_color birth_year sex  
#>    <chr> <chr>      <int> <dbl> <chr>      <chr>      <chr>          <dbl> <chr>
#>  1 Luke… Tatooine     172    77 blond      fair       blue            19   male 
#>  2 C-3PO Tatooine     167    75 NA         gold       yellow         112   none 
#>  3 R2-D2 Naboo         96    32 NA         white, bl… red             33   none 
#>  4 Dart… Tatooine     202   136 none       white      yellow          41.9 male 
#>  5 Leia… Alderaan     150    49 brown      light      brown           19   fema…
#>  6 Owen… Tatooine     178   120 brown, gr… light      blue            52   male 
#>  7 Beru… Tatooine     165    75 brown      light      blue            47   fema…
#>  8 R5-D4 Tatooine      97    32 NA         white, red red             NA   none 
#>  9 Bigg… Tatooine     183    84 black      light      brown           24   male 
#> 10 Obi-… Stewjon      182    77 auburn, w… fair       blue-gray       57   male 
#> # ℹ 77 more rows
#> # ℹ 5 more variables: gender <chr>, species <chr>, films <list>,
#> #   vehicles <list>, starships <list>

## Retrieve selected columns for a row specified using the index
retrieve_original_starwars("Luke Skywalker", where(is.atomic), homeworld)
#> # A tibble: 1 × 11
#>   name   homeworld height  mass hair_color skin_color eye_color birth_year sex  
#>   <chr>  <chr>      <int> <dbl> <chr>      <chr>      <chr>          <dbl> <chr>
#> 1 Luke … Tatooine     172    77 blond      fair       blue              19 male 
#> # ℹ 2 more variables: gender <chr>, species <chr>

## Create replacement function
`replace_at_name<-` <- remplacer(name)

## Replace the value of a selected column for a row specified using the index
starwars |> replace_at_name("Luke Skywalker", homeworld) <- "Mimiland"

## Retrieve selected columns for a row specified using the index
## "retrieval" function with labile_data TRUE reflects the change
retrieve_starwars("Luke Skywalker", where(is.atomic), homeworld)
#> # A tibble: 1 × 11
#>   name   homeworld height  mass hair_color skin_color eye_color birth_year sex  
#>   <chr>  <chr>      <int> <dbl> <chr>      <chr>      <chr>          <dbl> <chr>
#> 1 Luke … Mimiland     172    77 blond      fair       blue              19 male 
#> # ℹ 2 more variables: gender <chr>, species <chr>

## Retrieve selected columns for a row specified using the index
## "retrieval" function with labile_data FALSE shows no change
retrieve_original_starwars("Luke Skywalker", where(is.atomic), homeworld)
#> # A tibble: 1 × 11
#>   name   homeworld height  mass hair_color skin_color eye_color birth_year sex  
#>   <chr>  <chr>      <int> <dbl> <chr>      <chr>      <chr>          <dbl> <chr>
#> 1 Luke … Tatooine     172    77 blond      fair       blue              19 male 
#> # ℹ 2 more variables: gender <chr>, species <chr>

rm(retrieve_starwars, retrieve_original_starwars, `replace_at_name<-`, starwars)
```
