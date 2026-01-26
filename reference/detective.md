# Search for Pattern in a Data Frame Character Column

Find and modify strings containing a specified pattern in a data frame
character column.

## Usage

``` r
detective(.data, pattern, ..., .exclude = NULL, .arrange_by = desc(n))

detective(.data, pattern, ..., .exclude = NULL) <- value
```

## Arguments

- .data:

  a data frame, or a data frame extension (e.g. a
  [`tibble`](https://tibble.tidyverse.org/reference/tibble-package.html)).

- pattern:

  Pattern to look for.

  The default interpretation is a regular expression, as described in
  `vignette("regular-expressions")`. Use
  [`regex()`](https://stringr.tidyverse.org/reference/modifiers.html)
  for finer control of the matching behaviour.

  Match a fixed string (i.e. by comparing only bytes), using
  [`fixed()`](https://stringr.tidyverse.org/reference/modifiers.html).
  This is fast, but approximate. Generally, for matching human text,
  you'll want
  [`coll()`](https://stringr.tidyverse.org/reference/modifiers.html)
  which respects character matching rules for the specified locale.

  You can not match boundaries, including `""`, with this function.

- ...:

  \<[`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)\>
  `character` or `factor` columns to search and return.

- .exclude:

  a single `character` string signifying items to be excluded,
  interpreted as for `pattern`; default `NULL`.

- .arrange_by:

  \<[`data-masking`](https://rlang.r-lib.org/reference/args_data_masking.html)\>
  quoted name(s) of column(s) for ordering results. Use
  [`desc()`](https://dplyr.tidyverse.org/reference/desc.html) to sort by
  variables in descending order; default `desc(n)`.

- value:

  a single `character` string providing the replacement value.

## Value

`detective()` returns a
[`tibble`](https://tibble.tidyverse.org/reference/tibble-package.html)
with columns selected using ... and `n`, giving the count of occurences
of each item.

## Details

`detective()` finds and counts strings matching `pattern` but not
matching `.exclude` in selected columns in `.data`, while
`detective()<-` is the equivalent replacement function. Both functions
forms allow use of the various possibilities for the `pattern` argument
of
[`str_detect`](https://stringr.tidyverse.org/reference/str_detect.html).
Use `pattern = regex("xyz", ignore_case = TRUE)` for a case insensitive
search. Use [utils](https://rdrr.io/r/utils/utils-package.html) package
[`glob2rx()`](https://rdrr.io/r/utils/glob2rx.html) to change a wildcard
or globbing pattern into a regular expression.

`character` or `factor` columns in `.data` are selected using `...` with
the
\<[`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)\>
syntax of package
[dplyr](https://dplyr.tidyverse.org/reference/dplyr-package.html),
including use of **selection helpers**.

The output may be ordered by the values of selected columns using the
syntax of
[`arrange()`](https://dplyr.tidyverse.org/reference/arrange.html),
including use of
[`across()`](https://dplyr.tidyverse.org/reference/across.html) or
[`pick()`](https://dplyr.tidyverse.org/reference/pick.html) to select
columns with
\<[`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)\>
(see examples).

## See also

[`across()`](https://dplyr.tidyverse.org/reference/across.html),
[`arrange()`](https://dplyr.tidyverse.org/reference/arrange.html),
[`desc()`](https://dplyr.tidyverse.org/reference/desc.html),
[`glob2rx()`](https://rdrr.io/r/utils/glob2rx.html),
[`pick()`](https://dplyr.tidyverse.org/reference/pick.html) and
[`str_detect()`](https://stringr.tidyverse.org/reference/str_detect.html).

Other detective:
[`retriever()`](https://mark-eis.github.io/BitsnBobs/reference/retriever.md),
[`wizard()`](https://mark-eis.github.io/BitsnBobs/reference/wizard.md)

## Examples

``` r
## Find strings containing a specified pattern in a data frame
starwars |> detective("Sky", name)
#> # A tibble: 3 × 2
#>   name                 n
#>   <chr>            <int>
#> 1 Anakin Skywalker     1
#> 2 Luke Skywalker       1
#> 3 Shmi Skywalker       1

## Use regex() to make case insensitive
starwars |> detective(regex("WALKER", TRUE), name, .arrange_by = desc(name))
#> # A tibble: 3 × 2
#>   name                 n
#>   <chr>            <int>
#> 1 Shmi Skywalker       1
#> 2 Luke Skywalker       1
#> 3 Anakin Skywalker     1

## Use | for alternatives
starwars |> detective("Sky|Organa", name)
#> # A tibble: 5 × 2
#>   name                    n
#>   <chr>               <int>
#> 1 Anakin Skywalker        1
#> 2 Bail Prestor Organa     1
#> 3 Leia Organa             1
#> 4 Luke Skywalker          1
#> 5 Shmi Skywalker          1

## Replace strings containing a specified pattern
starwars |> detective("Darth", name)
#> # A tibble: 2 × 2
#>   name            n
#>   <chr>       <int>
#> 1 Darth Maul      1
#> 2 Darth Vader     1
starwars |> detective("Darth", name, .exclude = "Vader") <- "Darth The First"
starwars |> detective("Darth", name, .arrange_by = desc(name))
#> # A tibble: 2 × 2
#>   name                n
#>   <chr>           <int>
#> 1 Darth Vader         1
#> 2 Darth The First     1

## Exclude strings containing unwanted patterns 
starwars |> detective("Sky", name, .exclude = "Luke")
#> # A tibble: 2 × 2
#>   name                 n
#>   <chr>            <int>
#> 1 Anakin Skywalker     1
#> 2 Shmi Skywalker       1

## Return multiple columns 
starwars |> detective("Human", homeworld, species)
#> # A tibble: 15 × 3
#>    homeworld    species     n
#>    <chr>        <chr>   <int>
#>  1 Tatooine     Human       8
#>  2 NA           Human       6
#>  3 Naboo        Human       5
#>  4 Alderaan     Human       3
#>  5 Corellia     Human       2
#>  6 Coruscant    Human       2
#>  7 Bespin       Human       1
#>  8 Chandrila    Human       1
#>  9 Concord Dawn Human       1
#> 10 Eriadu       Human       1
#> 11 Haruun Kal   Human       1
#> 12 Kamino       Human       1
#> 13 Serenno      Human       1
#> 14 Socorro      Human       1
#> 15 Stewjon      Human       1
starwars |> detective("Human", homeworld, species, .exclude = "s")
#> # A tibble: 13 × 3
#>    homeworld    species     n
#>    <chr>        <chr>   <int>
#>  1 Tatooine     Human       8
#>  2 NA           Human       6
#>  3 Naboo        Human       5
#>  4 Alderaan     Human       3
#>  5 Corellia     Human       2
#>  6 Chandrila    Human       1
#>  7 Concord Dawn Human       1
#>  8 Eriadu       Human       1
#>  9 Haruun Kal   Human       1
#> 10 Kamino       Human       1
#> 11 Serenno      Human       1
#> 12 Socorro      Human       1
#> 13 Stewjon      Human       1
starwars |> detective("Human", homeworld, species, .exclude = regex("s", TRUE))
#> # A tibble: 10 × 3
#>    homeworld    species     n
#>    <chr>        <chr>   <int>
#>  1 Tatooine     Human       8
#>  2 NA           Human       6
#>  3 Naboo        Human       5
#>  4 Alderaan     Human       3
#>  5 Corellia     Human       2
#>  6 Chandrila    Human       1
#>  7 Concord Dawn Human       1
#>  8 Eriadu       Human       1
#>  9 Haruun Kal   Human       1
#> 10 Kamino       Human       1

## Select columns using <tidy-select> syntax from {dplyr},
## including use of “selection helpers”
starwars |> detective(
        "brown", contains("color"), species,
        .arrange_by = across(contains("color"))
    )
#> # A tibble: 25 × 5
#>    hair_color skin_color eye_color species     n
#>    <chr>      <chr>      <chr>     <chr>   <int>
#>  1 black      brown      brown     Zabrak      1
#>  2 black      dark       brown     Human       2
#>  3 black      dark       brown     NA          1
#>  4 black      fair       brown     Human       2
#>  5 black      light      brown     Human       1
#>  6 black      tan        brown     Human       2
#>  7 brown      brown      blue      Wookiee     1
#>  8 brown      brown      brown     Ewok        1
#>  9 brown      fair       blue      Human       3
#> 10 brown      fair       blue      NA          1
#> # ℹ 15 more rows

starwars |> detective(
        "brown", name, contains("color"), species,
        .exclude = "Human", .arrange_by = across(contains("color"))
    )
#> # A tibble: 12 × 6
#>    name                  hair_color skin_color       eye_color     species     n
#>    <chr>                 <chr>      <chr>            <chr>         <chr>   <int>
#>  1 Eeth Koth             black      brown            brown         Zabrak      1
#>  2 Gregar Typho          black      dark             brown         NA          1
#>  3 Tarfful               brown      brown            blue          Wookiee     1
#>  4 Wicket Systri Warrick brown      brown            brown         Ewok        1
#>  5 Jek Tono Porkins      brown      fair             blue          NA          1
#>  6 Cordé                 brown      light            brown         NA          1
#>  7 Chewbacca             brown      unknown          blue          Wookiee     1
#>  8 Dexter Jettster       none       brown            yellow        Besali…     1
#>  9 Ackbar                none       brown mottle     orange        Mon Ca…     1
#> 10 Grievous              none       brown, white     green, yellow Kaleesh     1
#> 11 Yoda                  white      green            brown         Yoda's…     1
#> 12 Jabba Desilijic Tiure NA         green-tan, brown orange        Hutt        1

starwars |> detective(
        "brown", contains("color"), species,
    ) <- "chestnut"

starwars |> detective("brown", name, contains("color"), species)
#> # A tibble: 0 × 6
#> # ℹ 6 variables: name <chr>, hair_color <chr>, skin_color <chr>,
#> #   eye_color <chr>, species <chr>, n <int>

starwars |> detective("chestnut", name, contains("color"), species)
#> # A tibble: 35 × 6
#>    name                hair_color skin_color eye_color species          n
#>    <chr>               <chr>      <chr>      <chr>     <chr>        <int>
#>  1 Ackbar              none       chestnut   orange    Mon Calamari     1
#>  2 Arvel Crynyd        chestnut   fair       chestnut  Human            1
#>  3 Bail Prestor Organa black      tan        chestnut  Human            1
#>  4 Beru Whitesun Lars  chestnut   light      blue      Human            1
#>  5 Biggs Darklighter   black      light      chestnut  Human            1
#>  6 Boba Fett           black      fair       chestnut  Human            1
#>  7 Chewbacca           chestnut   unknown    blue      Wookiee          1
#>  8 Cliegg Lars         chestnut   fair       blue      Human            1
#>  9 Cordé               chestnut   light      chestnut  NA               1
#> 10 Dexter Jettster     none       chestnut   yellow    Besalisk         1
#> # ℹ 25 more rows

## Use {utils} glob2rx() to create regular expression, in this instance 
## a wildcard * finding every character except a new line
starwars |> detective(glob2rx("*"), !c(name, contains("color")))
#> # A tibble: 65 × 5
#>    sex    gender    homeworld species      n
#>    <chr>  <chr>     <chr>     <chr>    <int>
#>  1 male   masculine Tatooine  Human        6
#>  2 male   masculine NA        Human        4
#>  3 male   masculine Naboo     Gungan       3
#>  4 male   masculine Naboo     Human        3
#>  5 female feminine  Mirial    Mirialan     2
#>  6 female feminine  Naboo     Human        2
#>  7 female feminine  Tatooine  Human        2
#>  8 female feminine  NA        Human        2
#>  9 male   masculine Alderaan  Human        2
#> 10 male   masculine Corellia  Human        2
#> # ℹ 55 more rows

## Equivalent using {stringr} regex(".")
identical(
    starwars |> detective(glob2rx("*"), !c(name, contains("color"))),
    starwars |> detective(regex("."), !c(name, contains("color")))
)
#> [1] TRUE

## Equivalent using caret "^" in pattern string
identical(
    starwars |> detective(glob2rx("*"), !c(name, contains("color"))),
    starwars |> detective("^", !c(name, contains("color")))
)
#> [1] TRUE

```
