# End String with Full Stop and no Other Punctuation or Spaces.

`endstop()` removes all punctuation and spaces from the end of a string
and optionally terminates the string with a full stop.

`endstop_data()` removes all punctuation and spaces from the end of
selected strings in `.data` and optionally terminates the strings with
full stops.

## Usage

``` r
endstop(string, .stop = TRUE)

endstop_data(.data, ..., .stop = TRUE)
```

## Arguments

- string:

  a character vector of length one.

- .stop:

  `logical`. Whether or not to add a full stop at the end of the string;
  default `TRUE`.

- .data:

  a data frame, or a data frame extension (e.g. a
  [`tibble`](https://tibble.tidyverse.org/reference/tibble-package.html)).

- ...:

  \<[`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)\>
  character columns in `.data` to `endstop`.

## Value

For `endstop()`, a character vector of length one, optionally
terminating in a full stop. For `endstop_data()`, a
[`tibble`](https://tibble.tidyverse.org/reference/tibble-package.html)
derived from `.data`, with selected character columns modified by
`endstop()`.

## Details

Uses
[`str_detect`](https://stringr.tidyverse.org/reference/str_detect.html)
from package
[stringr](https://stringr.tidyverse.org/reference/stringr-package.html)
to detect the regular expressions `'[:punct:]'` and `'[:space:]'`, and
[`str_sub`](https://stringr.tidyverse.org/reference/str_sub.html) to
modify the string.

For `endstop_data()`, character columns in `.data` are selected using
`...` with the
\<[`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)\>
syntax of package
[dplyr](https://dplyr.tidyverse.org/reference/dplyr-package.html),
including use of **selection helpers** and modified by `endstop()`. If
no character columns are selected in `...`, all character columns in
`.data` will be modified by `endstop()`.

## See also

[`str_detect()`](https://stringr.tidyverse.org/reference/str_detect.html),
[`str_sub()`](https://stringr.tidyverse.org/reference/str_sub.html)

Other utils:
[`const()`](https://mark-eis.github.io/BitsnBobs/reference/const.md),
[`marker()`](https://mark-eis.github.io/BitsnBobs/reference/marker.md),
[`op-min-max`](https://mark-eis.github.io/BitsnBobs/reference/op-min-max.md),
[`revmat()`](https://mark-eis.github.io/BitsnBobs/reference/revmat.md)

## Examples

``` r
"Mimiland" |> endstop()
#> [1] "Mimiland."
"Mimiland." |> endstop(FALSE)
#> [1] "Mimiland"
"Mimiland," |> endstop()
#> [1] "Mimiland."
"Mimiland ." |> endstop()
#> [1] "Mimiland."
"Mimiland. " |> endstop()
#> [1] "Mimiland."
s <- "Mimiland.!?\\(){}"
cat(s)
#> Mimiland.!?\(){}
endstop(s)
#> [1] "Mimiland."
s <- "Mimiland . ! ? \\ ( ) { } "
cat(s)
#> Mimiland . ! ? \ ( ) { } 
endstop(s, FALSE)
#> [1] "Mimiland"

starwars3 |> endstop_data(name)
#> # A tibble: 10 × 2
#>    name                   skin_color         
#>    <chr>                  <chr>              
#>  1 Ackbar.                brown mottle       
#>  2 Ben Quadinaros.        grey, green, yellow
#>  3 Gasgano.               white, blue        
#>  4 Grievous.              brown, white       
#>  5 Jabba Desilijic Tiure. green-tan, brown   
#>  6 Nute Gunray.           mottled green      
#>  7 R2-D2.                 white, blue        
#>  8 R4-P17.                silver, red        
#>  9 Shaak Ti.              red, blue, white   
#> 10 Zam Wesell.            fair, green, yellow
starwars3 |> endstop_data(starts_with("sk"))
#> # A tibble: 10 × 2
#>    name                  skin_color          
#>    <chr>                 <chr>               
#>  1 Ackbar                brown mottle.       
#>  2 Ben Quadinaros        grey, green, yellow.
#>  3 Gasgano               white, blue.        
#>  4 Grievous              brown, white.       
#>  5 Jabba Desilijic Tiure green-tan, brown.   
#>  6 Nute Gunray           mottled green.      
#>  7 R2-D2                 white, blue.        
#>  8 R4-P17                silver, red.        
#>  9 Shaak Ti              red, blue, white.   
#> 10 Zam Wesell            fair, green, yellow.
starwars3 |> endstop_data()
#> endstop_data(): no character variables selected in ...; processing all character variables in .data.
#> # A tibble: 10 × 2
#>    name                   skin_color          
#>    <chr>                  <chr>               
#>  1 Ackbar.                brown mottle.       
#>  2 Ben Quadinaros.        grey, green, yellow.
#>  3 Gasgano.               white, blue.        
#>  4 Grievous.              brown, white.       
#>  5 Jabba Desilijic Tiure. green-tan, brown.   
#>  6 Nute Gunray.           mottled green.      
#>  7 R2-D2.                 white, blue.        
#>  8 R4-P17.                silver, red.        
#>  9 Shaak Ti.              red, blue, white.   
#> 10 Zam Wesell.            fair, green, yellow.

rm(s)
```
