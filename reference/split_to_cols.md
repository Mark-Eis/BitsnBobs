# Split Strings In A Data Frame Character Column Into New Rows or Columns

Duplicate columns or rows in a data frame by splitting strings in a
character column at a given pattern to disaggregate data.

## Usage

``` r
split_to_cols(data, col_to_split, split_to, pattern, remove_parenth = FALSE)

split_to_rows(data, col_to_split, pattern)
```

## Arguments

- data:

  a data frame, or a data frame extension (e.g. a tibble).

- col_to_split:

  \<[`data-masking`](https://rlang.r-lib.org/reference/args_data_masking.html)\>
  quoted name of character column to be split.

- split_to:

  \<[`data-masking`](https://rlang.r-lib.org/reference/args_data_masking.html)\>
  quoted name of a new character column to be created in `data`.

- pattern:

  a single character string representing the pattern to split by.
  `split_to_rows()` allows use of the various possibilities for the
  `pattern` argument of
  [`str_split`](https://stringr.tidyverse.org/reference/str_split.html).

- remove_parenth:

  `logical` specifying whether parentheses should be removed from the
  strings in the new column `split_to`; default `TRUE`.

## Value

A [`tibble`](https://tibble.tidyverse.org/reference/tibble.html) data
frame based on `data`. With `split_to_cols()`, it has the same rows but
different columns and with `split_to_rows()`, it has the same columns
but different rows.

## Details

Similar to the more sophisticated
[tidyr](https://tidyr.tidyverse.org/reference/tidyr-package.html)
function
[`separate_wider_delim`](https://tidyr.tidyverse.org/reference/separate_wider_delim.html)
on which it is based, `split_to_cols()` trims leading and trailing
whitespace using
[`str_trim`](https://stringr.tidyverse.org/reference/str_trim.html) and
also optionally removes parentheses from the string in the newly created
column `split_to`. This was the main motivation for creating this
function, which can be used for conveniently disaggregating addenda to
strings contained in parentheses, see examples. The trade-off is that
unlike `separate_wider_delim()`, function `split_to_cols()` only handles
a single split.

BitsnBobs's homegrown `split_to_rows()` succeeds with
`pattern = "\\r\\n"` representing the CRLF escape sequence that results
for example, from an imported Excel file sporting within-cell line
breaks, whereas the otherwise similar but more sophisticated
[tidyr](https://tidyr.tidyverse.org/reference/tidyr-package.html)
function
[`separate_longer_delim()`](https://tidyr.tidyverse.org/reference/separate_longer_delim.html)
does not\*. Like `separate_longer_delim()`, function `split_to_rows()`
handles multiple splits.

Both `split_to_rows()` and `split_to_cols()` also add the convenience of
trimming leading and trailing whitespaces from the split strings using
[`str_trim`](https://stringr.tidyverse.org/reference/str_trim.html),
whereas the
[tidyr](https://tidyr.tidyverse.org/reference/tidyr-package.html)
functions do not afford this luxury.

\*Note that `separate_longer_delim()` does succeed with the equivalent
CRLF hex sequence `"\u000D\u000A"`; trimming leading and trailing
whitespaces may be `split_to_rows`'s main advantage.

## See also

[`separate_longer_delim`](https://tidyr.tidyverse.org/reference/separate_longer_delim.html),
[`separate_wider_delim`](https://tidyr.tidyverse.org/reference/separate_wider_delim.html),
[`str_split`](https://stringr.tidyverse.org/reference/str_split.html)
and [`str_trim`](https://stringr.tidyverse.org/reference/str_trim.html).

## Examples

``` r

starwars3
#> # A tibble: 10 × 2
#>    name                  skin_color         
#>    <chr>                 <chr>              
#>  1 Ackbar                brown mottle       
#>  2 Ben Quadinaros        grey, green, yellow
#>  3 Gasgano               white, blue        
#>  4 Grievous              brown, white       
#>  5 Jabba Desilijic Tiure green-tan, brown   
#>  6 Nute Gunray           mottled green      
#>  7 R2-D2                 white, blue        
#>  8 R4-P17                silver, red        
#>  9 Shaak Ti              red, blue, white   
#> 10 Zam Wesell            fair, green, yellow
split_to_rows(starwars3, skin_color, ",")
#> # A tibble: 21 × 2
#>    name                  skin_color  
#>    <chr>                 <chr>       
#>  1 Ackbar                brown mottle
#>  2 Ben Quadinaros        grey        
#>  3 Ben Quadinaros        green       
#>  4 Ben Quadinaros        yellow      
#>  5 Gasgano               white       
#>  6 Gasgano               blue        
#>  7 Grievous              brown       
#>  8 Grievous              white       
#>  9 Jabba Desilijic Tiure green-tan   
#> 10 Jabba Desilijic Tiure brown       
#> # ℹ 11 more rows
tidyr::separate_longer_delim(starwars3, skin_color, ",")
#> # A tibble: 21 × 2
#>    name                  skin_color    
#>    <chr>                 <chr>         
#>  1 Ackbar                "brown mottle"
#>  2 Ben Quadinaros        "grey"        
#>  3 Ben Quadinaros        " green"      
#>  4 Ben Quadinaros        " yellow"     
#>  5 Gasgano               "white"       
#>  6 Gasgano               " blue"       
#>  7 Grievous              "brown"       
#>  8 Grievous              " white"      
#>  9 Jabba Desilijic Tiure "green-tan"   
#> 10 Jabba Desilijic Tiure " brown"      
#> # ℹ 11 more rows

split_to_cols(starwars3, skin_color, alt_skin_color, ",")
#> # A tibble: 10 × 3
#>    name                  skin_color    alt_skin_color
#>    <chr>                 <chr>         <chr>         
#>  1 Ackbar                brown mottle  NA            
#>  2 Ben Quadinaros        grey          green, yellow 
#>  3 Gasgano               white         blue          
#>  4 Grievous              brown         white         
#>  5 Jabba Desilijic Tiure green-tan     brown         
#>  6 Nute Gunray           mottled green NA            
#>  7 R2-D2                 white         blue          
#>  8 R4-P17                silver        red           
#>  9 Shaak Ti              red           blue, white   
#> 10 Zam Wesell            fair          green, yellow 
tidyr::separate_wider_delim(
    starwars3, skin_color, ",",
    names = c("skin_color", "alt_skin_color"),
    too_few = "align_start",
    too_many = "merge"
)
#> # A tibble: 10 × 3
#>    name                  skin_color    alt_skin_color  
#>    <chr>                 <chr>         <chr>           
#>  1 Ackbar                brown mottle   NA             
#>  2 Ben Quadinaros        grey          " green, yellow"
#>  3 Gasgano               white         " blue"         
#>  4 Grievous              brown         " white"        
#>  5 Jabba Desilijic Tiure green-tan     " brown"        
#>  6 Nute Gunray           mottled green  NA             
#>  7 R2-D2                 white         " blue"         
#>  8 R4-P17                silver        " red"          
#>  9 Shaak Ti              red           " blue, white"  
#> 10 Zam Wesell            fair          " green, yellow"

starwars3[3, 2]$skin_color <- "white\r\nblue"
starwars3[8, 2]$skin_color <- "silver\r\nred"
starwars3
#> # A tibble: 10 × 2
#>    name                  skin_color           
#>    <chr>                 <chr>                
#>  1 Ackbar                "brown mottle"       
#>  2 Ben Quadinaros        "grey, green, yellow"
#>  3 Gasgano               "white\r\nblue"      
#>  4 Grievous              "brown, white"       
#>  5 Jabba Desilijic Tiure "green-tan, brown"   
#>  6 Nute Gunray           "mottled green"      
#>  7 R2-D2                 "white, blue"        
#>  8 R4-P17                "silver\r\nred"      
#>  9 Shaak Ti              "red, blue, white"   
#> 10 Zam Wesell            "fair, green, yellow"

split_to_rows(starwars3, skin_color, "\\r\\n")
#> # A tibble: 12 × 2
#>    name                  skin_color         
#>    <chr>                 <chr>              
#>  1 Ackbar                brown mottle       
#>  2 Ben Quadinaros        grey, green, yellow
#>  3 Gasgano               white              
#>  4 Gasgano               blue               
#>  5 Grievous              brown, white       
#>  6 Jabba Desilijic Tiure green-tan, brown   
#>  7 Nute Gunray           mottled green      
#>  8 R2-D2                 white, blue        
#>  9 R4-P17                silver             
#> 10 R4-P17                red                
#> 11 Shaak Ti              red, blue, white   
#> 12 Zam Wesell            fair, green, yellow
tidyr::separate_longer_delim(starwars3, skin_color, "\\r\\n")
#> # A tibble: 10 × 2
#>    name                  skin_color           
#>    <chr>                 <chr>                
#>  1 Ackbar                "brown mottle"       
#>  2 Ben Quadinaros        "grey, green, yellow"
#>  3 Gasgano               "white\r\nblue"      
#>  4 Grievous              "brown, white"       
#>  5 Jabba Desilijic Tiure "green-tan, brown"   
#>  6 Nute Gunray           "mottled green"      
#>  7 R2-D2                 "white, blue"        
#>  8 R4-P17                "silver\r\nred"      
#>  9 Shaak Ti              "red, blue, white"   
#> 10 Zam Wesell            "fair, green, yellow"

starwars3[3, 2]$skin_color <- "white (blue)"
starwars3[4, 2]$skin_color <- "brown, [white]"
starwars3[5, 2]$skin_color <- "green-tan, {brown}"
starwars3[8, 2]$skin_color <- "white (red]"
starwars3
#> # A tibble: 10 × 2
#>    name                  skin_color         
#>    <chr>                 <chr>              
#>  1 Ackbar                brown mottle       
#>  2 Ben Quadinaros        grey, green, yellow
#>  3 Gasgano               white (blue)       
#>  4 Grievous              brown, [white]     
#>  5 Jabba Desilijic Tiure green-tan, {brown} 
#>  6 Nute Gunray           mottled green      
#>  7 R2-D2                 white, blue        
#>  8 R4-P17                white (red]        
#>  9 Shaak Ti              red, blue, white   
#> 10 Zam Wesell            fair, green, yellow

split_to_cols(starwars3, skin_color, alt_skin_color, " ")
#> # A tibble: 10 × 3
#>    name                  skin_color alt_skin_color
#>    <chr>                 <chr>      <chr>         
#>  1 Ackbar                brown      mottle        
#>  2 Ben Quadinaros        grey,      green, yellow 
#>  3 Gasgano               white      (blue)        
#>  4 Grievous              brown,     [white]       
#>  5 Jabba Desilijic Tiure green-tan, {brown}       
#>  6 Nute Gunray           mottled    green         
#>  7 R2-D2                 white,     blue          
#>  8 R4-P17                white      (red]         
#>  9 Shaak Ti              red,       blue, white   
#> 10 Zam Wesell            fair,      green, yellow 
split_to_cols(starwars3, skin_color, alt_skin_color, " ", remove_parenth = TRUE)
#> # A tibble: 10 × 3
#>    name                  skin_color alt_skin_color
#>    <chr>                 <chr>      <chr>         
#>  1 Ackbar                brown      mottle        
#>  2 Ben Quadinaros        grey,      green, yellow 
#>  3 Gasgano               white      blue          
#>  4 Grievous              brown,     white         
#>  5 Jabba Desilijic Tiure green-tan, brown         
#>  6 Nute Gunray           mottled    green         
#>  7 R2-D2                 white,     blue          
#>  8 R4-P17                white      red           
#>  9 Shaak Ti              red,       blue, white   
#> 10 Zam Wesell            fair,      green, yellow 
tidyr::separate_wider_delim(
    starwars3, skin_color, " ",
    names = c("skin_color", "alt_skin_color"),
    too_few = "align_start",
    too_many = "merge"
)
#> # A tibble: 10 × 3
#>    name                  skin_color alt_skin_color
#>    <chr>                 <chr>      <chr>         
#>  1 Ackbar                brown      mottle        
#>  2 Ben Quadinaros        grey,      green, yellow 
#>  3 Gasgano               white      (blue)        
#>  4 Grievous              brown,     [white]       
#>  5 Jabba Desilijic Tiure green-tan, {brown}       
#>  6 Nute Gunray           mottled    green         
#>  7 R2-D2                 white,     blue          
#>  8 R4-P17                white      (red]         
#>  9 Shaak Ti              red,       blue, white   
#> 10 Zam Wesell            fair,      green, yellow 
```
