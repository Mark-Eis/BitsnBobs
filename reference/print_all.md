# Print All (or More) of an Object

`print_all()` is a generic function for extended printing of an object,
for instance printing all rows of a tibble, a derived class or even a
regular data frame, optionally following up by printing a specified
number of linefeeds. Being a generic function, new printing methods can
be easily added for a new [`class`](https://rdrr.io/r/base/class.html).

## Usage

``` r
print_all(x, ...)

# S3 method for class 'data.frame'
print_all(
  x,
  ...,
  linefeeds = NULL,
  digits = NULL,
  quote = FALSE,
  right = TRUE,
  row.names = TRUE,
  max = NULL
)

# S3 method for class 'tbl'
print_all(
  x,
  linefeeds = NULL,
  width = NULL,
  ...,
  max_extra_cols = NULL,
  max_footer_lines = NULL
)

# S3 method for class 'tbl_df'
print_all(
  x,
  linefeeds = NULL,
  width = NULL,
  ...,
  max_extra_cols = NULL,
  max_footer_lines = NULL
)

# S3 method for class 'htest'
print_all(x, ...)

# S3 method for class 'MethodsFunction'
print_all(x, ..., .arrange_by = across(everything()))
```

## Arguments

- x:

  An object such as a
  [`tibble`](https://tibble.tidyverse.org/reference/tibble-package.html)
  data frame.

- ...:

  further arguments passed to or from other methods.

- linefeeds:

  A positive integer specifying the number of linefeeds to follow up the
  printed output; default `NULL`.

- digits:

  minimal number of *significant* digits, see
  [`print.default`](https://rdrr.io/r/base/print.default.html).

- quote:

  logical, indicating whether or not strings should be printed with
  surrounding quotes.

- right:

  logical, indicating whether or not strings should be right aligned.

- row.names:

  logical (or character vector), indicating whether (or what) row names
  should be printed.

- max:

  numeric or `NULL`, specifying the maximal number of entries to be
  printed. By default, when `NULL`,
  [`getOption`](https://rdrr.io/r/base/options.html)`("max.print")`
  used.

- width:

  only used when `max.levels` is NULL, see above.

- max_extra_cols:

  Number of extra columns to print abbreviated information for, if the
  width is too small for the entire tibble. If `NULL`, the
  `max_extra_cols`
  [option](https://pillar.r-lib.org/reference/pillar_options.html) is
  used. The previously defined `n_extra` argument is soft-deprecated.

- max_footer_lines:

  Maximum number of footer lines. If `NULL`, the `max_footer_lines`
  [option](https://pillar.r-lib.org/reference/pillar_options.html) is
  used.

- .arrange_by:

  \<[`data-masking`](https://rlang.r-lib.org/reference/args_data_masking.html)\>
  names of columns or functions for ordering results using the syntax of
  [dplyr](https://dplyr.tidyverse.org/reference/dplyr-package.html)
  [`arrange()`](https://dplyr.tidyverse.org/reference/arrange.html). Use
  [`desc()`](https://dplyr.tidyverse.org/reference/desc.html) to sort by
  a variable in descending order.

## Value

Invisibly returns its argument.

## Details

For a
[`tibble`](https://tibble.tidyverse.org/reference/tibble-package.html)
`x`, `print_all(x)` is equivalent to `print(x, n = nrow(x))`, followed
up if required by `n` linefeeds generated as if by using
`cat(rep("\n", n))`.

The `linefeeds` argument may be useful within a piped sequence to
separate output from subsequent printing. If a vector of `length > 1` is
entered for `linefeeds`, only the first element will be used, and
negative integers will be converted to zero i.e., no line feeds.

## See also

[`print()`](https://rdrr.io/r/base/print.html),
[`print_lf()`](https://mark-eis.github.io/BitsnBobs/reference/lf.md),
[`method_info()`](https://mark-eis.github.io/BitsnBobs/reference/method_info.md),
[`methods()`](https://rdrr.io/r/utils/methods.html),
[`tibble`](https://tibble.tidyverse.org/reference/tibble-package.html).

Other print:
[`catapult()`](https://mark-eis.github.io/BitsnBobs/reference/catapult.md),
[`lf()`](https://mark-eis.github.io/BitsnBobs/reference/lf.md)

## Examples

``` r
(tib <- tibble(x = 1:26, y = LETTERS[x], z = paste0(x, y)))
#> # A tibble: 26 × 3
#>        x y     z    
#>    <int> <chr> <chr>
#>  1     1 A     1A   
#>  2     2 B     2B   
#>  3     3 C     3C   
#>  4     4 D     4D   
#>  5     5 E     5E   
#>  6     6 F     6F   
#>  7     7 G     7G   
#>  8     8 H     8H   
#>  9     9 I     9I   
#> 10    10 J     10J  
#> # ℹ 16 more rows
tib |> print_all()
#> # A tibble: 26 × 3
#>        x y     z    
#>    <int> <chr> <chr>
#>  1     1 A     1A   
#>  2     2 B     2B   
#>  3     3 C     3C   
#>  4     4 D     4D   
#>  5     5 E     5E   
#>  6     6 F     6F   
#>  7     7 G     7G   
#>  8     8 H     8H   
#>  9     9 I     9I   
#> 10    10 J     10J  
#> 11    11 K     11K  
#> 12    12 L     12L  
#> 13    13 M     13M  
#> 14    14 N     14N  
#> 15    15 O     15O  
#> 16    16 P     16P  
#> 17    17 Q     17Q  
#> 18    18 R     18R  
#> 19    19 S     19S  
#> 20    20 T     20T  
#> 21    21 U     21U  
#> 22    22 V     22V  
#> 23    23 W     23W  
#> 24    24 X     24X  
#> 25    25 Y     25Y  
#> 26    26 Z     26Z  
tib |> print_all() |> names()
#> # A tibble: 26 × 3
#>        x y     z    
#>    <int> <chr> <chr>
#>  1     1 A     1A   
#>  2     2 B     2B   
#>  3     3 C     3C   
#>  4     4 D     4D   
#>  5     5 E     5E   
#>  6     6 F     6F   
#>  7     7 G     7G   
#>  8     8 H     8H   
#>  9     9 I     9I   
#> 10    10 J     10J  
#> 11    11 K     11K  
#> 12    12 L     12L  
#> 13    13 M     13M  
#> 14    14 N     14N  
#> 15    15 O     15O  
#> 16    16 P     16P  
#> 17    17 Q     17Q  
#> 18    18 R     18R  
#> 19    19 S     19S  
#> 20    20 T     20T  
#> 21    21 U     21U  
#> 22    22 V     22V  
#> 23    23 W     23W  
#> 24    24 X     24X  
#> 25    25 Y     25Y  
#> 26    26 Z     26Z  
#> [1] "x" "y" "z"
tib |> print_all(linefeeds = 3) |> names()
#> # A tibble: 26 × 3
#>        x y     z    
#>    <int> <chr> <chr>
#>  1     1 A     1A   
#>  2     2 B     2B   
#>  3     3 C     3C   
#>  4     4 D     4D   
#>  5     5 E     5E   
#>  6     6 F     6F   
#>  7     7 G     7G   
#>  8     8 H     8H   
#>  9     9 I     9I   
#> 10    10 J     10J  
#> 11    11 K     11K  
#> 12    12 L     12L  
#> 13    13 M     13M  
#> 14    14 N     14N  
#> 15    15 O     15O  
#> 16    16 P     16P  
#> 17    17 Q     17Q  
#> 18    18 R     18R  
#> 19    19 S     19S  
#> 20    20 T     20T  
#> 21    21 U     21U  
#> 22    22 V     22V  
#> 23    23 W     23W  
#> 24    24 X     24X  
#> 25    25 Y     25Y  
#> 26    26 Z     26Z  
#> 
#>  
#>  
#> [1] "x" "y" "z"

df <- tib |> as.data.frame()
df |> print_all()                         ## Does nothing more than regular print()
#>     x y   z
#> 1   1 A  1A
#> 2   2 B  2B
#> 3   3 C  3C
#> 4   4 D  4D
#> 5   5 E  5E
#> 6   6 F  6F
#> 7   7 G  7G
#> 8   8 H  8H
#> 9   9 I  9I
#> 10 10 J 10J
#> 11 11 K 11K
#> 12 12 L 12L
#> 13 13 M 13M
#> 14 14 N 14N
#> 15 15 O 15O
#> 16 16 P 16P
#> 17 17 Q 17Q
#> 18 18 R 18R
#> 19 19 S 19S
#> 20 20 T 20T
#> 21 21 U 21U
#> 22 22 V 22V
#> 23 23 W 23W
#> 24 24 X 24X
#> 25 25 Y 25Y
#> 26 26 Z 26Z
df |> print_all(linefeeds = 2) |> names() ## Regular data frame printing, with line feeds
#>     x y   z
#> 1   1 A  1A
#> 2   2 B  2B
#> 3   3 C  3C
#> 4   4 D  4D
#> 5   5 E  5E
#> 6   6 F  6F
#> 7   7 G  7G
#> 8   8 H  8H
#> 9   9 I  9I
#> 10 10 J 10J
#> 11 11 K 11K
#> 12 12 L 12L
#> 13 13 M 13M
#> 14 14 N 14N
#> 15 15 O 15O
#> 16 16 P 16P
#> 17 17 Q 17Q
#> 18 18 R 18R
#> 19 19 S 19S
#> 20 20 T 20T
#> 21 21 U 21U
#> 22 22 V 22V
#> 23 23 W 23W
#> 24 24 X 24X
#> 25 25 Y 25Y
#> 26 26 Z 26Z
#> 
#>  
#> [1] "x" "y" "z"

rm(df, tib)
```
