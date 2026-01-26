# Remove Sequentially Numbered Objects from Workspace

Remove a series of sequentially named objects from the workspace or from
another specified `environment`. For example, conveniently remove a
series of sequentially numbered models.

## Usage

``` r
rm_objects(basename, suffixes, envir = parent.frame())
```

## Arguments

- basename:

  Common base name (quoted or unquoted) of the series of objects.

- suffixes:

  A numeric or character vector representing the suffixes of the series
  of objects.

- envir:

  An [`environment`](https://rdrr.io/r/base/environment.html) from which
  to remove objects. Use `.GlobalEnv` for the workspace; default
  [`parent.frame()`](https://rdrr.io/r/base/sys.parent.html) i.e., the
  `environment` in which `rm_objects()` was called.

## Value

A character vector of matching names remaining in the calling
[`environment`](https://rdrr.io/r/base/environment.html), usually the
workspace unless `rm_objects()` was called within a function, or another
specified `environment`, returned invisibly.

## Details

`rm_objects()` lists all objects in the workspace (or another specified
[`environment`](https://rdrr.io/r/base/environment.html)) whose names
start with `basename`, then removes any in which `basename` is followed
by an element included in `suffixes`, and finally lists all remaining
objects with names matching `basename`.

\#GF

## See also

[`environment`](https://rdrr.io/r/base/environment.html),
[`ls()`](https://rdrr.io/r/base/ls.html) and
[`rm()`](https://rdrr.io/r/base/rm.html).

## Examples

``` r
 ## Note: running outside example() will be more informative

 ## Create some sequentially numbered objects
 model1 <- model2 <- model3 <- model4 <- lm(1~1)
 ls(pattern = "model")
#> [1] "model1" "model2" "model3" "model4"

 ## Remove three of them
 rm_objects(model, 1:3)
#> Objects matching "model…" found in (unnamed) environment: –
#>   model1 model2 model3 model4 
#> Objects matching "model…" remaining in (unnamed) environment: –
#>   model4 

 ## Create some sequentially named objects
 model_a <- model_b <- model_c <- model_d <- lm(1~1)
 ls(pattern = "model_")
#> [1] "model_a" "model_b" "model_c" "model_d"

 ## Remove three of them
 rm_objects(model_, letters[1:3])
#> Objects matching "model_…" found in (unnamed) environment: –
#>   model_a model_b model_c model_d 
#> Objects matching "model_…" remaining in (unnamed) environment: –
#>   model_d 

 ## Use within a function
 (\() {                  ## Anonymous function, but doesn't have to be
   model1 <- model2 <- model3 <- model4 <- model5 <- lm(1~1)
   rm_objects(model, 1:5)
 })()
#> Objects matching "model…" found in (unnamed) environment: –
#>   model1 model2 model3 model4 model5 
#> Objects matching "model…" remaining in (unnamed) environment: –
#>   All gone! 

 ls(pattern = "model")
#> [1] "model4"  "model_d"

 rm_objects(model, c(4, "_d"))
#> Objects matching "model…" found in (unnamed) environment: –
#>   model4 model_d 
#> Objects matching "model…" remaining in (unnamed) environment: –
#>   All gone! 
```
