# Stars For Statistical Significance

Stars for statistical significance with levels as usual in R. A
vectorised function.

## Usage

``` r
starsig(p)
```

## Arguments

- p:

  A numeric vector of probabilities.

## Value

A character vector, length of `p`.

## Examples

``` r
(test_seq <- round(10 ^ seq(-4, 0, 0.5), 4))
#> [1] 0.0001 0.0003 0.0010 0.0032 0.0100 0.0316 0.1000 0.3162 1.0000

starsig(test_seq)
#> [1] *** *** **  **  *   *   NS  NS  NS 
#> Levels: *** ** * . NS

rbind(test_seq, as.character(starsig(test_seq)))
#>          [,1]    [,2]    [,3]    [,4]     [,5]   [,6]     [,7]  [,8]     [,9]
#> test_seq "1e-04" "3e-04" "0.001" "0.0032" "0.01" "0.0316" "0.1" "0.3162" "1" 
#>          "***"   "***"   "**"    "**"     "*"    "*"      "NS"  "NS"     "NS"

data.frame(val = test_seq, sig = starsig(test_seq))
#>      val sig
#> 1 0.0001 ***
#> 2 0.0003 ***
#> 3 0.0010  **
#> 4 0.0032  **
#> 5 0.0100   *
#> 6 0.0316   *
#> 7 0.1000  NS
#> 8 0.3162  NS
#> 9 1.0000  NS

rm(test_seq)
```
