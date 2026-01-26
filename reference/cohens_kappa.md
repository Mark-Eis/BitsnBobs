# Cohen's Kappa Coefficient of Agreement for Nominal Scales

Cohen's *kappa* measures the agreement between two raters (or diagnostic
tests) who each classify `N` items into `C` mutually exclusive
categories, typically two categories in the case of diagnostic tests.

[`print_all()`](https://mark-eis.github.io/BitsnBobs/reference/print_all.md)
for class `"cohens_kappa"` first prints the standard `htest` output,
then the matrices of observed and expected numbers with their marginal
sums, and lastly the numbers and proportions of observations agreeing.

## Usage

``` r
cohens_kappa(x, se_method = c("Fleiss", "Cohen"), conf.level = 0.95)

# S3 method for class 'cohens_kappa'
print_all(x, ...)
```

## Arguments

- x:

  a square `matrix` containing the observations of two raters or results
  of two diagnostic tests.

- se_method:

  a character string specifying the method of calculating the standard
  error; must be one of `"Fleiss"` (default), or `"Cohen"`. You can
  specify just the initial letter.

- conf.level:

  the confidence level required; default `0.95`.

- ...:

  further arguments passed to or from other methods.

## Value

A list with classes `"cohens_kappa"` and `"htest"`, containing the
following components: -

- statistic:

  \\\kappa\\, the kappa coefficient.

- parameter:

  the total number of observations.

- conf.int:

  confidence interval of \\\kappa\\ (95% or other specified level).

- estimate:

  the number of agreements observed and the number expected by chance.

- stderr:

  the standard error of \\\kappa\\.

- observed:

  the observed counts.

- expected:

  the expected counts under the null hypothesis of zero agreement.

- data.name:

  a character string giving the name of the data.

- method:

  the character string "Cohen's kappa coefficient of agreement" and the
  standard error method used.

## Details

Cohen's *kappa* coefficient is given by: -

\$\$\displaystyle \kappa = \frac{p\_{o}-p\_{e}}{1-p\_{e}}\$\$

where \\p\_{o}\\ is the proportion of observations in agreement and
\\p\_{e}\\ is the proportion of observations expected to agree by
chance.

Cohen's (1960) original approximation to the *standard error* of
\\\kappa\\ is given by: -

\$\$\displaystyle se =
\sqrt{\frac{p\_{o}(1-p\_{o})}{n(1-p\_{e})^{2}}}\$\$

Output returned by `cohens_kappa()` maybe printed using
[`print_all()`](https://mark-eis.github.io/BitsnBobs/reference/print_all.md)
to provide additional information, see examples.

## Note

Professor Joseph Fleiss *et al.* (1979) observed:
`Many human endeavors have been cursed with repeated failures before final success is achieved. The scaling of Mount Everest is one example. The discovery of the Northwest Passage is a second. The derivation of a correct standard error for kappa is a third.`
Coding the Fleiss *et al.* (1979) standard error method in R was an
endeavour similarly cursed!

## References

Cohen, J. (1960). A coefficient of agreement for nominal scales. *Educ
Psychol Meas*, **20**, 37–46.
[](https://doi.org/10.1177/001316446002000104)[doi:10.1177/001316446002000104](https://doi.org/10.1177/001316446002000104)
.

Fleiss, J.L., Nee, J.C., & Landis, J.R. (1979). Large sample variance of
kappa in the case of different sets of raters. *Psychol Bull*,
**86**(5), 974–977. https://doi.org/10.1037/0033-2909.86.5.974
[](https://psycnet.apa.org/record/1979-32706-001?doi=1)[doi:10.1037/0033-2909.86.5.974](https://doi.org/10.1037/0033-2909.86.5.974)
.

## See also

[`matrix`](https://rdrr.io/r/base/matrix.html),
[`mcnemar.test`](https://rdrr.io/r/stats/mcnemar.test.html)

## Examples

``` r
 ## Two-by-two table for diagnostic test comparison
 (twobytwo <- matrix(c(31, 12, 4, 58), nrow = 2, dimnames = rep(list(c("+ve", "-ve")), 2) |>
               setNames(c("Test1", "Test2"))
             ))
#>      Test2
#> Test1 +ve -ve
#>   +ve  31   4
#>   -ve  12  58

 (ck <- cohens_kappa(twobytwo))
#> 
#>  Cohen's kappa coefficient of agreement (Fleiss stderr)
#> 
#> data:  twobytwo
#> stderr = 0.073448, number of observations = 105
#> 95 percent confidence interval:
#>  0.5317210 0.8196303
#> sample estimates:
#>     kappa 
#> 0.6756757 
#> 

 ck |> print_all()   
#> 
#>  Cohen's kappa coefficient of agreement (Fleiss stderr)
#> 
#> data:  twobytwo
#> stderr = 0.073448, number of observations = 105
#> 95 percent confidence interval:
#>  0.5317210 0.8196303
#> sample estimates:
#>     kappa 
#> 0.6756757 
#> 
#> Observed: -
#>      Test2
#> Test1 +ve -ve Sum
#>   +ve  31   4  35
#>   -ve  12  58  70
#>   Sum  43  62 105
#> 
#> Expected: -
#>      Test2
#> Test1      +ve      -ve Sum
#>   +ve 14.33333 20.66667  35
#>   -ve 28.66667 41.33333  70
#>   Sum 43.00000 62.00000 105
#> 
#> Number of agreements: -
#> observed expected 
#> 89.00000 55.66667 
#> 
#> Proportion in agreement: -
#>  observed  expected 
#> 0.8476190 0.5301587 
#> 

 ## Example from Altman et al. (Statistics with Confidence 2nd Edn. 2008. ISBN:978-0-727-91375-3,
 ## p.117), using, as they did, Cohen's approximation to the standard error. 
 (twobytwo <- matrix(c(32, 3, 6, 42), nrow = 2, dimnames = rep(list(c("Yes", "No")), 2) |>
               setNames(c("Parent", "Paediatrician"))
             ))
#>       Paediatrician
#> Parent Yes No
#>    Yes  32  6
#>    No    3 42

 cohens_kappa(twobytwo, se_method = "Cohen") |> print_all()   
#> 
#>  Cohen's kappa coefficient of agreement (Cohen stderr)
#> 
#> data:  twobytwo
#> stderr = 0.069171, number of observations = 83
#> 95 percent confidence interval:
#>  0.6446565 0.9158024
#> sample estimates:
#>     kappa 
#> 0.7802295 
#> 
#> Observed: -
#>       Paediatrician
#> Parent Yes No Sum
#>    Yes  32  6  38
#>    No    3 42  45
#>    Sum  35 48  83
#> 
#> Expected: -
#>       Paediatrician
#> Parent     Yes      No Sum
#>    Yes 16.0241 21.9759  38
#>    No  18.9759 26.0241  45
#>    Sum 35.0000 48.0000  83
#> 
#> Number of agreements: -
#> observed expected 
#> 74.00000 42.04819 
#> 
#> Proportion in agreement: -
#>  observed  expected 
#> 0.8915663 0.5066047 
#> 

 ## Confidence interval using Fleiss et al.'s standard error for comparison
 cohens_kappa(twobytwo, se_method = "Fleiss") |> _$conf.int
#> [1] 0.6450640 0.9153949
#> attr(,"conf.level")
#> [1] 0.95

 ## Example with three categories from Cohen (1960).
 (threebythree <- matrix(c(88, 10,  2, 14, 40,  6, 18, 10, 12), nrow = 3,
                   dimnames = rep(list(c("Cat1", "Cat2", "Cat3")), 2) |>
                       setNames(c("Judge_B", "Judge_A"))
                 ))
#>        Judge_A
#> Judge_B Cat1 Cat2 Cat3
#>    Cat1   88   14   18
#>    Cat2   10   40   10
#>    Cat3    2    6   12

 cohens_kappa(threebythree, se_method = "Cohen") |> print_all()
#> 
#>  Cohen's kappa coefficient of agreement (Cohen stderr)
#> 
#> data:  threebythree
#> stderr = 0.054922, number of observations = 200
#> 95 percent confidence interval:
#>  0.3838812 0.5991696
#> sample estimates:
#>     kappa 
#> 0.4915254 
#> 
#> Observed: -
#>        Judge_A
#> Judge_B Cat1 Cat2 Cat3 Sum
#>    Cat1   88   14   18 120
#>    Cat2   10   40   10  60
#>    Cat3    2    6   12  20
#>    Sum   100   60   40 200
#> 
#> Expected: -
#>        Judge_A
#> Judge_B Cat1 Cat2 Cat3 Sum
#>    Cat1   60   36   24 120
#>    Cat2   30   18   12  60
#>    Cat3   10    6    4  20
#>    Sum   100   60   40 200
#> 
#> Number of agreements: -
#> observed expected 
#>      140       82 
#> 
#> Proportion in agreement: -
#> observed expected 
#>     0.70     0.41 
#> 

 ## Using Fleiss et al.'s standard error for comparison
 cohens_kappa(threebythree, se_method = "Fleiss") |> _$conf.int
#> [1] 0.3899498 0.5931011
#> attr(,"conf.level")
#> [1] 0.95

 rm(ck, threebythree, twobytwo)
```
