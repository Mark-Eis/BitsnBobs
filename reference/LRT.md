# Likelihood ratio test statistic

`LRT()` is a generic function for the likelihood ratio test statistic
from a given object.

## Usage

``` r
LRT(object, ...)

# S3 method for class 'coxph'
LRT(object, ...)

# S3 method for class 'coxme'
LRT(object, ..., type = c("integrated", "penalized"))
```

## Arguments

- object:

  any object from which a likelihood ratio test statistic, or a
  contribution to a likelihood ratio test statistic, can be extracted.

- ...:

  further arguments passed to or from other methods.

- type:

  character, which of the two types of partial likelihood to extract,
  either "penalized" or "integrated".

## Value

Returns the likelihood ratio statistic as a `double`.

## Details

Currently implemented for `"coxph"` and `"coxme"` objects.

## References

[Terry Therneau, 2024: Mixed Effects Cox
Models](https://cran.r-project.org/web/packages/coxme/vignettes/coxme.pdf).

## See also

[`coxph()`](https://rdrr.io/pkg/survival/man/coxph.html), `coxme()`,
`logLik.coxme()`.

## Examples

``` r
if (require("coxme", quietly = TRUE)) {
    efit1 <- coxph(Surv(y, uncens) ~ trt, eortc)
    efit2 <- coxme(Surv(y, uncens) ~ trt + (1|center), eortc)
    
    summary(efit1) |> print_lf()
    cat("LRT(efit1):", LRT(efit1), "\n\n")

    efit2 |> print_lf()
    list(LRT(efit2), LRT(efit2, type = "penalized"))
} else {
    message("R packages coxme and survival must both be installed to run example.")
}
#> R packages coxme and survival must both be installed to run example.
```
