# Info Attribute of utils::Methods() Function

Information on available methods for an S3 and S4 generic function, or
all methods for an S3 or S4 class.

## Usage

``` r
method_info(..., .arrange_by = across(everything()))
```

## Arguments

- ...:

  Arguments passed on to
  [`utils::methods`](https://rdrr.io/r/utils/methods.html)

  `generic.function`

  :   a generic function, or a character string naming a generic
      function.

  `class`

  :   a symbol or character string naming a class: only used if
      `generic.function` is not supplied.

- .arrange_by:

  \<[`data-masking`](https://rlang.r-lib.org/reference/args_data_masking.html)\>
  names of columns or functions for ordering results using the syntax of
  [dplyr](https://dplyr.tidyverse.org/reference/dplyr-package.html)
  [`arrange()`](https://dplyr.tidyverse.org/reference/arrange.html). Use
  [`desc()`](https://dplyr.tidyverse.org/reference/desc.html) to sort by
  a variable in descending order.

## Value

A [`data.frame`](https://rdrr.io/r/base/data.frame.html) with class
attributes `c("info_df", "catapult", "data.frame")` and the following
columns: -

- Method:

  `character vector` of the S3 method names obtained by pasting the
  generic function and class together.

- Visible:

  `logical`, is the method exported from the namespace of the package in
  which it is defined?

- From:

  `factor`, the location or package name where the method was found.

- Generic:

  `character vector` of the names of the generic.

- isS4:

  `logical`, true when the method is an S4 method.

## Details

`method_info()` provides information on available methods for an S3 and
S4 [`generic function`](https://rdrr.io/r/base/InternalMethods.html), or
all methods for an S3 or S4 class obtained from the `"info"` attribute
returned by [`methods()`](https://rdrr.io/r/utils/methods.html), which
is more informative than the usual simple `character vector` printed
output.

The
[`print_all()`](https://mark-eis.github.io/BitsnBobs/reference/print_all.md)
S3 method for class `MethodsFunction` works similarly and is used
internally by `method_info()`.

## See also

[`class`](https://rdrr.io/r/base/class.html),
[`getS3method`](https://rdrr.io/r/utils/getS3method.html),
[`methods()`](https://rdrr.io/r/utils/methods.html),
[`print_all.MethodsFunction()`](https://mark-eis.github.io/BitsnBobs/reference/print_all.md)
and [`UseMethod`](https://rdrr.io/r/base/UseMethod.html)

Other methods:
[`S3Gen_Meth`](https://mark-eis.github.io/BitsnBobs/reference/S3Gen_Meth.md),
[`S3_Method_Debug`](https://mark-eis.github.io/BitsnBobs/reference/S3_Method_Debug.md)

## Examples

``` r
methods(summary)
#>  [1] summary.Date                        summary.POSIXct                    
#>  [3] summary.POSIXlt                     summary.aov                        
#>  [5] summary.aovlist*                    summary.aspell*                    
#>  [7] summary.check_packages_in_dir*      summary.connection                 
#>  [9] summary.data.frame                  summary.default                    
#> [11] summary.difftime                    summary.ecdf*                      
#> [13] summary.factor                      summary.ggplot2::ggplot*           
#> [15] summary.glm                         summary.infl*                      
#> [17] summary.lm                          summary.loess*                     
#> [19] summary.loglm*                      summary.manova                     
#> [21] summary.matrix                      summary.mlm*                       
#> [23] summary.negbin*                     summary.nls*                       
#> [25] summary.packageStatus*              summary.polr*                      
#> [27] summary.ppr*                        summary.prcomp*                    
#> [29] summary.princomp*                   summary.proc_time                  
#> [31] summary.rlang:::list_of_conditions* summary.rlang_error*               
#> [33] summary.rlang_message*              summary.rlang_trace*               
#> [35] summary.rlang_warning*              summary.rlm*                       
#> [37] summary.srcfile                     summary.srcref                     
#> [39] summary.stepfun                     summary.stl*                       
#> [41] summary.table                       summary.tukeysmooth*               
#> [43] summary.vctrs_sclr*                 summary.vctrs_vctr*                
#> [45] summary.warnings                   
#> see '?methods' for accessing help and source code
method_info(summary)
#> _____________
#> S3 Methods: -
#> 
#>  Method                             Visible From                           
#>  summary.Date                        TRUE   base                           
#>  summary.POSIXct                     TRUE   base                           
#>  summary.POSIXlt                     TRUE   base                           
#>  summary.aov                         TRUE   stats                          
#>  summary.aovlist                    FALSE   registered S3method for summary
#>  summary.aspell                     FALSE   registered S3method for summary
#>  summary.check_packages_in_dir      FALSE   registered S3method for summary
#>  summary.connection                  TRUE   base                           
#>  summary.data.frame                  TRUE   base                           
#>  summary.default                     TRUE   base                           
#>  summary.difftime                    TRUE   base                           
#>  summary.ecdf                       FALSE   registered S3method for summary
#>  summary.factor                      TRUE   base                           
#>  summary.ggplot2::ggplot            FALSE   registered S3method for summary
#>  summary.glm                         TRUE   stats                          
#>  summary.infl                       FALSE   registered S3method for summary
#>  summary.lm                          TRUE   stats                          
#>  summary.loess                      FALSE   registered S3method for summary
#>  summary.loglm                      FALSE   registered S3method for summary
#>  summary.manova                      TRUE   stats                          
#>  summary.matrix                      TRUE   base                           
#>  summary.mlm                        FALSE   registered S3method for summary
#>  summary.negbin                     FALSE   registered S3method for summary
#>  summary.nls                        FALSE   registered S3method for summary
#>  summary.packageStatus              FALSE   registered S3method for summary
#>  summary.polr                       FALSE   registered S3method for summary
#>  summary.ppr                        FALSE   registered S3method for summary
#>  summary.prcomp                     FALSE   registered S3method for summary
#>  summary.princomp                   FALSE   registered S3method for summary
#>  summary.proc_time                   TRUE   base                           
#>  summary.rlang:::list_of_conditions FALSE   registered S3method for summary
#>  summary.rlang_error                FALSE   registered S3method for summary
#>  summary.rlang_message              FALSE   registered S3method for summary
#>  summary.rlang_trace                FALSE   registered S3method for summary
#>  summary.rlang_warning              FALSE   registered S3method for summary
#>  summary.rlm                        FALSE   registered S3method for summary
#>  summary.srcfile                     TRUE   base                           
#>  summary.srcref                      TRUE   base                           
#>  summary.stepfun                     TRUE   stats                          
#>  summary.stl                        FALSE   registered S3method for summary
#>  summary.table                       TRUE   base                           
#>  summary.tukeysmooth                FALSE   registered S3method for summary
#>  summary.vctrs_sclr                 FALSE   registered S3method for summary
#>  summary.vctrs_vctr                 FALSE   registered S3method for summary
#>  summary.warnings                    TRUE   base                           
#>  Generic isS4 
#>  summary FALSE
#>  summary FALSE
#>  summary FALSE
#>  summary FALSE
#>  summary FALSE
#>  summary FALSE
#>  summary FALSE
#>  summary FALSE
#>  summary FALSE
#>  summary FALSE
#>  summary FALSE
#>  summary FALSE
#>  summary FALSE
#>  summary FALSE
#>  summary FALSE
#>  summary FALSE
#>  summary FALSE
#>  summary FALSE
#>  summary FALSE
#>  summary FALSE
#>  summary FALSE
#>  summary FALSE
#>  summary FALSE
#>  summary FALSE
#>  summary FALSE
#>  summary FALSE
#>  summary FALSE
#>  summary FALSE
#>  summary FALSE
#>  summary FALSE
#>  summary FALSE
#>  summary FALSE
#>  summary FALSE
#>  summary FALSE
#>  summary FALSE
#>  summary FALSE
#>  summary FALSE
#>  summary FALSE
#>  summary FALSE
#>  summary FALSE
#>  summary FALSE
#>  summary FALSE
#>  summary FALSE
#>  summary FALSE
#>  summary FALSE
methods(summary) |> print_all()
#> _____________
#> S3 Methods: -
#> 
#>  Method                             Visible From                           
#>  summary.Date                        TRUE   base                           
#>  summary.POSIXct                     TRUE   base                           
#>  summary.POSIXlt                     TRUE   base                           
#>  summary.aov                         TRUE   stats                          
#>  summary.aovlist                    FALSE   registered S3method for summary
#>  summary.aspell                     FALSE   registered S3method for summary
#>  summary.check_packages_in_dir      FALSE   registered S3method for summary
#>  summary.connection                  TRUE   base                           
#>  summary.data.frame                  TRUE   base                           
#>  summary.default                     TRUE   base                           
#>  summary.difftime                    TRUE   base                           
#>  summary.ecdf                       FALSE   registered S3method for summary
#>  summary.factor                      TRUE   base                           
#>  summary.ggplot2::ggplot            FALSE   registered S3method for summary
#>  summary.glm                         TRUE   stats                          
#>  summary.infl                       FALSE   registered S3method for summary
#>  summary.lm                          TRUE   stats                          
#>  summary.loess                      FALSE   registered S3method for summary
#>  summary.loglm                      FALSE   registered S3method for summary
#>  summary.manova                      TRUE   stats                          
#>  summary.matrix                      TRUE   base                           
#>  summary.mlm                        FALSE   registered S3method for summary
#>  summary.negbin                     FALSE   registered S3method for summary
#>  summary.nls                        FALSE   registered S3method for summary
#>  summary.packageStatus              FALSE   registered S3method for summary
#>  summary.polr                       FALSE   registered S3method for summary
#>  summary.ppr                        FALSE   registered S3method for summary
#>  summary.prcomp                     FALSE   registered S3method for summary
#>  summary.princomp                   FALSE   registered S3method for summary
#>  summary.proc_time                   TRUE   base                           
#>  summary.rlang:::list_of_conditions FALSE   registered S3method for summary
#>  summary.rlang_error                FALSE   registered S3method for summary
#>  summary.rlang_message              FALSE   registered S3method for summary
#>  summary.rlang_trace                FALSE   registered S3method for summary
#>  summary.rlang_warning              FALSE   registered S3method for summary
#>  summary.rlm                        FALSE   registered S3method for summary
#>  summary.srcfile                     TRUE   base                           
#>  summary.srcref                      TRUE   base                           
#>  summary.stepfun                     TRUE   stats                          
#>  summary.stl                        FALSE   registered S3method for summary
#>  summary.table                       TRUE   base                           
#>  summary.tukeysmooth                FALSE   registered S3method for summary
#>  summary.vctrs_sclr                 FALSE   registered S3method for summary
#>  summary.vctrs_vctr                 FALSE   registered S3method for summary
#>  summary.warnings                    TRUE   base                           
#>  Generic isS4 
#>  summary FALSE
#>  summary FALSE
#>  summary FALSE
#>  summary FALSE
#>  summary FALSE
#>  summary FALSE
#>  summary FALSE
#>  summary FALSE
#>  summary FALSE
#>  summary FALSE
#>  summary FALSE
#>  summary FALSE
#>  summary FALSE
#>  summary FALSE
#>  summary FALSE
#>  summary FALSE
#>  summary FALSE
#>  summary FALSE
#>  summary FALSE
#>  summary FALSE
#>  summary FALSE
#>  summary FALSE
#>  summary FALSE
#>  summary FALSE
#>  summary FALSE
#>  summary FALSE
#>  summary FALSE
#>  summary FALSE
#>  summary FALSE
#>  summary FALSE
#>  summary FALSE
#>  summary FALSE
#>  summary FALSE
#>  summary FALSE
#>  summary FALSE
#>  summary FALSE
#>  summary FALSE
#>  summary FALSE
#>  summary FALSE
#>  summary FALSE
#>  summary FALSE
#>  summary FALSE
#>  summary FALSE
#>  summary FALSE
#>  summary FALSE

methods(class = "glm")
#>  [1] add1           anova          coerce         confint        cooks.distance
#>  [6] deviance       drop1          effects        extractAIC     family        
#> [11] formula        influence      initialize     logLik         model.frame   
#> [16] nobs           predict        print          profile        residuals     
#> [21] rstandard      rstudent       show           sigma          slotsFromS3   
#> [26] summary        vcov           weights       
#> see '?methods' for accessing help and source code
method_info(class = "glm")
#> _____________
#> S3 Methods: -
#> 
#>  Method                      Visible From                Generic        isS4 
#>  add1.glm                    FALSE   registered S3method add1           FALSE
#>  anova.glm                   FALSE   registered S3method anova          FALSE
#>  coerce,oldClass,S3-method    TRUE                       coerce          TRUE
#>  confint.glm                 FALSE   registered S3method confint        FALSE
#>  cooks.distance.glm          FALSE   registered S3method cooks.distance FALSE
#>  deviance.glm                FALSE   registered S3method deviance       FALSE
#>  drop1.glm                   FALSE   registered S3method drop1          FALSE
#>  effects.glm                 FALSE   registered S3method effects        FALSE
#>  extractAIC.glm              FALSE   registered S3method extractAIC     FALSE
#>  family.glm                  FALSE   registered S3method family         FALSE
#>  formula.glm                 FALSE   registered S3method formula        FALSE
#>  influence.glm               FALSE   registered S3method influence      FALSE
#>  initialize,oldClass-method   TRUE                       initialize      TRUE
#>  logLik.glm                  FALSE   registered S3method logLik         FALSE
#>  model.frame.glm             FALSE   registered S3method model.frame    FALSE
#>  nobs.glm                    FALSE   registered S3method nobs           FALSE
#>  predict.glm                  TRUE   stats               predict        FALSE
#>  print.glm                   FALSE   registered S3method print          FALSE
#>  profile.glm                 FALSE   registered S3method profile        FALSE
#>  residuals.glm                TRUE   stats               residuals      FALSE
#>  rstandard.glm               FALSE   registered S3method rstandard      FALSE
#>  rstudent.glm                FALSE   registered S3method rstudent       FALSE
#>  show,oldClass-method         TRUE                       show            TRUE
#>  sigma.glm                   FALSE   registered S3method sigma          FALSE
#>  slotsFromS3,oldClass-method  TRUE                       slotsFromS3     TRUE
#>  summary.glm                  TRUE   stats               summary        FALSE
#>  vcov.glm                    FALSE   registered S3method vcov           FALSE
#>  weights.glm                 FALSE   registered S3method weights        FALSE
method_info(class = "glm", .arrange_by = across(c(isS4, Method)))
#> _____________
#> S3 Methods: -
#> 
#>  Method                      Visible From                Generic        isS4 
#>  add1.glm                    FALSE   registered S3method add1           FALSE
#>  anova.glm                   FALSE   registered S3method anova          FALSE
#>  confint.glm                 FALSE   registered S3method confint        FALSE
#>  cooks.distance.glm          FALSE   registered S3method cooks.distance FALSE
#>  deviance.glm                FALSE   registered S3method deviance       FALSE
#>  drop1.glm                   FALSE   registered S3method drop1          FALSE
#>  effects.glm                 FALSE   registered S3method effects        FALSE
#>  extractAIC.glm              FALSE   registered S3method extractAIC     FALSE
#>  family.glm                  FALSE   registered S3method family         FALSE
#>  formula.glm                 FALSE   registered S3method formula        FALSE
#>  influence.glm               FALSE   registered S3method influence      FALSE
#>  logLik.glm                  FALSE   registered S3method logLik         FALSE
#>  model.frame.glm             FALSE   registered S3method model.frame    FALSE
#>  nobs.glm                    FALSE   registered S3method nobs           FALSE
#>  predict.glm                  TRUE   stats               predict        FALSE
#>  print.glm                   FALSE   registered S3method print          FALSE
#>  profile.glm                 FALSE   registered S3method profile        FALSE
#>  residuals.glm                TRUE   stats               residuals      FALSE
#>  rstandard.glm               FALSE   registered S3method rstandard      FALSE
#>  rstudent.glm                FALSE   registered S3method rstudent       FALSE
#>  sigma.glm                   FALSE   registered S3method sigma          FALSE
#>  summary.glm                  TRUE   stats               summary        FALSE
#>  vcov.glm                    FALSE   registered S3method vcov           FALSE
#>  weights.glm                 FALSE   registered S3method weights        FALSE
#>  coerce,oldClass,S3-method    TRUE                       coerce          TRUE
#>  initialize,oldClass-method   TRUE                       initialize      TRUE
#>  show,oldClass-method         TRUE                       show            TRUE
#>  slotsFromS3,oldClass-method  TRUE                       slotsFromS3     TRUE
methods(class = "glm") |> print_all()
#> _____________
#> S3 Methods: -
#> 
#>  Method                      Visible From                Generic        isS4 
#>  add1.glm                    FALSE   registered S3method add1           FALSE
#>  anova.glm                   FALSE   registered S3method anova          FALSE
#>  coerce,oldClass,S3-method    TRUE                       coerce          TRUE
#>  confint.glm                 FALSE   registered S3method confint        FALSE
#>  cooks.distance.glm          FALSE   registered S3method cooks.distance FALSE
#>  deviance.glm                FALSE   registered S3method deviance       FALSE
#>  drop1.glm                   FALSE   registered S3method drop1          FALSE
#>  effects.glm                 FALSE   registered S3method effects        FALSE
#>  extractAIC.glm              FALSE   registered S3method extractAIC     FALSE
#>  family.glm                  FALSE   registered S3method family         FALSE
#>  formula.glm                 FALSE   registered S3method formula        FALSE
#>  influence.glm               FALSE   registered S3method influence      FALSE
#>  initialize,oldClass-method   TRUE                       initialize      TRUE
#>  logLik.glm                  FALSE   registered S3method logLik         FALSE
#>  model.frame.glm             FALSE   registered S3method model.frame    FALSE
#>  nobs.glm                    FALSE   registered S3method nobs           FALSE
#>  predict.glm                  TRUE   stats               predict        FALSE
#>  print.glm                   FALSE   registered S3method print          FALSE
#>  profile.glm                 FALSE   registered S3method profile        FALSE
#>  residuals.glm                TRUE   stats               residuals      FALSE
#>  rstandard.glm               FALSE   registered S3method rstandard      FALSE
#>  rstudent.glm                FALSE   registered S3method rstudent       FALSE
#>  show,oldClass-method         TRUE                       show            TRUE
#>  sigma.glm                   FALSE   registered S3method sigma          FALSE
#>  slotsFromS3,oldClass-method  TRUE                       slotsFromS3     TRUE
#>  summary.glm                  TRUE   stats               summary        FALSE
#>  vcov.glm                    FALSE   registered S3method vcov           FALSE
#>  weights.glm                 FALSE   registered S3method weights        FALSE
methods(class = "glm") |> print_all(.arrange_by = across(c(isS4, Method)))
#> _____________
#> S3 Methods: -
#> 
#>  Method                      Visible From                Generic        isS4 
#>  add1.glm                    FALSE   registered S3method add1           FALSE
#>  anova.glm                   FALSE   registered S3method anova          FALSE
#>  confint.glm                 FALSE   registered S3method confint        FALSE
#>  cooks.distance.glm          FALSE   registered S3method cooks.distance FALSE
#>  deviance.glm                FALSE   registered S3method deviance       FALSE
#>  drop1.glm                   FALSE   registered S3method drop1          FALSE
#>  effects.glm                 FALSE   registered S3method effects        FALSE
#>  extractAIC.glm              FALSE   registered S3method extractAIC     FALSE
#>  family.glm                  FALSE   registered S3method family         FALSE
#>  formula.glm                 FALSE   registered S3method formula        FALSE
#>  influence.glm               FALSE   registered S3method influence      FALSE
#>  logLik.glm                  FALSE   registered S3method logLik         FALSE
#>  model.frame.glm             FALSE   registered S3method model.frame    FALSE
#>  nobs.glm                    FALSE   registered S3method nobs           FALSE
#>  predict.glm                  TRUE   stats               predict        FALSE
#>  print.glm                   FALSE   registered S3method print          FALSE
#>  profile.glm                 FALSE   registered S3method profile        FALSE
#>  residuals.glm                TRUE   stats               residuals      FALSE
#>  rstandard.glm               FALSE   registered S3method rstandard      FALSE
#>  rstudent.glm                FALSE   registered S3method rstudent       FALSE
#>  sigma.glm                   FALSE   registered S3method sigma          FALSE
#>  summary.glm                  TRUE   stats               summary        FALSE
#>  vcov.glm                    FALSE   registered S3method vcov           FALSE
#>  weights.glm                 FALSE   registered S3method weights        FALSE
#>  coerce,oldClass,S3-method    TRUE                       coerce          TRUE
#>  initialize,oldClass-method   TRUE                       initialize      TRUE
#>  show,oldClass-method         TRUE                       show            TRUE
#>  slotsFromS3,oldClass-method  TRUE                       slotsFromS3     TRUE
```
