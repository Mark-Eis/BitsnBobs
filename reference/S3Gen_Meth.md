# Tables of S3 Generic Functions and Methods

`known_s3generics()` tabulates "known" S3 generic functions.

`s3_in_namespace()` tabulates S3 `methods` in a loaded `namespace`.

## Usage

``` r
known_s3generics(.arrange_by = across(Generic:Namespace))

s3_in_namespace(namespace, .arrange_by = Method)

# S3 method for class 'info_df'
print(x, ...)
```

## Arguments

- .arrange_by:

  \<[`data-masking`](https://rlang.r-lib.org/reference/args_data_masking.html)\>
  names of columns or functions for ordering results using the syntax of
  [dplyr](https://dplyr.tidyverse.org/reference/dplyr-package.html)
  [`arrange()`](https://dplyr.tidyverse.org/reference/arrange.html). Use
  [`desc()`](https://dplyr.tidyverse.org/reference/desc.html) to sort by
  a variable in descending order.

- namespace:

  (unquoted) name of a `namespace`.

## Value

A [`data.frame`](https://rdrr.io/r/base/data.frame.html) with class
attributes `c("info_df", "catapult", "data.frame")` and columns
`Generic` and `Namespace` for `known_s3generics()`, and `Generic`,
`Class` and `Method` for `s3_in_namespace()`, as follows: -

- Generic:

  `character`, the generic function name.

- Namespace:

  `character`, the namespace environment of the generic or `"primitive"`
  in the case of `primitive` (internally implemented) generics.

- Class:

  `character`, the `class` for the method.

- Method:

  `character`, the full method name.

## Details

`known_s3generics()` returns a `data.frame` of "known"
[`S3 generic functions`](https://rdrr.io/r/base/InternalMethods.html),
based on two `character vectors` in the
[base](https://rdrr.io/r/base/base-package.html) package `namespace`,
namely `.knownS3Generics` and
[`.S3PrimitiveGenerics`](https://rdrr.io/r/base/InternalMethods.html).

`s3_in_namespace()` returns a `data.frame` of
[`S3 methods`](https://rdrr.io/r/base/UseMethod.html) in a loaded
`namespace`.

## References

R Internals:
[`1.2.2 Namespaces`](https://cran.r-project.org/doc/manuals/r-devel/R-ints.html#Namespaces).

## See also

`.knownS3Generics`,
[`.S3PrimitiveGenerics`](https://rdrr.io/r/base/InternalMethods.html)
[`class`](https://rdrr.io/r/base/class.html),
[`getS3method`](https://rdrr.io/r/utils/getS3method.html),
`loadNamespace`, [`methods`](https://rdrr.io/r/utils/methods.html) and
[`UseMethod`](https://rdrr.io/r/base/UseMethod.html).

Other methods:
[`S3_Method_Debug`](https://mark-eis.github.io/BitsnBobs/reference/S3_Method_Debug.md),
[`method_info()`](https://mark-eis.github.io/BitsnBobs/reference/method_info.md)

## Examples

``` r
known_s3generics()
#> _______________________________
#> "Known" S3 Generic Functions: -
#> 
#>  Generic        Namespace  
#>  AIC            stats      
#>  Complex        base       
#>  Math           base       
#>  Ops            base       
#>  Summary        base       
#>  add1           stats      
#>  anova          stats      
#>  anyNA          "primitive"
#>  as.call        "primitive"
#>  as.character   "primitive"
#>  as.character   base       
#>  as.complex     "primitive"
#>  as.data.frame  base       
#>  as.double      "primitive"
#>  as.environment "primitive"
#>  as.environment base       
#>  as.integer     "primitive"
#>  as.logical     "primitive"
#>  as.matrix      base       
#>  as.numeric     "primitive"
#>  as.raw         "primitive"
#>  as.vector      base       
#>  biplot         stats      
#>  c              "primitive"
#>  cbind          base       
#>  coef           stats      
#>  confint        stats      
#>  contour        graphics   
#>  deviance       stats      
#>  df.residual    stats      
#>  dim            "primitive"
#>  dim<-          "primitive"
#>  dimnames       "primitive"
#>  dimnames<-     "primitive"
#>  drop1          stats      
#>  edit           utils      
#>  extractAIC     stats      
#>  fitted         stats      
#>  formula        stats      
#>  hist           graphics   
#>  identify       graphics   
#>  image          graphics   
#>  is.array       "primitive"
#>  is.finite      "primitive"
#>  is.infinite    "primitive"
#>  is.matrix      "primitive"
#>  is.na          "primitive"
#>  is.nan         "primitive"
#>  is.numeric     "primitive"
#>  labels         base       
#>  length         "primitive"
#>  length<-       "primitive"
#>  levels<-       "primitive"
#>  lines          graphics   
#>  log10          "primitive"
#>  log2           "primitive"
#>  logLik         stats      
#>  matrixOps      base       
#>  model.frame    stats      
#>  model.matrix   stats      
#>  names          "primitive"
#>  names<-        "primitive"
#>  pairs          graphics   
#>  plot           base       
#>  points         graphics   
#>  predict        stats      
#>  print          base       
#>  profile        stats      
#>  qqnorm         stats      
#>  rbind          base       
#>  rep            "primitive"
#>  rep            base       
#>  residuals      stats      
#>  se.contrast    stats      
#>  seq            base       
#>  seq.int        "primitive"
#>  seq.int        base       
#>  sequence       base       
#>  solve          base       
#>  str            utils      
#>  summary        base       
#>  t              base       
#>  terms          stats      
#>  text           graphics   
#>  update         stats      
#>  vcov           stats      
#>  xtfrm          "primitive"
known_s3generics(.arrange_by = across(Namespace:Generic))
#> _______________________________
#> "Known" S3 Generic Functions: -
#> 
#>  Generic        Namespace  
#>  anyNA          "primitive"
#>  as.call        "primitive"
#>  as.character   "primitive"
#>  as.complex     "primitive"
#>  as.double      "primitive"
#>  as.environment "primitive"
#>  as.integer     "primitive"
#>  as.logical     "primitive"
#>  as.numeric     "primitive"
#>  as.raw         "primitive"
#>  c              "primitive"
#>  dim            "primitive"
#>  dim<-          "primitive"
#>  dimnames       "primitive"
#>  dimnames<-     "primitive"
#>  is.array       "primitive"
#>  is.finite      "primitive"
#>  is.infinite    "primitive"
#>  is.matrix      "primitive"
#>  is.na          "primitive"
#>  is.nan         "primitive"
#>  is.numeric     "primitive"
#>  length         "primitive"
#>  length<-       "primitive"
#>  levels<-       "primitive"
#>  log10          "primitive"
#>  log2           "primitive"
#>  names          "primitive"
#>  names<-        "primitive"
#>  rep            "primitive"
#>  seq.int        "primitive"
#>  xtfrm          "primitive"
#>  Complex        base       
#>  Math           base       
#>  Ops            base       
#>  Summary        base       
#>  as.character   base       
#>  as.data.frame  base       
#>  as.environment base       
#>  as.matrix      base       
#>  as.vector      base       
#>  cbind          base       
#>  labels         base       
#>  matrixOps      base       
#>  plot           base       
#>  print          base       
#>  rbind          base       
#>  rep            base       
#>  seq            base       
#>  seq.int        base       
#>  sequence       base       
#>  solve          base       
#>  summary        base       
#>  t              base       
#>  contour        graphics   
#>  hist           graphics   
#>  identify       graphics   
#>  image          graphics   
#>  lines          graphics   
#>  pairs          graphics   
#>  points         graphics   
#>  text           graphics   
#>  AIC            stats      
#>  add1           stats      
#>  anova          stats      
#>  biplot         stats      
#>  coef           stats      
#>  confint        stats      
#>  deviance       stats      
#>  df.residual    stats      
#>  drop1          stats      
#>  extractAIC     stats      
#>  fitted         stats      
#>  formula        stats      
#>  logLik         stats      
#>  model.frame    stats      
#>  model.matrix   stats      
#>  predict        stats      
#>  profile        stats      
#>  qqnorm         stats      
#>  residuals      stats      
#>  se.contrast    stats      
#>  terms          stats      
#>  update         stats      
#>  vcov           stats      
#>  edit           utils      
#>  str            utils      

s3_in_namespace(BitsnBobs)
#> ____________________________________
#> S3 Methods in Namespace BitsnBobs: -
#> 
#>  Generic       Class          
#>  sum_degminsec decdeg         
#>  sum_degminsec degmin         
#>  sum_degminsec degminsec      
#>  sum_minsec    decdeg         
#>  sum_minsec    degmin         
#>  sum_minsec    degminsec      
#>  sum_sec       decdeg         
#>  sum_sec       degmin         
#>  sum_sec       degminsec      
#>  as_coord      coord          
#>  as_coord      numeric        
#>  as_coord      waypoint       
#>  as.double     coord          
#>  as.double     decdeg         
#>  as.double     degmin         
#>  as.double     degminsec      
#>  format        coord          
#>  format        coordpart      
#>  format        waypoint       
#>  print         catapult       
#>  print         coord          
#>  print         info_df        
#>  print         rostido        
#>  print         waypoint       
#>  print_all     MethodsFunction
#>  print_all     cohens_kappa   
#>  print_all     data.frame     
#>  print_all     default        
#>  print_all     htest          
#>  print_all     tbl            
#>  print_all     tbl_df         
#>  rbind         rostido        
#>  sum_degminsec decdeg         
#>  sum_degminsec decdeg         
#>  sum_degminsec degmin         
#>  sum_degminsec degminsec      
#>  sum_degminsec degmin         
#>  sum_degminsec degminsec      
#>  sum_minsec    decdeg         
#>  sum_minsec    decdeg         
#>  sum_minsec    degmin         
#>  sum_minsec    degmin         
#>  sum_minsec    degminsec      
#>  sum_minsec    degminsec      
#>  sum_sec       decdeg         
#>  sum_sec       degmin         
#>  sum_sec       decdeg         
#>  sum_sec       degmin         
#>  sum_sec       degminsec      
#>  sum_sec       degminsec      
#>  Method                                                                                                              
#>  sum_degminsec.decdeg                                                                                                
#>  sum_degminsec.degmin                                                                                                
#>  sum_degminsec.degminsec                                                                                             
#>  sum_minsec.decdeg                                                                                                   
#>  sum_minsec.degmin                                                                                                   
#>  sum_minsec.degminsec                                                                                                
#>  sum_sec.decdeg                                                                                                      
#>  sum_sec.degmin                                                                                                      
#>  sum_sec.degminsec                                                                                                   
#>  as_coord.coord                                                                                                      
#>  as_coord.numeric                                                                                                    
#>  as_coord.waypoint                                                                                                   
#>  as.double.coord                                                                                                     
#>  as.double.decdeg                                                                                                    
#>  as.double.degmin                                                                                                    
#>  as.double.degminsec                                                                                                 
#>  format.coord                                                                                                        
#>  format.coordpart                                                                                                    
#>  format.waypoint                                                                                                     
#>  print.catapult                                                                                                      
#>  print.coord                                                                                                         
#>  print.info_df                                                                                                       
#>  print.rostido                                                                                                       
#>  print.waypoint                                                                                                      
#>  print_all.MethodsFunction                                                                                           
#>  print_all.cohens_kappa                                                                                              
#>  print_all.data.frame                                                                                                
#>  print_all.default                                                                                                   
#>  print_all.htest                                                                                                     
#>  print_all.tbl                                                                                                       
#>  print_all.tbl_df                                                                                                    
#>  rbind.rostido                                                                                                       
#>  function (object, ...) , {,     check_dots_empty(),     as.numeric(with(object, deg)), }                            
#>  function (object, ...) , {,     check_dots_empty(),     as.numeric(with(object, deg)), }                            
#>  function (object, ...) , {,     check_dots_empty(),     as.numeric(with(object, deg + sum_minsec(object)/60)), }    
#>  function (object, ...) , {,     check_dots_empty(),     as.numeric(with(object, deg + sum_minsec(object)/60)), }    
#>  function (object, ...) , {,     check_dots_empty(),     as.numeric(with(object, deg + sum_minsec(object)/60)), }    
#>  function (object, ...) , {,     check_dots_empty(),     as.numeric(with(object, deg + sum_minsec(object)/60)), }    
#>  function (object, ...) , {,     check_dots_empty(),     sum_minsec_polish(with(object, deg%%1 * 60)), }             
#>  function (object, ...) , {,     check_dots_empty(),     sum_minsec_polish(with(object, deg%%1 * 60)), }             
#>  function (object, ...) , {,     check_dots_empty(),     sum_minsec_polish(with(object, min)), }                     
#>  function (object, ...) , {,     check_dots_empty(),     sum_minsec_polish(with(object, min)), }                     
#>  function (object, ...) , {,     check_dots_empty(),     sum_minsec_polish(with(object, min + sum_sec(object)/60)), }
#>  function (object, ...) , {,     check_dots_empty(),     sum_minsec_polish(with(object, min + sum_sec(object)/60)), }
#>  function (object, ...) , {,     check_dots_empty(),     sum_minsec(object)%%1 * 60, }                               
#>  function (object, ...) , {,     check_dots_empty(),     sum_minsec(object)%%1 * 60, }                               
#>  function (object, ...) , {,     check_dots_empty(),     sum_minsec(object)%%1 * 60, }                               
#>  function (object, ...) , {,     check_dots_empty(),     sum_minsec(object)%%1 * 60, }                               
#>  function (object, ...) , {,     check_dots_empty(),     as.numeric(with(object, sec)), }                            
#>  function (object, ...) , {,     check_dots_empty(),     as.numeric(with(object, sec)), }                            
s3_in_namespace(BitsnBobs, .arrange_by = across(Class:Generic))
#> ____________________________________
#> S3 Methods in Namespace BitsnBobs: -
#> 
#>  Generic       Class          
#>  sum_degminsec decdeg         
#>  sum_degminsec decdeg         
#>  sum_degminsec decdeg         
#>  sum_minsec    decdeg         
#>  sum_minsec    decdeg         
#>  sum_minsec    decdeg         
#>  sum_sec       decdeg         
#>  sum_sec       decdeg         
#>  sum_sec       decdeg         
#>  as.double     decdeg         
#>  sum_degminsec degmin         
#>  sum_degminsec degmin         
#>  sum_degminsec degmin         
#>  sum_minsec    degmin         
#>  sum_minsec    degmin         
#>  sum_minsec    degmin         
#>  sum_sec       degmin         
#>  sum_sec       degmin         
#>  sum_sec       degmin         
#>  as.double     degmin         
#>  sum_degminsec degminsec      
#>  sum_degminsec degminsec      
#>  sum_degminsec degminsec      
#>  sum_minsec    degminsec      
#>  sum_minsec    degminsec      
#>  sum_minsec    degminsec      
#>  sum_sec       degminsec      
#>  sum_sec       degminsec      
#>  sum_sec       degminsec      
#>  as.double     degminsec      
#>  as_coord      coord          
#>  as.double     coord          
#>  format        coord          
#>  print         coord          
#>  as_coord      numeric        
#>  as_coord      waypoint       
#>  format        waypoint       
#>  print         waypoint       
#>  format        coordpart      
#>  print         catapult       
#>  print         info_df        
#>  print         rostido        
#>  rbind         rostido        
#>  print_all     MethodsFunction
#>  print_all     cohens_kappa   
#>  print_all     data.frame     
#>  print_all     default        
#>  print_all     htest          
#>  print_all     tbl            
#>  print_all     tbl_df         
#>  Method                                                                                                              
#>  sum_degminsec.decdeg                                                                                                
#>  function (object, ...) , {,     check_dots_empty(),     as.numeric(with(object, deg)), }                            
#>  function (object, ...) , {,     check_dots_empty(),     as.numeric(with(object, deg)), }                            
#>  sum_minsec.decdeg                                                                                                   
#>  function (object, ...) , {,     check_dots_empty(),     sum_minsec_polish(with(object, deg%%1 * 60)), }             
#>  function (object, ...) , {,     check_dots_empty(),     sum_minsec_polish(with(object, deg%%1 * 60)), }             
#>  sum_sec.decdeg                                                                                                      
#>  function (object, ...) , {,     check_dots_empty(),     sum_minsec(object)%%1 * 60, }                               
#>  function (object, ...) , {,     check_dots_empty(),     sum_minsec(object)%%1 * 60, }                               
#>  as.double.decdeg                                                                                                    
#>  sum_degminsec.degmin                                                                                                
#>  function (object, ...) , {,     check_dots_empty(),     as.numeric(with(object, deg + sum_minsec(object)/60)), }    
#>  function (object, ...) , {,     check_dots_empty(),     as.numeric(with(object, deg + sum_minsec(object)/60)), }    
#>  sum_minsec.degmin                                                                                                   
#>  function (object, ...) , {,     check_dots_empty(),     sum_minsec_polish(with(object, min)), }                     
#>  function (object, ...) , {,     check_dots_empty(),     sum_minsec_polish(with(object, min)), }                     
#>  sum_sec.degmin                                                                                                      
#>  function (object, ...) , {,     check_dots_empty(),     sum_minsec(object)%%1 * 60, }                               
#>  function (object, ...) , {,     check_dots_empty(),     sum_minsec(object)%%1 * 60, }                               
#>  as.double.degmin                                                                                                    
#>  sum_degminsec.degminsec                                                                                             
#>  function (object, ...) , {,     check_dots_empty(),     as.numeric(with(object, deg + sum_minsec(object)/60)), }    
#>  function (object, ...) , {,     check_dots_empty(),     as.numeric(with(object, deg + sum_minsec(object)/60)), }    
#>  sum_minsec.degminsec                                                                                                
#>  function (object, ...) , {,     check_dots_empty(),     sum_minsec_polish(with(object, min + sum_sec(object)/60)), }
#>  function (object, ...) , {,     check_dots_empty(),     sum_minsec_polish(with(object, min + sum_sec(object)/60)), }
#>  sum_sec.degminsec                                                                                                   
#>  function (object, ...) , {,     check_dots_empty(),     as.numeric(with(object, sec)), }                            
#>  function (object, ...) , {,     check_dots_empty(),     as.numeric(with(object, sec)), }                            
#>  as.double.degminsec                                                                                                 
#>  as_coord.coord                                                                                                      
#>  as.double.coord                                                                                                     
#>  format.coord                                                                                                        
#>  print.coord                                                                                                         
#>  as_coord.numeric                                                                                                    
#>  as_coord.waypoint                                                                                                   
#>  format.waypoint                                                                                                     
#>  print.waypoint                                                                                                      
#>  format.coordpart                                                                                                    
#>  print.catapult                                                                                                      
#>  print.info_df                                                                                                       
#>  print.rostido                                                                                                       
#>  rbind.rostido                                                                                                       
#>  print_all.MethodsFunction                                                                                           
#>  print_all.cohens_kappa                                                                                              
#>  print_all.data.frame                                                                                                
#>  print_all.default                                                                                                   
#>  print_all.htest                                                                                                     
#>  print_all.tbl                                                                                                       
#>  print_all.tbl_df                                                                                                    

s3_in_namespace(utils) |> head(40)
#> ________________________________
#> S3 Methods in Namespace utils: -
#> 
#>  Generic       Class                  Method                       
#>  [             getAnywhere            [.getAnywhere                
#>  [             news_db                [.news_db                    
#>  [             roman                  [.roman                      
#>  Ops           roman                  Ops.roman                    
#>  Summary       roman                  Summary.roman                
#>  as.character  person                 as.character.person          
#>  as.character  roman                  as.character.roman           
#>  as.person     default                as.person.default            
#>  as.personList default                as.personList.default        
#>  as.personList person                 as.personList.person         
#>  bitstring     default                bitstring.default            
#>  bitstring     raw                    bitstring.raw                
#>  bitstring     integer                bitstring.integer            
#>  bitstring     numeric                bitstring.numeric            
#>  close         txtProgressBar         close.txtProgressBar         
#>  edit          data.frame             edit.data.frame              
#>  edit          default                edit.default                 
#>  edit          matrix                 edit.matrix                  
#>  edit          vignette               edit.vignette                
#>  format        MethodsFunction        format.MethodsFunction       
#>  format        bitstring              format.bitstring             
#>  format        aspell                 format.aspell                
#>  format        aspell_inspect_context format.aspell_inspect_context
#>  format        news_db                format.news_db               
#>  format        object_size            format.object_size           
#>  format        roman                  format.roman                 
#>  getRcode      vignette               getRcode.vignette            
#>  head          array                  head.array                   
#>  head          data.frame             head.array                   
#>  head          default                head.default                 
#>  head          function               head.function                
#>  head          matrix                 head.matrix                  
#>  head          ftable                 head.ftable                  
#>  print         aspell                 print.aspell                 
#>  print         aspell_inspect_context print.aspell_inspect_context 
#>  print         Bibtex                 print.Bibtex                 
#>  print         bitstring              print.bitstring              
#>  print         changedFiles           print.changedFiles           
#>  print         fileSnapshot           print.fileSnapshot           
#>  print         findLineNumResult      print.findLineNumResult      
s3_in_namespace(utils, .arrange_by = across(Class:Generic)) |> head(40)
#> ________________________________
#> S3 Methods in Namespace utils: -
#> 
#>  Generic       Class       Method               
#>  [             getAnywhere [.getAnywhere        
#>  print         getAnywhere print.getAnywhere    
#>  [             news_db     [.news_db            
#>  format        news_db     format.news_db       
#>  print         news_db     print.news_db        
#>  subset        news_db     subset.news_db       
#>  [             roman       [.roman              
#>  Ops           roman       Ops.roman            
#>  Summary       roman       Summary.roman        
#>  as.character  roman       as.character.roman   
#>  format        roman       format.roman         
#>  print         roman       print.roman          
#>  rep           roman       rep.roman            
#>  [             person      [.person             
#>  as.character  person      as.character.person  
#>  as.personList person      as.personList.person 
#>  format        person      format.person        
#>  print         person      print.person         
#>  rep           person      rep.person           
#>  toBibtex      person      toBibtex.person      
#>  $             person      $.person             
#>  $<-           person      $<-.person           
#>  [<-           person      [<-.person           
#>  [[            person      [[.person            
#>  [[<-          person      [[<-.person          
#>  as.data.frame person      as.data.frame.person 
#>  c             person      c.person             
#>  unique        person      unique.person        
#>  as.person     default     as.person.default    
#>  as.personList default     as.personList.default
#>  bitstring     default     bitstring.default    
#>  edit          default     edit.default         
#>  head          default     head.default         
#>  prompt        default     prompt.default       
#>  relist        default     relist.default       
#>  stack         default     stack.default        
#>  str           default     str.default          
#>  tail          default     tail.default         
#>  type.convert  default     type.convert.default 
#>  unstack       default     unstack.default      
```
