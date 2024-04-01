
# BitsnBobs
### R Package For General Bits and Bobs of Code

**Author:** Mark C. Eisler

**eMail:** Mark.Eisler@bristol.ac.uk

**ORCID** = [0000-0001-6843-3345](https://orcid.org/0000-0001-6843-3345)

## Installation

You can install the development version of BitsnBobs from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("Mark-Eis/BitsnBobs")
```
	
### BitsnBobs Package Description: –
The **BitsnBobs R package** does this and that using functions: –

`boxcox3()`, `browse()`, `cat_names()`, `cc_rate()`, `cohens_kappa()`, `const()`, `cor_coef.test()`, `count_lgl()`,
`data_wizard()`, `decdeg()`, `decdeg_to_dms()`, `degminsec()`, `design_effect()`, `detective()`,
[`detective<-()`](https://mark-eis.github.io/BitsnBobs/reference/detective.html), `dms_to_decdeg()`, `eff_rate()`,
`endstop()`, `endstop_data()`, `facet_histo()`,`fct_to_num()`, `formul_pwrseq()`, `get_amortint()`, `get_amortval()`,
`iterate()`, `j()`, `j2eff_rate()`, `known_s3generics()`, `kwd_cols()`, `kwd_cols<-()`, `latlon()`, `latlon_dd()`,
`lf()`, `lgl_cols()`, `list_lgl()`, `logit()`, `ls_all()`, `marker()`, `method_info()`, `nom_rate()`, `opt_bc()`,
`phi_coef()`, `phi_coef.test()`, `power_seq()`, `print_all()`, `print_lf()`, `prob_from_logit()`, `recursive()`,
`remplacer()`, `retriever()`, `revmat()`, `rm_objects()`, `s3_in_namespace()`, `s3debug()`, `s3mag7()`, `sample_size()`,
`skew()`,`skew.test()`, `skewness()`, `skewness.test()`, `split_to_cols()`, `split_to_rows()`, `starsig()`, `sum_lgl()`,
`wizard()`;

infix operators: –

[`op-min-max`](https://mark-eis.github.io/BitsnBobs/reference/op-min-max.html);

and datasets: –

[`heights`](https://mark-eis.github.io/BitsnBobs/reference/heights.html), [`litter_sizes`](https://mark-eis.github.io/BitsnBobs/reference/litter_sizes.html), [`starwars2`](https://mark-eis.github.io/BitsnBobs/reference/starwars2.html) and [`starwars3`](https://mark-eis.github.io/BitsnBobs/reference/starwars3.html).

## Example

This is a basic example which shows you how to use a few of the BitsnBobs functions: –

``` r
library(BitsnBobs)
## Use dplyr::starwars data
starwars <- dplyr::starwars

## Extract and sort unique values of the "homeworld" column in the starwars data
starwars |> wizard(homeworld)
## …and paste them into a character string.
starwars |> wizard(homeworld, ", ")

## Find strings containing the pattern "Darth" in the starwars "name" column
starwars |> detective(name, .pattern = "Darth")
## Modify strings containing the pattern "Darth" but not "Vader" in the "name" column
starwars |> detective(name, .pattern = "Darth", .exclude = "Vader") <- "Darth The First"
## Find strings containing the pattern "Darth" in the revised data in descending order
starwars |> detective(name, .pattern = "Darth", .arrange_by = desc(name))

## Create a "retrieval" function for the starwars data frame using "name" as index
retrieve_starwars <- retriever(starwars, name)
## … and retrieve selected columns for row(s) specified using the "name" index
retrieve_starwars("Luke Skywalker", ends_with("color"), homeworld)

## Create a replacement function using "name" as index
`replace_at_name<-` <- remplacer(name)
## Replace the value in the "homeworld" column for row(s) specified using the "name" index
starwars |> replace_at_name("Luke Skywalker", homeworld) <- "Mimiland"
## Retrieve selected columns for row(s) specified using the "name" index
retrieve_starwars("Luke Skywalker", ends_with("color"), homeworld)
```

