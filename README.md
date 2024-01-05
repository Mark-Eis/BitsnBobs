
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
The ** BitsnBobs R package** does this and that using functions: –

boxcox3(), browse(), cat_names(), cc_rate(), cohens_kappa(), const(), cor_coef.test(), count_lgl(),
design_effect(), detective() detective()<-, eff_rate(), endstop(), endstop_data(), facet_histo(),
fct_to_num(), formul_pwrseq(), get_amortint(), get_amortval(), iterate(), j(), j2eff_rate(),
known_s3generics(), kwd_cols(), kwd_cols()<-, lf(), lgl_cols(), list_lgl(), logit(), ls_all(),
marker(), method_info(), nom_rate(), opt_bc(), phi_coef(), phi_coef.test(), power_seq(), print_all(),
print_lf(), prob_from_logit(), recursive(), remplacer(), retriever(), revmat(), rm_objects(),
s3_in_namespace(), s3debug(), s3mag7(), sample_size(), skew(), skew.test(), skewness(), skewness.test(),
split_to_cols(), split_to_rows(), starsig(), sum_lgl(), wizard();

infix operators: –

op-min-max;

and datasets: –

heights, litter_sizes, starwars2 and starwars3.

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(BitsnBobs)
## Extract and sort unique values of a selected column from a data frame
starwars |> wizard(homeworld)
## …and optionally paste into a character string.
starwars |> wizard(homeworld, ", '")

## Find and modify strings containing a specified pattern in a data frame character column
starwars |> detective(name, .pattern = "Darth")
starwars |> detective(name, .pattern = "Darth", .exclude = "Vader") <- "Darth The First"
starwars |> detective(name, .pattern = "Darth", .arrange_by = desc(name))

## Create a "retrieval" function
retrieve_starwars <- retriever(starwars, name)
## … and retrieve selected columns for a row specified using the index
retrieve_starwars("Luke Skywalker", ends_with("color"), homeworld)

## Create replacement function
`remplace_at_name<-` <- remplacer(name)
## Replace the value of a selected column  for a row specified using the index
remplace_at_name(starwars, "Luke Skywalker", homeworld) <- "Mimiland"
## Retrieve selected columns for a row specified using the index
retrieve_starwars("Luke Skywalker", ends_with("color"), homeworld)
```

