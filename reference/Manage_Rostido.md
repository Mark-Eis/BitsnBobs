# Manage Rostido Bank Account Transaction Data

Functions to facilitate formatting and combining CSV transaction data
downloaded from the Rostido website.

`as_rostido()` reformats a data frame containing downloaded Rostido Bank
transaction data.

## Usage

``` r
as_rostido(data, dateformat = "%d/%m/%Y")

# S3 method for class 'rostido'
rbind(..., .arrange_by = NULL)

# S3 method for class 'rostido'
print(x, ..., .include = NULL, maxwidth = 65L)
```

## Arguments

- data:

  data frame, as returned by
  [`read_rostido_csv()`](https://mark-eis.github.io/BitsnBobs/reference/Import_Rostido.md).

- dateformat:

  `character` string, passed as the `format` argument to
  [`as.Date()`](https://rdrr.io/r/base/as.Date.html); default
  `"%d/%m/%Y"`.

- ...:

  for [`rbind()`](https://rdrr.io/r/base/cbind.html) S3 method for class
  `"rostido"`, data frames of class `"rostido"` to be combined.

  for [`print()`](https://rdrr.io/r/base/print.html) S3 method for class
  `"rostido"`, further arguments passed to or from other methods.

- .arrange_by:

  a list of expressions containing names of column(s) for sorting rows
  of the combined `"rostido"` data frame e.g.,
  `exprs(Account, Code, desc(Amount))`. Use
  [`desc()`](https://dplyr.tidyverse.org/reference/desc.html) to sort a
  variable in descending order; default `NULL`.

- x:

  an object used to select a method.

- .include:

  \<[`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)\>
  names of variables to be included or excluded when printing a
  `"rostido"` data frame containing Rostido Bank transaction data;
  default `NULL`.

- maxwidth:

  an `integer`, maximum width for printing `Description` field; default
  `65L`.

## Value

- `as_rostido()`:

  An object of class `"rostido"` inheriting from `"data.frame"`
  containing reformatted Rostido Bank transaction data.

## Details

`as_rostido()` reformats a data frame containing Rostido Bank
transaction data obtained using
[`read_rostido_csv()`](https://mark-eis.github.io/BitsnBobs/reference/Import_Rostido.md),
replacing `character` strings in the `Date` field with `"Date"` objects,
and those in the `Amount` and `Balance` fields with `numeric` values.

By default, if no `.arrange_by ` argument is specified, the
[`rbind()`](https://rdrr.io/r/base/cbind.html) S3 method for class
`"rostido"` sorts the results by `Date`, `AccountNo` and `Code`.

By default, if no `.include` argument is specified, the
[`print()`](https://rdrr.io/r/base/print.html) S3 method for class
`"rostido"` excludes the `SortCode` and `ChequeNo` columns from the
printed output.

## See also

[`as.Date()`](https://rdrr.io/r/base/as.Date.html),
[`print()`](https://rdrr.io/r/base/print.html),
[`rbind()`](https://rdrr.io/r/base/cbind.html).

Other Rostido:
[`Import_Rostido`](https://mark-eis.github.io/BitsnBobs/reference/Import_Rostido.md)

## Examples

``` r
if (FALSE) { # \dontrun{
   dnldpath <- "~/Rostido Bank/Downloads"

   ## __________________________
   ## Current account 55545372

   (curacc <- file.path(dnldpath, 55545372) |>
       most_recent_fname() |>
       read_rostido_csv() |>
       as_rostido())

   ## __________________________
   ## Savings account 55596784

   (savacc <- file.path(dnldpath, 55596784) |>
       most_recent_fname() |>
       read_rostido_csv() |>
       as_rostido())

   savacc |> print(.include = c(Description, Code, Amount, Balance))

   ## ______________
   ## All accounts

   rbind(curacc, savacc) ## default sort is by Date, AccountNo and Code.
   rbind(curacc, savacc, .arrange_by = exprs(AccountNo, Date, Code))
   rbind(curacc, savacc, .arrange_by = exprs(desc(Amount)))
   rbind(curacc, savacc, .arrange_by = exprs(Code, desc(Amount)))

   rm(curacc, savacc)
} # }
```
