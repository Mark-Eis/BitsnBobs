# Import Rostido Bank Account Transaction Data

Functions to facilitate reading CSV transaction files downloaded from
the Rostido website.

`rostido_fname()` returns the name of a Rostido Bank CSV format
transactions file as a `character` string.

`most_recent_fname()` finds the Rostido Bank CSV format transactions
file incorporating the most recent date within its name among those in a
specified folder.

`read_rostido_csv()` reads a CSV format transactions file downloaded
from the Rostido website and returns the contents as a data frame.

## Usage

``` r
rostido_fname(.date, filepath = NULL)

most_recent_fname(
  filepath = NULL,
  trydate = Sys.Date(),
  earliest = as.Date("2024-02-01"),
  fun = rostido_fname
)

read_rostido_csv(filename)
```

## Arguments

- .date:

  `Date` object, the date to be incorporated into a filename string.

- filepath:

  `character` string, the path to a folder in which to conduct the file
  search; default `NULL`.

- trydate:

  `Date` object, the most recent date within the file name from which to
  start the search; default
  [`Sys.Date()`](https://rdrr.io/r/base/Sys.time.html).

- earliest:

  `Date` object, the earliest date within the file name beyond which the
  search is discontinued; default `as.Date("2024-02-01")`.

- fun:

  `function`, used to incorporate `.date` into a filename search string;
  default `rostido_fname`.

- filename:

  `character` string, the name of a CSV file to be read.

## Value

- `rostido_fname()`:

  A `character` string representing a filename incorporating a date, of
  the form `"Downloadyyyymmdd.csv"`, with attributes `"date"`, a
  `"Date"` object, and `"filepath"`, corresponding to the argument of
  the same name (if supplied).

- `most_recent_fname()`:

  An object comprising a `character` string representing a filename
  incorporating a date as specified in `fun`.

- `read_rostido_csv()`:

  CSV transaction file data formatted by Rostido Bank, as a dataframe.

## Details

`rostido_fname()` returns a `character` string representing the name of
a CSV format transactions file downloaded from the Rostido website by
concatenating the strings `"Download"`, a date of the form `"yyyymmdd"`
and the extension `".csv"` e.g., `"Download20240401.csv"`

`most_recent_fname()` searches the current folder or a folder specified
using `filepath` for a filename incorporating a date specified by the
`.date` argument, typically the current date obtained using the default
[`Sys.Date()`](https://rdrr.io/r/base/Sys.time.html). The filename
incorporates the date as specified by `fun`, typically the default
`rostido_fname()` as above. If no such file exists, filenames
incorporating earlier dates are searched for successively until either a
corresponding file is found or the search is discontinued upon reaching
the date specified in the `earliest` argument.

`read_rostido_csv()` reads a transactions CSV file as downloaded from
the Rostido website using
[`read.csv()`](https://rdrr.io/r/utils/read.table.html) and returns the
contents as a data frame. The file to be read is specified in the
`filename` argument and is located in the folder specified in that
argument's `"filepath"` attribute if it has one, otherwise in the
current working directory.

## See also

[`as.Date()`](https://rdrr.io/r/base/as.Date.html),
[`read.csv()`](https://rdrr.io/r/utils/read.table.html),
[`Sys.Date()`](https://rdrr.io/r/base/Sys.time.html).

Other Rostido:
[`Manage_Rostido`](https://mark-eis.github.io/BitsnBobs/reference/Manage_Rostido.md)

## Examples

``` r

   dnldpath <- "~/Rostido Bank/Downloads"

   rostido_fname(Sys.Date())
#> [1] "Download20260507.csv"
#> attr(,"date")
#> [1] "2026-05-07"

   ## __________________________
   ## Current account 55545372

   rostido_fname(Sys.Date(), file.path(dnldpath, 55545372))
#> [1] "Download20260507.csv"
#> attr(,"date")
#> [1] "2026-05-07"
#> attr(,"filepath")
#> [1] "~/Rostido Bank/Downloads/55545372"

if (FALSE) { # \dontrun{
   (file.path(dnldpath, 55545372) |>
       most_recent_fname() |>
       read_rostido_csv())
} # }

   ## __________________________
   ## Savings account 55596784

   rostido_fname(Sys.Date(), file.path(dnldpath, 55596784))
#> [1] "Download20260507.csv"
#> attr(,"date")
#> [1] "2026-05-07"
#> attr(,"filepath")
#> [1] "~/Rostido Bank/Downloads/55596784"

if (FALSE) { # \dontrun{
   (savacc <- file.path(dnldpath, 55596784) |>
       most_recent_fname() |>
       read_rostido_csv())
} # }
```
