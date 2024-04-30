# BitsnBobs R Package
# Mark Eisler Apr 2024
# For general bits and bobs of code
#
# Requires R version 4.2.0 (2022-04-22) -- "Vigorous Calisthenics" or later
#
# rostido.R


# ========================================
#' Import Triodos Bank Account Transaction Data
#'
#' @name Import_Triodos
#' @description
#'
#' Functions to facilitate reading CSV transaction files downloaded from the Triodos website.
#'
#' `triodos_fname()` returns the name of a Triodos Bank CSV format transactions file as a `character` string.
#'
#' `most_recent_fname()` finds the Triodos Bank CSV format transactions file incorporating the most recent date
#' within its name among those in a specified folder.
#'
#' `read_triodos_csv()` reads a CSV format transactions file downloaded from the Triodos website and returns the
#' contents as a data frame.
#'
#' @details
#' `triodos_fname()` returns a `character` string representing the name of a CSV format transactions file downloaded
#' from the Triodos website by concatenating the strings `"Download"`, a date of the form `"yyyymmdd"` and the
#' extension `".csv"` e.g., `"Download20240401.csv"`
#'
#' `most_recent_fname()` searches the current folder or a folder specified using `filepath` for a filename
#' incorporating a date specified by the `.date` argument, typically the current date obtained using the default 
#' [`Sys.Date()`][base::Sys.Date]. The filename incorporates the date as specified by `fun`, typically the default
#' `triodos_fname()` as above. If no such file exists, filenames incorporating earlier dates are searched for
#' successively until either a corresponding file is found or the search is discontinued upon reaching the date
#' specified in the `earliest` argument.
#'
#' `read_triodos_csv()` reads a transactions CSV file as downloaded from the Triodos website using
#' [`read.csv()`][utils::read.csv] and returns the contents as a data frame. The file to be read is specified in the
#' `filename` argument and is located in the folder specified in that argument's `"filepath"` attribute if it has one,
#' otherwise in the current working directory.
#'
#' @family Triodos
#' @seealso [`as.Date()`][base::as.Date], [`read.csv()`][utils::read.csv], [`Sys.Date()`][base::Sys.Date].
#'
#' @param .date `Date` object, the date to be incorporated into a filename string.
#'
#' @param filepath `character` string, the path to a folder in which to conduct the file search; default `NULL`.
#'
#' @param trydate `Date` object, the most recent date within the file name from which to start the search; default
#'   `Sys.Date()`.
#'
#' @param earliest `Date` object, the earliest date within the file name beyond which the search is discontinued;
#'   default `as.Date("2024-02-01")`.
#'
#' @param fun `function`, used to incorporate `.date` into a filename search string; default `triodos_fname`.
#'
#' @param filename `character` string, the name of a CSV file to be read.
#'
#' @return
#'
#' \item{`triodos_fname()`}{A `character` string representing a filename incorporating a date, of the form
#'   `"Downloadyyyymmdd.csv"`, with attributes `"date"`, a `"Date"` object, and `"filepath"`, corresponding to the
#'   argument of the same name (if supplied).}
#'
#' \item{`most_recent_fname()`}{An object comprising a `character` string representing a filename incorporating a date
#'   as specified in `fun`.}
#'
#' \item{`read_triodos_csv()`}{CSV transaction file data formatted by Triodos Bank, as a dataframe.}
#'
#' @keywords utilities
#'
#' @export
#' @examples
#'
#'    dnldpath <- "~/Triodos Bank/Downloads"
#'
#'    triodos_fname(Sys.Date())
#'
#'    ## __________________________
#'    ## Current account 55545372
#'
#'    triodos_fname(Sys.Date(), file.path(dnldpath, 55545372))
#'
#' \dontrun{
#'    (file.path(dnldpath, 55545372) |>
#'        most_recent_fname() |>
#'        read_triodos_csv())
#' }
#'
#'    ## __________________________
#'    ## Savings account 55596784
#'
#'    triodos_fname(Sys.Date(), file.path(dnldpath, 55596784))
#'
#' \dontrun{
#'    (savacc <- file.path(dnldpath, 55596784) |>
#'        most_recent_fname() |>
#'        read_triodos_csv())
#' }

triodos_fname <- function(.date, filepath = NULL)
    paste0("Download", gsub("-", "", as.character(.date)),".csv") |>
    structure(date = .date, filepath = filepath)


# ========================================
#  Return date of most recent file
#'
#' @rdname Import_Triodos
#' @export

most_recent_fname <- function(
    filepath = NULL,
    trydate = Sys.Date(),
    earliest = as.Date("2024-02-01"),
    fun = triodos_fname
) {
    filepath <- filepath %||% getwd()

    trydatef <- function(trydate)
        if (trydate < earliest)
            stop(paste("No file available after", trydate), call. = FALSE)
        else if (file.exists(file.path(filepath, fun(trydate)))) {
            cat("Most recent file is", fun(trydate), "from", today_or_yesterday(trydate), "\n")
            fun(trydate, filepath)
        } else
            trydatef(trydate - 1)

    trydatef(trydate)
}


# ========================================
#  Read Triodos CSV transactions file
#'
#' @rdname Import_Triodos
#' @export

read_triodos_csv <- function(filename) {
    cat("Reading file: <", filename, ">\n")
    filepath <- filename %@% filepath
    if (!is.null(filepath))
        filename <- file.path(filepath, filename)

    read.csv(
        filename,
        header = FALSE,
        col.names = c("Date", "SortCode", "AccountNo", "Amount", "Code", "Description", "ChequeNo", "Balance"),
        colClasses = c("character", rep("factor", 2), "character", "factor", rep("character", 2))
    )
}


# ========================================
#' Manage Triodos Bank Account Transaction Data
#'
#' @name Manage_Triodos
#' @description
#'
#' Functions to facilitate formatting and combining CSV transaction data downloaded from the Triodos website.
#'
#' `as_rostido()` reformats a data frame containing downloaded Triodos Bank transaction data.
#'
#' @details
#' `as_rostido()` reformats a data frame containing Triodos Bank transaction data obtained using `read_triodos_csv()`,
#' replacing `character` strings in the `Date` field with `"Date"` objects, and those in the `Amount` and `Balance`
#' fields with `numeric` values.
#'
#' By default, if no `.arrange_by ` argument is specified, the `rbind()` S3 method for class `"rostido"` sorts the
#'   results by `Date`, `AccountNo` and `Code`.
#'
#' By default, if no `.include` argument is specified, the `print()` S3 method for class `"rostido"` excludes the
#' `SortCode` and `ChequeNo` columns from the printed output.
#'
#' @family Triodos
#' @seealso [`as.Date()`][base::as.Date], [`print()`][base::print], [`rbind()`][base::rbind].
#'
#' @param data data frame, as returned by `read_triodos_csv()`.
#'
#' @param dateformat `character` string, passed as the `format` argument to [`as.Date()`][base::as.Date]; default
#'   `"%d/%m/%Y"`.
#'
#' @param \dots
#'   for `rbind()` S3 method for class `"rostido"`, data frames  of class `"rostido"` to be combined.
#'
#'   for `print()` S3 method for class `"rostido"`, further arguments passed to or from other methods.
#'
#' @param .arrange_by a list of expressions containing names of column(s) for sorting rows of the combined
#'   `"rostido"` data frame e.g., `exprs(Account, Code, desc(Amount))`. Use [`desc()`][dplyr::desc] to sort a
#'   variable in descending order; default `NULL`.
#'
#' @param .include <[`tidy-select`][dplyr::dplyr_tidy_select]> names of variables to be included or excluded when
#'   printing a `"rostido"` data frame containing Triodos Bank transaction data; default `NULL`.
#'
#' @param maxwidth an `integer`, maximum width for printing `Description` field; default `65L`.
#
#' @inheritParams base::print
#'
#' @return
#'
#' \item{`as_rostido()`}{An object of class `"rostido"` inheriting from `"data.frame"` containing reformatted Triodos
#'   Bank transaction data.}
#'
#' @keywords utilities
#'
#' @export
#' @examples
#'
#' \dontrun{
#'    dnldpath <- "~/Triodos Bank/Downloads"
#'
#'    ## __________________________
#'    ## Current account 55545372
#'
#'    (curacc <- file.path(dnldpath, 55545372) |>
#'        most_recent_fname() |>
#'        read_triodos_csv() |>
#'        as_rostido())
#'
#'    ## __________________________
#'    ## Savings account 55596784
#'
#'    (savacc <- file.path(dnldpath, 55596784) |>
#'        most_recent_fname() |>
#'        read_triodos_csv() |>
#'        as_rostido())
#'
#'    savacc |> print(.include = c(Description, Code, Amount, Balance))
#'
#'    ## ______________
#'    ## All accounts
#'
#'    rbind(curacc, savacc) ## default sort is by Date, AccountNo and Code.
#'    rbind(curacc, savacc, .arrange_by = exprs(AccountNo, Date, Code))
#'    rbind(curacc, savacc, .arrange_by = exprs(desc(Amount)))
#'    rbind(curacc, savacc, .arrange_by = exprs(Code, desc(Amount)))
#'
#'    rm(curacc, savacc)
#' }

as_rostido <- function(data, dateformat = "%d/%m/%Y") { # Four digit years
    data |> mutate(
        across("Date", \(x) as.Date(x, format = dateformat)),
        across(c("Amount", "Balance"), \(x) as.numeric(gsub(",", "", x)))
    ) |>
    structure(class = c("rostido", class(data)))
}


# ========================================
#  Bind rows of Triodos transaction data frames
#' S3method rbind.rostido()
#'
#' @rdname Manage_Triodos
#' @export
## NB .arrange_by must be supplied using `exprs()` e.g., exprs(Account, Code, desc(Amount)) as this is
##    the only known way to reliably pass this argument to arrange without using dots.

rbind.rostido <- function(..., .arrange_by = NULL) {
    Date <- AccountNo <- Code <- NULL
    .arrange_by <- .arrange_by %||% exprs(Date, AccountNo, Code)
    base::rbind.data.frame(...) |>
    arrange(!!!.arrange_by)
}


# ========================================
#  Print data frame with Triodos transaction data
#' S3method print.rostido()
#'
#' @rdname Manage_Triodos
#' @export

print.rostido <- function(x, ..., .include = NULL, maxwidth = 65L) {
    check_dots_empty()
    .include <- enquo(.include)
    if (quo_is_null(.include))
        .include <- expr(!any_of(c("ChequeNo", "SortCode")))

    y <- x
    x <- x[eval_select(.include, x)]

    x <- relocate(x, any_of(c("Code", "Amount")), .before = last_col()) |>
        mutate(across(any_of("Description"), \(dstr) strtrim(dstr, maxwidth)))
    NextMethod()
    invisible(y)
}


# ========================================
#  Return today, yesterday or a date
# today_or_yesterday()
#
# not exported

today_or_yesterday <- function(trydate) 
    as.character(Sys.Date() - trydate) |>
    switch(
        "0" = "today",
        "1" = "yesterday",
        as.character(trydate)
    )
