# BitsnBobs R Package
# Mark Eisler Apr 2024
# For general bits and bobs of code
#
# Requires R version 4.2.0 (2022-04-22) -- "Vigorous Calisthenics" or later
#
# triodos.R


# ========================================
#' Manage Triodos Bank Account
#'
#' @name triodos
#' @description
#'
#' `file_name()` returns a character string representing the name of a transactions file downloaded from the Triodos
#' bank website in CSV format.
#'
#' `most_recent_fdate()` returns the most recent date as incorporated within such a filename string.
#'
#' `read_triodos()` reads a CSV file as formatted by Triodos Bank and returns the contents as a data frame.
#'
#' `as_triodos()` reformats a data frame containing Triodos Bank data.
#'
#' @details
#' These four functions facilitate reading and formatting CSV transaction files downloaded from the Triodos bank
#' website.
#'
#' `file_name()` returns a character string representing the name of a transactions file downloaded from the Triodos
#' bank website in CSV format i.e., concatenating the strings `"Download"`, a date of the form `"yyyymmdd"` and the
#' extension `".csv"` e.g., `"Download20240401.csv"`
#'
#' `most_recent_fdate()` searches the current folder for a filename as returned by `file_name()`, typically
#' incorporating the current date obtained using the default `.date` argument [`Sys.Date()`][base::Sys.Date]. If no
#' such file exists, the previous dates are used successively until a corresponding file is found, the search being
#' discontinued on reaching the date specified in the `earliest` argument.
#'
#' `read_triodos()` reads a file downloaded from the Triodos bank website in CSV format and returns the contents as a
#' data frame.
#'
#' `as_triodos()` reformats a data frame containing Triodos Bank data obtained using `read_triodos()`, replacing
#' `character` strings in the `Date` field with `"Date"` objects, and in the `Amount` and `Balance` fields with
#' `numeric` values. It also truncates `character` strings in the `Description` field to the length specified in the
#' `maxwidth` argument and saves the full version of any `Description` exceeding `maxwidth` as `attributes`
#' `"descr_no"` and `"descr"`. 
#'
#' @param .date `Date` object, date to be incorporated into a filename string.
#'
#' @param trydate `Date` object, most recent date from which to search for file; default `Sys.Date()`.
#'
#' @param earliest `Date` object, date before which search is discontinued; default `as.Date("2024-02-01")`.
#'
#' @param fun `function`, used to incorporate `.date' into a filename search string; default `file_name`.
#'
#' @param filename `character` string, name of the CSV file to be read.
#'
#' @param data data frame, as returned by `read_triodos()`.
#'
#' @param dateformat `character string`, passed as the `format` argument to [`as.Date()`][base::as.Date]; default
#'   `"%d/%m/%Y"`.
#'
#' @param maxwidth an `integer`, maximum width for printing `Description` field; default `50L`.
#'
#' @return
#'
#' \item{`file_name()`}{Filename string incorporating a date of the form `"Downloadyyyymmdd.csv"`.}
#'
#' \item{`most_recent_fdate()`}{`"Date"` object containing the most recent date as incorporated within the specified
#'   filename string.}
#'
#' \item{`read_triodos()`}{CSV file data formatted by Triodos Bank, as a dataframe.}
#'
#' \item{`as_triodos()`}{reformatted Triodos Bank data, as a dataframe.}
#'
#' @keywords utilities
#'
#' @export
#' @examples
#' \dontrun{
#'
#'     filepath <- "/Users/Charlie Chaplin/Triodos Bank/Downloads"
#'     (oldwd <- setwd(filepath))
#'     getwd()
#'     ## __________________________
#'     ## Current account 22245372
#'     setwd(paste0(filepath, "/22245372"))
#'     getwd()
#'
#'     most_recent_fdate() |>
#'         file_name() |>
#'         read_triodos() |>
#'         as_triodos() |>
#'         _[, -2]
#'
#'     setwd(oldwd)
#'
#' }

file_name <- function(.date)
    paste0("Download", gsub("-", "", as.character(.date)),".csv")

# ========================================
#  Return date of most recent file
#'
#' @rdname triodos
#' @export

most_recent_fdate <- function(trydate = Sys.Date(), earliest = as.Date("2024-02-01"), fun = file_name) {
    while(!file.exists(fun(trydate))) {
        trydate <- trydate - 1
        if (trydate < earliest)
            stop(paste("No file available after", trydate))
    }
    if (trydate < Sys.Date())
        cat("Most recent file available is for", as.character(trydate), "\n")
    trydate
}

# ========================================
#  Read CSV file formatted by Triodos
#'
#' @rdname triodos
#' @export

read_triodos <- function(filename) {
    cat("Reading file: <", filename, ">\n")
    read.csv(
        filename,
        header = FALSE,
        col.names = c("Date", "SortCode", "AccountNo", "Amount", "Code", "Description", "ChequeNo", "Balance"),
        colClasses = c("character", rep("factor", 2), "character", "factor", rep("character", 2))
    )
}

# ========================================
#  Reformat data frame with Triodos data
#'
#' @rdname triodos
#' @export

# fmt_triodos <- function(data, dateformat = "%d/%m/%Y", maxwidth = 50L) { # Four digit years
    # longs <- (data$Description |> nchar() > maxwidth) |> which()
    # attr(data, "descr_no") <- longs
    # attr(data, "descr") <- data$Description[longs]

    # data |> mutate(
        # across("Date", \(x) as.Date(x, format = dateformat)),
        # across(c("Amount", "Balance"), \(x) as.numeric(gsub(",", "", x))),
        # across("Description", \(x) strtrim(x, maxwidth))
    # )
# }

as_triodos <- function(data, dateformat = "%d/%m/%Y", maxwidth = 50L) { # Four digit years
    longs <- (data$Description |> nchar() > maxwidth) |> which()
    attr(data, "descr_no") <- longs
    attr(data, "descr") <- data$Description[longs]

    data |> mutate(
        across("Date", \(x) as.Date(x, format = dateformat)),
        across(c("Amount", "Balance"), \(x) as.numeric(gsub(",", "", x))),
        across("Description", \(x) strtrim(x, maxwidth))
    ) |>
    structure(class = c("triodos", class(data)))
}
