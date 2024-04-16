# BitsnBobs R Package
# Mark Eisler Apr 2024
# For general bits and bobs of code
#
# Requires R version 4.2.0 (2022-04-22) -- "Vigorous Calisthenics" or later
#
# rostido.R


# ========================================
#' Manage Triodos Bank Account
#'
#' @name rostido
#' @description
#'
#' `file_name()` returns a character string representing the name of a CSV format transactions file downloaded from
#' the Triodos bank website.
#'
#' `most_recent_fdate()` returns the most recent date incorporated within such a filename in the current folder.
#'
#' `read_triodos_csv()` reads a Triodos Bank transactions CSV file and returns the contents as a
#'  data frame.
#'
#' `as_rostido()` reformats a data frame containing downloaded Triodos Bank transaction data.
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
#' `read_triodos_csv()` reads a transactions CSV file downloaded from the Triodos bank website and returns the
#' contents as a data frame.
#'
#' `as_rostido()` reformats a data frame containing Triodos Bank data obtained using `read_triodos_csv()`, replacing
#' `character` strings in the `Date` field with `"Date"` objects, and in the `Amount` and `Balance` fields with
#' `numeric` values. It also truncates `character` strings in the `Description` field to the length specified in the
#' `maxwidth` argument and saves the full version of any `Description` exceeding `maxwidth` as `attributes`
#' `"descr_no"` and `"descr"`. 
#'
#' @param .date `Date` object, the date to be incorporated into a filename string.
#'
#' @param trydate `Date` object, the most recent date from which to search for file; default `Sys.Date()`.
#'
#' @param earliest `Date` object, the date before which the search is discontinued; default `as.Date("2024-02-01")`.
#'
#' @param fun `function`, used to incorporate `.date' into a filename search string; default `file_name`.
#'
#' @param filename `character` string, the name of a CSV file to be read.
#'
#' @param data data frame, as returned by `read_triodos_csv()`.
#'
#' @param dateformat `character string`, passed as the `format` argument to [`as.Date()`][base::as.Date]; default
#'   `"%d/%m/%Y"`.
#'
#' @param maxwidth an `integer`, maximum width for printing `Description` field; default `50L`.
#
#' @inheritParams dplyr::arrange
#'
#' @return
#'
#' \item{`file_name()`}{Filename string incorporating a date of the form `"Downloadyyyymmdd.csv"`.}
#'
#' \item{`most_recent_fdate()`}{`"Date"` object containing the most recent date as incorporated within the specified
#'   filename string.}
#'
#' \item{`read_triodos_csv()`}{CSV file data formatted by Triodos Bank, as a dataframe.}
#'
#' \item{`as_rostido()`}{An object of class `"rostido"` inheriting from `"data.frame"` containign reformatted Triodos
#'   Bank data.}
#'
#' @keywords utilities
#'
#' @export
#' @examples
#' \dontrun{
#'
#'     filepath <- "~/Triodos Bank/Downloads"
#'     (oldwd <- setwd(filepath))
#'     getwd()
#'
#'     ## __________________________
#'     ## Current account 55545372
#'     setwd(paste0(filepath, "/55545372"))
#'     getwd()
#'
#'     (curracc <- most_recent_fdate() |>
#'         file_name() |>
#'         read_triodos_csv() |>
#'         as_rostido())
#'
#'     ## __________________________
#'     ## Savings account 55596784
#'
#'     setwd(paste0(filepath, "/55596784"))
#'     getwd()
#'
#'     (savacc <- most_recent_fdate() |>
#'          file_name() |>
#'          read_triodos_csv() |>
#'          as_rostido())
#'
#'     ## ______________
#'     ## All accounts
#'     rbind(curracc, savacc) |>
#'         arrange()
#'
#'     rm(curracc, savacc)
#'     setwd(oldwd)
#'
#' }

file_name <- function(.date)
    paste0("Download", gsub("-", "", as.character(.date)),".csv")

# ========================================
#  Return date of most recent file
#'
#' @rdname rostido
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
#  Read Triodos CSV transactions file
#'
#' @rdname rostido
#' @export

read_triodos_csv <- function(filename) {
    cat("Reading file: <", filename, ">\n")
    read.csv(
        filename,
        header = FALSE,
        col.names = c("Date", "SortCode", "AccountNo", "Amount", "Code", "Description", "ChequeNo", "Balance"),
        colClasses = c("character", rep("factor", 2), "character", "factor", rep("character", 2))
    )
}

# ========================================
#  Reformat Triodos transaction data frame
#'
#' @rdname rostido
#' @export


as_rostido <- function(data, dateformat = "%d/%m/%Y") { # Four digit years
    data |> mutate(
        across("Date", \(x) as.Date(x, format = dateformat)),
        across(c("Amount", "Balance"), \(x) as.numeric(gsub(",", "", x)))
    ) |>
    structure(class = c("rostido", class(data)))
}

# ========================================
#  Sort data frame with Triodos transaction data
#' S3method arrange.rostido()
#'
#' @rdname rostido
#' @export

arrange.rostido <- function(.data, ..., .by_group = FALSE) {
    if (...length())
        NextMethod("arrange")
    else
        arrange(.data, .data$Date, .data$AccountNo, .data$Code)
}

# ========================================
#  Print data frame with Triodos transaction data
#' S3method print.rostido()
#'
#' @rdname rostido
#' @export

print.rostido <- function(x, ..., exclude = c("ChequeNo", "SortCode"), maxwidth = 50L) {
    y <- x
    x <- x |>
    select(!any_of(exclude)) |>
    relocate(Amount, .before = Balance) |>
    mutate(across("Description", \(x) strtrim(x, maxwidth)))
    NextMethod()
    invisible(y)
}