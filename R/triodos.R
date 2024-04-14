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
#' @description
#'
#' `file_name()` returns a filename string incorporating a date of the form `"Downloadyyyymmdd.csv"` e.g.,
#' `"Download20240401.csv"`
#'
#' `most_recent_fdate()` returns the most recent date incorporated within a specified filename string.
#'
#' `read_triodos()` reads a CSV file as formatted by Triodos Bank and returns as a data frame.
#'
#' `fmt_triodos()` reformats a data frame containing Triodos Bank data.
#'
#'
#' @details
#' `decdeg()` works with a numeric (`double`) vector representing one or more coordinates of latitude or longitude in
#' decimal degrees.
#'
#' `as_decdeg()` is an S3 function that works with individual coordinates supplied as `numeric values`, with
#' [`"degminsec"`][BitsnBobs::degminsec] objects, or with lists of such coordinates. It also works with latitude and
#' longitude coordinates paired in a [`"latlon"`][BitsnBobs::latlon] object (see examples), or with lists of
#' `"latlon"` paired coordinates. The S3 method for an existing `"decdeg"` object simply validates and returns its
#' argument.
#'
#' @param .date `Date` object, date to be incorporated into a filename string.
#'
#' @param trydate `Date` object, most recent date from which to search for file; default `Sys.Date()`.
#'
#' @param earliest `Date` object, date before which search is discontinued; default `as.Date("2024-02-01")`.
#'
#' @param fun `function`, used to incorporate `.date' into a filename search string; default `file_name`.
#'
#' @param filename a `character` string, name of the CSV file to be read.
#'
#' @param data a data frame, as returned by `read_triodos()`.
#'
#' @param dateformat a character string passed as the `format` argument to [`as.Date()`][base::as.Date]; default
#'   `"%d/%m/%Y"`.
#'
#' @param maxwidth an `integer`, maximum width for printing `Description` field; default `50L`.
#'
#' @return
#'
#' \item{`file_name()`}{a filename string incorporating a date of the form `"Downloadyyyymmdd.csv"`} 
#'
#' \item{`most_recent_fdate()`}{a `Date` object, the most recent date incorporated within the specified filename string.} 
#'
#' \item{`read_triodos()`}{a data frame.} 
#'
#' \item{`fmt_triodos()`}{a data frame.} 
#'
#' @keywords utilities
#'
#' @export
#' @examples
#' \dontrun{
#'
#'     filepath <- "/Users/frzmce/Library/CloudStorage/OneDrive-UniversityofBristol/Documents/MCE Admin/MCE Admin Safe/Triodos Bank/Downloads"
#"     (oldwd <- setwd(filepath))
#"     getwd() 
#'     ## __________________________ 
#'     ## Current account 22245372 
#'     setwd(paste0(filepath, "/22245372")) 
#'     getwd() 
#'      
#'     most_recent_fdate() |>
#'         file_name() |>
#'         read_triodos() |>
#'         fmt_triodos()
#'
#'     setwd(oldwd)
#'
#' } ## end \dontrun

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

fmt_triodos <- function(data, dateformat = "%d/%m/%Y", maxwidth = 50L) { # Four digit years
	longs <- (data$Description |> nchar() > maxwidth) |> which()
	attr(data, "descr_no") <- longs
	attr(data, "descr") <- data$Description[longs]

	data |> mutate(
		across(Date, \(x) as.Date(x, format = dateformat)),
		across(c(Amount, Balance), \(x) as.numeric(gsub(",", "", x))),
		across(Description, \(x) strtrim(x, maxwidth))
	)
}
