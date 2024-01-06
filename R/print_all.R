# BitsnBobs R Package
# Mark Eisler Jan 2024
# For general bits and bobs of code
#
# Requires R version 4.2.0 (2022-04-22) -- "Vigorous Calisthenics" or later
#
# print_all.R

# ========================================
#' Print All (or More) of an Object
#'
#' @description
#' `print_all()` is a generic function for extended printing of an object, for instance printing all rows of a tibble,
#' a derived class or even a regular data frame, optionally following up by printing a specified number of linefeeds.
#' Being a generic function, new printing methods can be easily added for a new [`class`][base::class].
#'
#' @details
#' For a [`tibble`][tibble::tibble-package] \code{x}, \code{print_all(x)} is equivalent to
#' \code{print(x, n = nrow(x))}, followed up if required by \var{n} linefeeds generated as if by using
#' \code{cat(rep("\n", n))}.
#'
#' The \code{linefeeds} argument may be useful within a piped sequence to separate output from subsequent printing. If
#' a vector of \code{length > 1} is entered for \code{linefeeds}, only the first element will be used, and negative
#' integers will be converted to zero i.e., no line feeds.  
#'
#' @seealso [`print()`][base::print], [`print_lf()`][print_lf], [`method_info()`][method_info],
#'   [`methods()`][utils::methods], [`tibble`][tibble::tibble-package].
#' @family print
#'
#' @param x An object such as a [`tibble`][tibble::tibble-package] data frame.
#'
#' @param linefeeds A positive integer specifying the number of linefeeds to follow up the printed output; default
#'   \code{NULL}.
#'
#' @inheritParams base::print
#' @inheritParams tibble::print.tbl_df
#' @inheritParams method_info
#'
#' @return Invisibly returns its argument.
#'
#' @keywords print
#' @export
#' @examples
#' (tib <- tibble(x = 1:26, y = LETTERS[x], z = paste0(x, y)))
#' tib |> print_all()
#' tib |> print_all() |> names()
#' tib |> print_all(linefeeds = 3) |> names()
#'
#' df <- tib |> as.data.frame()
#' df |> print_all()                         ## Does nothing more than regular print()
#' df |> print_all(linefeeds = 2) |> names() ## Regular data frame printing, with line feeds
#'
#' rm(df, tib)

print_all <- function(x, ...) {
    UseMethod("print_all")
}

# ========================================
#  Print All Rows of a Data Frame
#  S3method print_all.data.frame()
#'
#' @rdname print_all
#' @export

print_all.data.frame <- function(x, ..., linefeeds = NULL, digits = NULL, quote = FALSE, right = TRUE, 
    row.names = TRUE, max = NULL) {

    print(x, digits = digits, quote = quote, right = right, row.names = row.names, max = max)
    linefeed(linefeeds)
    invisible(x)
}

# ========================================
#  Print All Rows of a Tibble
#  S3method print_all.tbl()
#'
#' @rdname print_all
#' @export

print_all.tbl <- function(x, linefeeds = NULL, width = NULL, ..., max_extra_cols = NULL, 
    max_footer_lines = NULL) {

    print(x, width, ..., n = nrow(x), max_extra_cols = max_extra_cols, max_footer_lines = max_footer_lines)
    linefeed(linefeeds)
    invisible(x)
}

# ========================================
#  Print All Rows of a Tibble
#  S3method print_all.tbl_df()
#'
#' @rdname print_all
#' @export

print_all.tbl_df <- function(x, linefeeds = NULL, width = NULL, ..., max_extra_cols = NULL, 
    max_footer_lines = NULL) {

   NextMethod()
}

# ========================================
#  Print All of an htest
#  S3method print_all.tbl_df()
#'
#' @rdname print_all
#' @export

print_all.htest <- function(x, ...) {
    NextMethod()
}

# ========================================
#  Print All of a MethodsFunction
#  S3method print_all.MethodsFunction()
#'
#' @rdname print_all
#' @export

print_all.MethodsFunction <- function(x, ..., .arrange_by = across(everything())) {
    x %@% info |>
        rownames_to_column(var = "Method") |>
        rename_with(str_to_title, !last_col()) |>
        arrange(.arrange_by) |>
        new_info_df(prt_str = "S3 Methods")
}

# ========================================
#  Print All Default Method
#  S3method print_all.default()
#' @export

print_all.default <- function(x, ...) {
	print(x, ...)
}

