# BitsnBobs R Package
# Mark Eisler - Feb 2024
# For general bits and bobs of code
#
# Requires R version 4.2.0 (2022-04-22) -- "Vigorous Calisthenics" or later
#
# logical-cols.R

# ========================================
#' Add Logical Columns to Data Frame Flagging Presence of Keywords
#'
#' @description
#' Function to add logical columns indicating whether or not specified keywords are present in a character column of
#' a data frame.
#'
#' @details
#' The character column of \code{data} identified by \code{.look_in} is searched for keywords provided in \code{value}
#' using [`str_detect`][stringr::str_detect]. The keyword search is case insensitive. If no character column identified
#' by \code{look_in} is present in \code{data}, an error message will be given.
#'
#' The logical columns resulting from use of \code{kwd_cols()} and the infix form \code{kwd_cols()<-} may be analysed
#' and the number of \code{TRUE} values counted using the functions [`count_lgl`][count_lgl] and [`sum_lgl`][sum_lgl].
#'
#' The function \code{lgl_cols()} returns the names of all logical columns in \code{data}. 
#'
#' @seealso [`detective()`][detective], [`str_detect`][stringr::str_detect].
#' @family logical-cols
#'
#' @param data a data frame, or a data frame extension (e.g. a tibble).
#'
#' @param .look_in <[`data-masking`][rlang::args_data_masking]> quoted name of the character column in which to look for
#'   keywords; default \code{Response}.
#'
#' @param value a \code{character vector} containing the keywords to be identified.
#'
#' @return \code{kwd_cols()} returns a modified data frame with logical columns indicating the presence of keywords in
#'   \code{.look_in}. The infix form of the function \code{kwd_cols<-()} modifies and returns the original data frame.
#'   \code{lgl_cols()} returns a character vector of the names of all logical columns in \code{data}.
#'
#' @export
#' @examples
#' (car_names <- data.frame(Response = rownames(mtcars)))
#'
#' kwd <- c("Mazda", "Merc", "Toyota", "Volkswagen", "X", "450")
#' car_names |> kwd_cols(value = kwd)
#' car_names    ## Original data frame unchanged
#'
#' kwd_cols(car_names)<- kwd
#' car_names    ## Original data frame modified by infix version
#'
#' lgl_cols(car_names)
#' identical(kwd, lgl_cols(car_names))
#' rm(car_names, kwd)

kwd_cols <- function(data, .look_in = Response, value) {
	Response <- NULL
    .look_in <- enquo(.look_in) 
    stopifnot(is.data.frame(data), is.character(eval_tidy(.look_in, data)))
    fns <- setNames(tolower(value), str_to_title(value)) |>
        map(\(val) \(look_in) str_detect(tolower(look_in), val))
    data |> mutate(across(!!.look_in, fns, .names = "{.fn}"))
}

# ========================================
#  Infix function version
#' @rdname kwd_cols
#' @export

`kwd_cols<-` <- function(data, .look_in = Response, value) {
	Response <- NULL
    kwd_cols(data, {{.look_in}}, value)
}

# ========================================
#  Get column names of logical columns in a data frame.
#' @rdname kwd_cols
#' @export

lgl_cols<- function(data) {
    stopifnot(is.data.frame(data))
    eval_select(expr(where(is.logical)), data) |> names()
}


# ========================================
#' Count Combinations and Totals for Logical Columns in Data Frame
#'
#' @description
#' Functions to count the number of ocurrences of unique combinations of values across all logical columns in a data
#' frame and the total number of \code{TRUE} values for each logical column.
#'
#' @details
#' Function \code{count_lgl()} counts the number of ocurrences of unique combinations of values across all logical
#' columns in a data frame using [`summarise`][dplyr::summarise] in package \pkg{\link[dplyr]{dplyr}}.
#'
#' Function \code{sum_lgl()} counts the total number of \code{TRUE} values for each logical column in a data frame using
#' matrix multiplication.
#'
#' These functions may be useful for counting combinations or tallying totals of keywords flagged as \code{TRUE} in
#' logical columns added to a data frame using [`kwd_cols`][kwd_cols].
#'
#' @seealso [`summarise`][dplyr::summarise].
#' @family logical-cols
#'
#' @param df a data frame, or a data frame extension (e.g. a tibble).
#'
#' @param .newcol quoted name to be assigned to the new count column; default \code{n}.
#'
#' @param .arrange_by <[`data-masking`][rlang::args_data_masking]> quoted name of a column for ordering results; default
#'   \code{NULL}.
#'
#' @param wt frequency weights. Can be \code{NULL} or the name of a numeric variable column; default \code{NULL}.
#'   * If \code{NULL} (the default), counts the number of rows with \code{TRUE} values for each logical column.
#'   * If a variable, computes sum(wt) for rows with \code{TRUE} values for each logical column.
#'
#' @return \code{count_lgl()} returns a data frame comprising unique combinations of the logical columns of \code{df}
#' and an additional column of the counts of ocurrences of each of these combinations. \code{sum_lgl()} returns a named
#' numeric vector of totals for each logical column.
#'
#' @export
#' @examples
#' ## Following on from kwd_cols() examples… 
#' car_names <- data.frame(Response = rownames(mtcars)) |>
#'   kwd_cols(, c("Mazda", "Merc", "Toyota", "Volkswagen", "X", "450"))
#' car_names
#'
#' (carname_counts <- count_lgl(car_names, .arrange_by = desc(n)))
#' sum_lgl(car_names)
#' sum_lgl(carname_counts, wt = "n")
#'
#' car_names |> count_lgl(.newcol = subtotals) |> print_lf() |> sum_lgl(wt = "subtotals")
#'
#' rm(car_names, carname_counts)

count_lgl <- function(df, .newcol = n, .arrange_by = NULL) {
    n <- NULL
    stopifnot(is.data.frame(df))
    df |>
        summarise({{.newcol}} := dplyr::n(), .by = where(is.logical)) |>
        arrange({{.arrange_by}} %||% across(where(is.logical)))
}

# ========================================
#  Count the total number of TRUE values for each logical column in a data frame
#' @rdname count_lgl
#' @export

sum_lgl <- function(df, wt = NULL) {
    stopifnot(is.data.frame(df), is.null(wt) || is.numeric(df[[wt]]))
    df |>
        mutate(
            across(where(is.logical), as.integer),
            .keep = "used"
        ) |>
        as.matrix() |>
        crossprod(
            if(is.null(wt))
                rep(1, nrow(df))
            else
                df[[wt]]
        ) |>
        (\(y) y[,1])()
}

# ========================================
#' @title
#' Split Data Frame into a List Based on Values in Logical Columns
#'
#' @description
#' Split a data frame into a named \code{list} of \code{tibble} data frames on the basis that each new data frame forming
#' an element of the \code{list} comprises rows of the original data frame that contained \code{TRUE} values in
#' a particular \code{logical} column.
#'
#' @details
#' Columns of \code{data} to be included in \code{tibble}s comprising the list returned may be selected using the
#' \code{\dots} argument with the <[`tidy-select`][dplyr::dplyr_tidy_select]> syntax of package \pkg{\link[dplyr]{dplyr}},
#' including use of \strong{selection helpers}. If this argument is omitted, all non-logical columns will be selected
#' using the tidy selection predicate function \code{\link[tidyselect]{where}(\(x) !\link[base]{is.logical}(x))}.
#'
#' The length of the \code{list} returned is equal to the number of \code{logical} columns in the original data frame.
#' Each list element is named the same as the corresponding logical column in \code{data} from which rows with
#' \code{TRUE} values were selected for inclusion in its \code{tibble}.
#'
#' \code{list_lgl()} may be used to tease out information in a character column of a data frame containing selected
#' keywords previously flagged in logical columns using [`kwd_cols`][kwd_cols], see examples. Finally, the named list
#' may be converted back to a single data frame using [`bind_rows`][dplyr::bind_rows], see examples. 
#'
#' @seealso [`bind_rows`][dplyr::bind_rows],  [`select`][dplyr::select], [`split`][base::split],
#'   <[`tidy-select`][dplyr::dplyr_tidy_select]>, [`tibble`][tibble::tibble-package].
#' @family logical-cols
#'
#' @param .data a data frame, or a data frame extension (e.g. a [`tibble`][tibble::tibble-package]).
#'
#' @param \dots <[`tidy-select`][dplyr::dplyr_tidy_select]> The selection of columns in \code{data} to be included in
#'   the tibbles comprising elements of the returned list.
#'
#' @return A named list of [tibble][tibble::tibble-package] data frames containing selected rows and columns of the
#'   original data frame, having length equal to the number of logical columns in the original data frame and names the
#'   same as the corresponding logical columns.
#'
#' @export
#' @examples
#' ## Following on from kwd_cols() examples… 
#' car_names <- data.frame(Response = rownames(mtcars))
#' kwd <- c("Mazda", "Merc", "Toyota", "Volkswagen", "X", "450")
#' kwd_cols(car_names)<- kwd
#' car_names
#'
#' list_lgl(car_names)
#' list_lgl(car_names, Response, X)
#' list_lgl(car_names, last_col())
#' list_lgl(car_names, contains("o"))
#'
#' ## Convert back to single data frame
#' car_names |>
#'   list_lgl() |>
#'   bind_rows(.id = "Group")
#'
#' rm(kwd, car_names)

list_lgl <- function(.data, ...) {
    stopifnot(is.data.frame(.data))
    pos <- eval_select(expr(c(...)), .data)
    pos_lgl <- eval_select(expr(where(is.logical)), .data)
    if (!length(pos))
        pos <- seq_along(.data)[-pos_lgl]
    .data[pos_lgl] |> map(\(x) .data[x, ] |> _[pos])
}
