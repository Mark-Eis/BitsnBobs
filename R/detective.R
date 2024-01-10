# BitsnBobs R Package
# Mark Eisler - Jan 2024
# For general bits and bobs of code
#
# Requires R version 4.2.0 (2022-04-22) -- "Vigorous Calisthenics" or later
#
# detective.R


# ========================================
#' Search for Pattern in a Data Frame Character Column
#'
#' @description
#' Find and modify strings containing a specified pattern in a data frame character column.
#'
#' @details
#' `detective()` finds and counts strings matching `.pattern` but not matching `.exclude` in selected
#' columns in `.data`, while `detective()<-` is the equivalent replacement function. Both functions forms
#' allow use of the various possibilities for the `.pattern` argument of [`str_detect`][stringr::str_detect].
#'
#' Character columns in `.data` are selected using \code{\dots} with the
#' <[`tidy-select`][dplyr::dplyr_tidy_select]> syntax of package \pkg{\link[dplyr]{dplyr}}, including use of
#' \strong{selection helpers}.
#'
#' The output may be ordered by the values of selected columns using the syntax of [`arrange`][dplyr::arrange],
#' including use of [`across`][dplyr::across] or [`pick`][dplyr::pick] to select columns with
#' <[`tidy-select`][dplyr::dplyr_tidy_select]> (see examples).
#'
#' @seealso [`arrange()`][dplyr::arrange], [`desc()`][dplyr::desc], [`str_detect()`][stringr::str_detect] and
#'   [`pick()`][dplyr::pick].
#' @family detective
#'
#' @param .data a data frame, or a data frame extension (e.g. a [`tibble`][tibble::tibble-package]).
#'
#' @param \dots <[`tidy-select`][dplyr::dplyr_tidy_select]> `character` or `factor` columns to search and
#'   return.
#'
#' @param .exclude a single `character` string signifying items to be excluded, interpreted as for `.pattern`;
#'   default `NULL`.  
#'
#' @param .arrange_by <[`data-masking`][rlang::args_data_masking]> quoted name(s) of column(s) for ordering  
#'   results. Use \code{\link[dplyr]{desc}} to sort by variables in descending order; default `desc(n)`.
#'
#' @param value a single `character` string providing the replacement value.
#'
#' @inheritParams stringr::str_detect
#'
#' @return `detective()` returns a [`tibble`][tibble::tibble-package] with columns selected using `\dots` and
#'   `n`, giving the count of occurences of each item.
#'
#' @export
#' @examples
#' \dontshow{starwars <- (\() dplyr::starwars)()}
#'
#' starwars |> detective(name, .pattern = "Sky")
#' starwars |> detective(name, .pattern = "Sky", .exclude = "Luke")
#' starwars |> detective(name, .pattern = "Sky", .arrange_by = desc(name))
#'
#' starwars |> detective(name, .pattern = "Darth")
#' starwars |> detective(name, .pattern = "Darth", .exclude = "Vader") <- "Darth The First"
#' starwars |> detective(name, .pattern = "Darth", .arrange_by = desc(name))
#'
#' starwars |> detective(homeworld, species, .pattern = "Human")
#' starwars |> detective(homeworld, species, .pattern = "Human", .exclude = "S")
#' starwars |> detective(homeworld, species, .pattern = "Human", .exclude = "s")
#'
#' starwars |> detective(!c(name, contains("color")))
#' starwars |> detective(
#'         contains("color"), species, .pattern = "brown",
#'        .arrange_by = across(contains("color"))
#'     )
#'
#' starwars |> detective(
#'          name, contains("color"), species, .pattern = "brown",
#'         .exclude = "Human", .arrange_by = across(contains("color"))
#'     )
#'
#' starwars |> detective(
#'         contains("color"), species, .pattern = "brown",
#          .exclude = "Human"
#'     ) <- "chestnut"
#'
#' starwars |> detective(name, contains("color"), species, .pattern = "brown")
#'
#' starwars |> detective(name, contains("color"), species, .pattern = "chestnut")
#'
#' \dontshow{rm(starwars)}
#'

detective <- function(.data, ..., .pattern, .exclude = NULL, .arrange_by = desc(n)) {
    n <- NULL
    pos <- eval_select(expr(c(...) & where(\(x) is.factor(x) | is.character(x))), .data)
    .arrange_by <- enquo(.arrange_by)
    if (!length(pos))
        pos <- eval_select(expr(where(is.character)), .data)
    if (missing(.pattern))
        selrow <- !logical(nrow(.data))
    else {
        selrow <- pos |> map(\(x) str_detect(.data[[x]], .pattern)) |> pmap_lgl(any)
        selrow <- selrow & !is.na(selrow)  ## NA becomes FALSE
    }
    if (!is.null(.exclude)) {
        exlrow <- pos |> map(\(x) str_detect(.data[[x]], .exclude, TRUE)) |> pmap_lgl(all)
        exlrow <- exlrow | is.na(exlrow)  ## NA becomes TRUE
        selrow <- selrow & exlrow
    }
    .data[pos] |>
        filter(selrow) |>
        count(!!!data_syms(names(pos))) |>
        arrange(!!.arrange_by)
}

# ========================================
#  Replacement function
#' @rdname detective
#' @export

`detective<-` <- function(.data, ..., .pattern, .exclude = NULL, value) {
    stopifnot(is.data.frame(.data))
    pos <- eval_select(expr(c(...)), .data)
    if (!is.null(.exclude)) {
        exlrow <- pos |> map(\(x) str_detect(.data[[x]], .exclude, TRUE)) |> pmap_lgl(all)
        exlrow <- exlrow | is.na(exlrow)  ## NA becomes TRUE
    } else
        exlrow <- seq_len(nrow(.data))

    .data |>
        mutate(across(all_of(pos), \(x) modify_at(x, exlrow, \(y) 
            if (str_detect(y, .pattern) & !is.na(y))
                value
            else
                y
        )))
}

# ========================================
#' @title
#' Extract, Sort Unique Values, and Paste Column from Data Frame
#'
#' @description
#' Extract and sort unique values of a selected column from a data frame, and optionally paste into a character string.
#'
#' @details
#' Useful within a piped sequence to quickly extract or review [`sorted`][base::sort], [`unique`][base::unique]
#' contents of a column and optionally collapse into a single character string using [`paste`][base::paste] by
#' providing a suitable value for `.collapse`.
#'
#' @seealso [`paste`][base::paste], [`sort`][base::sort], [`unique`][base::unique].
#' @family detective
#'
#' @param data a data frame, or a data frame extension (e.g. a tibble).
#'
#' @param col <[`data-masking`][rlang::args_data_masking]> quoted name of character column to extract.
#'
#' @param .collapse an optional character string to separate the results, see [`paste`][base::paste]; default
#'   `NULL`.
#'
#' @return A vector of the same type as `col` or a single character string if a value for `.collapse` is
#'   supplied.
#'
#' @export
#' @examples
#' \dontshow{starwars <- dplyr::starwars}
#'
#' starwars |> wizard(homeworld)
#' starwars |> wizard(homeworld, ", ")
#' starwars |> wizard(homeworld, "\t") |> cat()
#' starwars |> wizard(homeworld, "\n") |> cat()
#'
#' \dontshow{rm(starwars)}

wizard <- function(data, col, .collapse = NULL) {
    x <- eval_tidy(expr({{col}}), data = data) |>
    unique() |>
    sort(na.last = T)
    if (!is.null(.collapse))
        paste0(x, collapse = .collapse)
    else x
}
