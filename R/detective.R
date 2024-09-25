# BitsnBobs R Package
# Mark Eisler - Sep 2024
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
#' `detective()` finds and counts strings matching `pattern` but not matching `.exclude` in selected
#' columns in `.data`, while `detective()<-` is the equivalent replacement function. Both functions forms
#' allow use of the various possibilities for the `pattern` argument of [`str_detect`][stringr::str_detect].
#' Use `pattern = regex("xyz", ignore_case = TRUE)` for a case insensitive search. Use \pkg{\link[utils]{utils}}
#' package [`glob2rx()`][utils::glob2rx] to change a wildcard or globbing pattern into a regular expression.
#'
#' `character` or `factor` columns in `.data` are selected using \code{\dots} with the
#' <[`tidy-select`][dplyr::dplyr_tidy_select]> syntax of package \pkg{\link[dplyr]{dplyr}}, including use of
#' \strong{selection helpers}.
#'
#' The output may be ordered by the values of selected columns using the syntax of [`arrange()`][dplyr::arrange],
#' including use of [`across()`][dplyr::across] or [`pick()`][dplyr::pick] to select columns with
#' <[`tidy-select`][dplyr::dplyr_tidy_select]> (see examples).
#'
#' @seealso [`across()`][dplyr::across], [`arrange()`][dplyr::arrange], [`desc()`][dplyr::desc],
#'   [`glob2rx()`][utils::glob2rx], [`pick()`][dplyr::pick] and [`str_detect()`][stringr::str_detect].
#'
#' @family detective
#'
#' @param .data a data frame, or a data frame extension (e.g. a [`tibble`][tibble::tibble-package]).
#'
#' @param \dots <[`tidy-select`][dplyr::dplyr_tidy_select]> `character` or `factor` columns to search and
#'   return.
#'
#' @param .exclude a single `character` string signifying items to be excluded, interpreted as for `pattern`;
#'   default `NULL`.  
#'
#' @param .arrange_by <[`data-masking`][rlang::args_data_masking]> quoted name(s) of column(s) for ordering  
#'   results. Use [`desc()`][dplyr::desc] to sort by variables in descending order; default `desc(n)`.
#'
#' @param value a single `character` string providing the replacement value.
#'
#' @inheritParams stringr::str_detect
#'
#' @return `detective()` returns a [`tibble`][tibble::tibble-package] with columns selected using \dots and
#'   `n`, giving the count of occurences of each item.
#'
#' @export
#' @examples
#' \dontshow{starwars <- (\() dplyr::starwars)()}
#'
#' ## Find strings containing a specified pattern in a data frame
#' starwars |> detective("Sky", name)
#' starwars |> detective("Sky", name, .exclude = "Luke")
#'
#' ## Use regex() to make case insensitive
#' starwars |> detective(regex("WALKER", TRUE), name, .arrange_by = desc(name))
#'
#' ## Replace strings containing a specified pattern
#' starwars |> detective("Darth", name)
#' starwars |> detective("Darth", name, .exclude = "Vader") <- "Darth The First"
#' starwars |> detective("Darth", name, .arrange_by = desc(name))
#'
#' ## Exclude strings containing unwanted patterns 
#' starwars |> detective("Human", homeworld, species)
#' starwars |> detective("Human", homeworld, species, .exclude = "S")
#' starwars |> detective("Human", homeworld, species,.exclude = "s")
#' starwars |> detective("Human", homeworld, species, .exclude = regex("s", TRUE))
#'
#' ## Select columns using <tidy-select> syntax from {dplyr},
#' ## including use of “selection helpers”
#' starwars |> detective(glob2rx("*"), !c(name, contains("color")))
#' starwars |> detective(
#'         "brown", contains("color"), species,
#'        .arrange_by = across(contains("color"))
#'     )
#'
#' starwars |> detective(
#'         "brown", name, contains("color"), species,
#'         .exclude = "Human", .arrange_by = across(contains("color"))
#'     )
#'
#' starwars |> detective(
#'         "brown", contains("color"), species,
#          .exclude = "Human"
#'     ) <- "chestnut"
#'
#' starwars |> detective("brown", name, contains("color"), species)
#'
#' starwars |> detective("chestnut", name, contains("color"), species)
#'
#' \dontshow{rm(starwars)}
#'

detective <- function(.data, pattern, ..., .exclude = NULL, .arrange_by = desc(n)) {
    n <- NULL
    pos <- eval_select(expr(c(...) & chr_or_fct()), .data)
    if (!length(pos))
        pos <- eval_select(expr(chr_or_fct()), .data)
    if (missing(pattern))
        selrow <- !logical(nrow(.data))
    else {
        selrow <- pos |> lapply(\(x) str_detect(.data[[x]], pattern)) |> pmap_lgl(any)
        selrow <- selrow & !is.na(selrow)  ## NA becomes FALSE
    }
    if (!is.null(.exclude)) {
        exlrow <- pos |> lapply(\(x) str_detect(.data[[x]], .exclude, TRUE)) |> pmap_lgl(all)
        exlrow <- exlrow | is.na(exlrow)  ## NA becomes TRUE
        selrow <- selrow & exlrow
    }
    .data[pos] |>
        filter(selrow) |>
        count(!!!data_syms(names(pos))) |>
        arrange({{.arrange_by}})
}

# ========================================
#  Replacement function
#' @rdname detective
#' @export

`detective<-` <- function(.data, pattern, ..., .exclude = NULL, value) {
    stopifnot(is.data.frame(.data))
    pos <- eval_select(expr(c(...) & chr_or_fct()), .data)
    posfct <- eval_select(expr(c(...) & where(is.factor)), .data)
    if (!is.null(.exclude)) {
        exlrow <- pos |> lapply(\(x) str_detect(.data[[x]], .exclude, TRUE)) |> pmap_lgl(all)
        exlrow <- exlrow | is.na(exlrow)  ## NA becomes TRUE
    } else
        exlrow <- seq_len(nrow(.data))

    .data |>
        mutate(
           across(all_of(posfct), \(x) {
                x |>
                fct_expand(value) |>
                fct_relevel(sort)
            }),
            across(all_of(pos), \(x) modify_at(x, exlrow, \(y) 
                if (str_detect(y, pattern) & !is.na(y))
                    value
                else
                    y
            )),
            across(all_of(posfct), fct_drop)        
       )
}

# ========================================
#' @title
#' Extract Unique Values from Data Frame Columns, Sort and Concatenate
#'
#' @description
#' `wizard()` extracts and sorts unique values of a selected column from a data frame, and optionally pastes into
#' a character string.
#'
#' `data_wizard()` does the same across all columns in a  data frame.
#'
#' @details
#' `wizard()` can be useful within a piped sequence to quickly extract or review [`sorted`][base::sort],
#' [`unique`][base::unique] contents of a column and optionally collapse into a single character string using
#' [`paste`][base::paste] by providing a suitable value for `.collapse`.
#'
#' `data_wizard()` invokes `wizard()` for all columns in `data` that are one of the [`atomic`][base::vector] types,
#' ignores columns of other types and shows a warning if any present. If `.collapse` is used with `data_wizard()`,
#' all columns are returned as `character vectors`. 
#'
#' Further information at \href{https://mark-eis.github.io/BitsnBobs/articles/Using-wizard.html}{Using wizard}.
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
#' @param .noquote `logical` whether to return an object of class `"noquote"`; default `FALSE` for `wizard()`, `TRUE`
#'   for  `data_wizard()`.
#'
#' @return
#' For `wizard()`, a vector of the same type as `col` or a single character string if a value for `.collapse` is
#'   supplied.
#'
#' For `data_wizard()`, a [`list`][base::list] corresponding to columns in `data` that are of [`atomic`][base::vector]
#'   types, with list elements as for `wizard()`.
#'
#' @export
#' @examples
#' \dontshow{starwars <- dplyr::starwars}
#'
#' starwars |> wizard(homeworld)
#' starwars |> wizard(homeworld, ", ")
#' starwars |> wizard(homeworld, ", ", TRUE)
#' starwars |> wizard(homeworld, "\t") |> cat()
#' starwars |> wizard(homeworld, "\n") |> cat()
#'
#' data_wizard(mtcars) 
#'
#' data_wizard(starwars, ", ") 
#'
#' \dontshow{rm(starwars)}

wizard <- function(data, col, .collapse = NULL, .noquote = FALSE) {
    stopifnot(is.data.frame(data))
    x <- eval_tidy(expr({{col}}), data = data) |>
    unique() |>
    sort(na.last = T)
    if (!is.null(.collapse))
        x <- paste0(x, collapse = .collapse)
    if (.noquote)
        noquote(x)
    else x
}

# ========================================
# Extract, Sort Unique Values, and Paste all Columns in Data Frame
#' @rdname wizard
#' @export

data_wizard <- function(data, .collapse = NULL, .noquote = TRUE) {
    stopifnot(is.data.frame(data))
    types <- data |> purrr::map_lgl(is.atomic)
    if (!all(types))
        if (!any(types))
            warning("No columns with atomic types in `data`.")
        else
            warning(
                "Omitting `data` column(s) \"",
                paste(names(types[!types]), collapse = "\", \""),
                "\" with non-atomic types."
            )
    data |>
    select(where(is.atomic)) |>
    lapply(wizard, data = data, .collapse = .collapse, .noquote = .noquote)
}


# ========================================
# chr_or_fct
# Predicate function
# Not exported

chr_or_fct <- function()
    force(\(x) is.factor(x) | is.character(x))