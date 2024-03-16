# BitsnBobs R Package
# Mark Eisler - Mar 2024
# For general bits and bobs of code
#
# Requires R version 4.2.0 (2022-04-22) -- "Vigorous Calisthenics" or later
#
# retriever.R


# ========================================
#' @title
#' 'Function Factory' for Bespoke Data Frame Retrieval and Replacement
#'
#' @description
#' `retriever()` creates bespoke functions for retrieval of rows of a data frame identified using a specified
#' index column.
#'
#' `remplacer()` creates bespoke replacement functions for modifying rows of a data frame identified using a
#'   specified index column.
#'
#' @details
#' The `key` value supplied as an argument to functions derived from both `retriever()` and `remplacer()` must be exact
#' matches with values in the `index` column for rows to be retrieved or replaced. For pattern matching, use
#' [`detective()`][detective]. 
#'
#' If `labile_data` is `TRUE`, `data` are represented in the function environment as a [`quosure`][rlang::topic-quosure],
#' and functions returned by `retriever()` will automatically refer to the current version of `data` in its original
#' [`environment`][base::environment], usually the calling environment i.e., typically but not necessarily the global
#' environment. If `labile_data` is `FALSE`, returned functions refer to a copy of `data` saved in the function
#' environment at the time of execution of `retriever()`, and will not reflect any subsequent changes to the original
#' `data`. 
#'
#' @note
#' The original idea was for `remplacer()` to create bespoke replacement functions using `data` captured in the function
#'   environment as with `retriever()`. However, there is no immediately apparent way to do this as the object to be
#'   "replaced" must be the first argument for replacement functions. 
#'
#' @family detective
#' @seealso  [`data frame`][base::data.frame]
#'
#' @param data a [`data frame`][base::data.frame], or a data frame extension (e.g. a `tibble`).
#'
#' @param index <[`data-masking`][rlang::args_data_masking]> quoted name of the index column to be searched.
#'
#' @param labile_data `logical`. If `TRUE`, `data` are represented in the function environment as a `quosure`.
#'   If `FALSE`, a copy of `data` is saved in the function environment. Default `TRUE`.
#'
#' @return
#' For `retriever()`, a [`function`][base::function] for retrieval of specified columns of `data` in rows matching a
#'   specified value in the `index` column, having the following arguments: –
#'
#' \item{key}{Value to be matched in the `index` column.}
#'
#' \item{\dots}{<[`tidy-select`][dplyr::dplyr_tidy_select]> character columns to return. If none are provided, all
#'   columns are returned.}
#'
#' For `remplacer()`, a replacement `function` for modifying a value in a specified column of `data` in rows matching a
#'   specified value in the index column, having the following arguments: –
#'
#' \item{data}{A [`data frame`][base::data.frame], or a data frame extension (e.g. a `tibble`).}
#'
#' \item{key}{Value to be matched in the `index` column.}
#'
#' \item{replace}{<[`data-masking`][rlang::args_data_masking]> quoted name of the column of the value to be replaced.}
#'
#' \item{value}{The replacement value of the same type as `replace`.}
#'
#' @export
#' @examples
#' \dontshow{starwars <- (\() dplyr::starwars)()}
#'
#' ## Create "retrieval" function with labile_data TRUE
#' retrieve_starwars <- retriever(starwars, name)
#'
#' ## `data` represented as "quosure" in function environment 
#' environment(retrieve_starwars)$data
#'
#' ## Retrieve selected columns for a row specified using the index
#' retrieve_starwars("Luke Skywalker", ends_with("color"), homeworld)
#'
#' ## Create "retrieval" function with labile_data FALSE
#' retrieve_original_starwars <- retriever(starwars, name, FALSE)
#'
#' ## Copy of `data` saved in function environment 
#' environment(retrieve_original_starwars)$data
#'
#' ## Retrieve selected columns for a row specified using the index
#' retrieve_original_starwars("Luke Skywalker", ends_with("color"), homeworld)
#'
#' ## Create replacement function
#' `replace_at_name<-` <- remplacer(name)
#'
#' ## Replace the value of a selected column for a row specified using the index
#' starwars |> replace_at_name("Luke Skywalker", homeworld) <- "Mimiland"
#'
#' ## Retrieve selected columns for a row specified using the index
#' ## "retrieval" function with labile_data TRUE reflects the change
#' retrieve_starwars("Luke Skywalker", ends_with("color"), homeworld)
#'
#' ## Retrieve selected columns for a row specified using the index
#' ## "retrieval" function with labile_data FALSE shows no change
#' retrieve_original_starwars("Luke Skywalker", ends_with("color"), homeworld)
#' 
#' rm(retrieve_starwars, retrieve_original_starwars, `replace_at_name<-`, starwars)
#'

retriever <- function(data, index, labile_data = TRUE) {
    data <- {
        if (labile_data) enquo(data) else force(data)
    }
    index <- enquo(index)
    stopifnot(is.data.frame(if (labile_data) eval_tidy(data) else data))

    function(key, ...) {
        data <- {
            if (labile_data) eval_tidy(data) else data
        } |>
        filter(!!index == key)

        if(...length())
            select(data, !!index, ...)
        else
            select(data, everything())
    }
}

# _________________________________________________
# Function factory to create replacement function
#' @rdname retriever
#' @export

remplacer <- function(index) {
    index <- enquo(index)

    function(data, key, replace, value) {
        stopifnot(is.data.frame(data))
        data |>
            mutate({{replace}} := if_else(!!index == key, value, {{replace}}))
    }
}
