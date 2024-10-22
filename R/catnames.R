# BitsnBobs R Package
# Mark Eisler - Jan 2024
# For general bits and bobs of code
#
# Requires R version 4.2.0 (2022-04-22) -- "Vigorous Calisthenics" or later
#
# catnames.R


# ========================================
#' @title
#' Concatenate First Names and Surnames in a Data Frame into Delimiter-Separated Character Strings
#'
#' @description
#' Concatenates first names and surnames contained in individual columns of a data frame. The resulting first name-surname
#' pairs are themselves further combined into one or more delimiter-separated character strings on the basis of selected
#' grouping variables.
#'
#' @details
#' A number of first name-surname pairs in \code{data} are all concatenated in a single string for each of one or more
#' grouping variables selected using \code{\dots}. Grouping variables in \code{.data} are selected using the \code{\dots}
#' argument with the <[`tidy-select`][dplyr::dplyr_tidy_select]> syntax of package \pkg{\link[dplyr]{dplyr}}, including
#' use of \strong{selection helpers}.
#'
#' By default, the name pairs within a string are separated using commas or otherwise using \code{.delimiter}, if
#' provided.
#'
#' @seealso [`paste`][base::paste], <[`tidy-select`][dplyr::dplyr_tidy_select]>.
#'
#' @param data a data frame, or a data frame extension (e.g. a tibble).
#'
#' @param firstname quoted name of column containing first names; default \code{Firstname}.
#'
#' @param surname quoted name of column containing surnames; default \code{Surname}.
#'
#' @param \dots <[`tidy-select`][dplyr::dplyr_tidy_select]> names of variables to group by.
#'
#' @param .delimiter a character string to separate the first name-surname pairs, see \code{collapse} argument of
#'   [`paste`][base::paste]; default \code{", "}.
#'
#' @return A [tibble][tibble::tibble-package] data frame containing the column \code{Names} and further individual columns
#'   for each grouping variable.
#'
#' @export
#' @examples
#'
#' starwars2 ## Has name column of original starwars split into Firstname and Surname
#'
#' starwars2 |>
#'   cat_names(Firstname, Surname, homeworld) |>
#'   print_all()
#' 
#' starwars2 |>
#'   cat_names(,, species, .delimiter = "; ") |>
#'   print_all()
#' 
#' starwars2 |>
#'   cat_names(,, homeworld, species) |>
#'   print_all()
#' 

cat_names <- function(data, firstname = Firstname, surname = Surname, ..., .delimiter = ", ") {
    Firstname <- Surname <- NULL
    firstname = enquo(firstname)
    surname = enquo(surname)

    stopifnot(
        is.data.frame(data),
        !is.null(data[[as_name(firstname)]]), !is.null(data[[as_name(surname)]]),
        is.character(eval_tidy(firstname, data)), is.character(eval_tidy(surname, data))
    )

    pos <- eval_select(expr(c(...)), data)

    data[c(eval_select(expr(c(!!firstname, !!surname)), data), pos)] |>
        mutate(Names = paste(!!firstname, !!surname, collapse = .delimiter), .by = names(pos), .keep = "unused") |>
        unique() |>
        arrange(...)
}

