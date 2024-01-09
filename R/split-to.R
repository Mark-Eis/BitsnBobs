# BitsnBobs R Package
# Mark Eisler - Jan 2024
# For general bits and bobs of code
#
# Requires R version 4.2.0 (2022-04-22) -- "Vigorous Calisthenics" or later
#
# split-to.R


#' Split Strings In A Data Frame Character Column Into New Rows or Columns
#'
#' @description
#' Duplicate columns or rows in a data frame by splitting strings in a character column at a given pattern to
#' disaggregate data.
#'
#' @details 
#' Similar to the more sophisticated \pkg{\link[tidyr]{tidyr}} function \code{\link[tidyr]{separate_wider_delim}} on
#'  which it is based, \code{split_to_cols()} trims leading and trailing whitespace using \code{\link[stringr]{str_trim}}
#' and also optionally removes parentheses from the string in the newly created column \code{split_to}. This was the main
#' motivation for creating this function, which can be used for conveniently disaggregating addenda to strings contained
#' in parentheses, see examples. The trade-off is that unlike \code{separate_wider_delim()}, function
#' \code{split_to_cols()} only handles a single split.
#'
#' \pkg{BitsnBobs}'s homegrown \code{split_to_rows()} succeeds with \code{pattern = "\\\\r\\\\n"} representing
#' the CRLF escape sequence that results for example, from an imported Excel file sporting within-cell line breaks,
#' whereas the otherwise similar but more sophisticated \pkg{\link[tidyr]{tidyr}} function
#' \code{\link[tidyr]{separate_longer_delim}()} does not*. Like \code{separate_longer_delim()}, function
#' \code{split_to_rows()} handles multiple splits.
#'
#' Both \code{split_to_rows()} and \code{split_to_cols()} also add the convenience of trimming leading and trailing
#' whitespaces from the split strings using \code{\link[stringr]{str_trim}}, whereas the \pkg{\link[tidyr]{tidyr}}
#' functions do not afford this luxury.
#'
#' *Note that \code{separate_longer_delim()} does succeed with the equivalent CRLF hex sequence \verb{"\u000D\u000A"};
#' trimming leading and trailing whitespaces may be \code{split_to_rows}'s main advantage.
#'
#' @seealso \code{\link[tidyr]{separate_longer_delim}}, \code{\link[tidyr]{separate_wider_delim}},
#'    \code{\link[stringr]{str_split}} and \code{\link[stringr]{str_trim}}.
#' @family split_to
#'
#' @param data a data frame, or a data frame extension (e.g. a tibble).
#'
#' @param col_to_split <[`data-masking`][rlang::args_data_masking]> quoted name of character column to be split.
#'
#' @param split_to <[`data-masking`][rlang::args_data_masking]> quoted name of a new character column to be created in \code{data}.
#'
#' @param pattern a single character string representing the pattern to split by. \code{split_to_rows()} allows use
#'   of the various possibilities for the \code{pattern} argument of \code{\link[stringr]{str_split}}.
#'
#' @param remove_parenth \code{logical} specifying whether parentheses should be removed from the strings in the new
#'   column \code{split_to}; default \code{TRUE}.
#'
#' @return A \code{\link[tibble]{tibble}} data frame based on \code{data}. With \code{split_to_cols()}, it has the same
#'   rows but different columns and with \code{split_to_rows()}, it has the same columns but different rows.
#'
#' @keywords datagen 
#' @export
#' @examples
#'
#' starwars3
#' split_to_rows(starwars3, skin_color, ",")
#' tidyr::separate_longer_delim(starwars3, skin_color, ",")
#'
#' split_to_cols(starwars3, skin_color, alt_skin_color, ",")
#' tidyr::separate_wider_delim(
#'     starwars3, skin_color, ",",
#'     names = c("skin_color", "alt_skin_color"),
#'     too_few = "align_start",
#'     too_many = "merge"
#' )
#'
#' starwars3[3, 2]$skin_color <- "white\r\nblue"
#' starwars3[8, 2]$skin_color <- "silver\r\nred"
#' starwars3
#'
#' split_to_rows(starwars3, skin_color, "\\r\\n")
#' tidyr::separate_longer_delim(starwars3, skin_color, "\\r\\n")
#'
#' starwars3[3, 2]$skin_color <- "white (blue)"
#' starwars3[4, 2]$skin_color <- "brown, [white]"
#' starwars3[5, 2]$skin_color <- "green-tan, {brown}"
#' starwars3[8, 2]$skin_color <- "white (red]"
#' starwars3
#'
#' split_to_cols(starwars3, skin_color, alt_skin_color, " ")
#' split_to_cols(starwars3, skin_color, alt_skin_color, " ", remove_parenth = TRUE)
#' tidyr::separate_wider_delim(
#'     starwars3, skin_color, " ",
#'     names = c("skin_color", "alt_skin_color"),
#'     too_few = "align_start",
#'     too_many = "merge"
#' )
#'

split_to_cols <- function(data, col_to_split, split_to, pattern, remove_parenth = FALSE) {
	col_to_split <- enquo(col_to_split)
	split_to <- enquo(split_to)
	
	data |>
		tidyr::separate_wider_delim(
			!!col_to_split,
			pattern,
			names = c(as_label(col_to_split), as_label(split_to)),
			too_few = "align_start",
			too_many = "merge"
		) |>
		(\(x) 
			if (remove_parenth)
				mutate(x, across(!!split_to, \(x) gsub("[][(){}]", "", x)))
			else
				x
		)() |>
		mutate(across(c(!!col_to_split, !!split_to), str_trim))
}

# ========================================
#  Split Strings In A Data Frame Character Column Into New Rows
#' @rdname split_to_cols
#' @export

split_to_rows <- function(data, col_to_split, pattern) {
	mutate(data, across({{col_to_split}}, \(x) str_split(x, pattern) |> map(str_trim))) |>
	tidyr::unnest_longer(col = {{col_to_split}})
}

