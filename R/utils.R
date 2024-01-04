# BitsnBobs R Package
# Mark Eisler Jan 2024
# For general bits and bobs of code
#
# Requires R version 4.2.0 (2022-04-22) -- "Vigorous Calisthenics" or later
#
# utils.R


# ========================================
#' Make a Constant with Active Binding
#'
#' @description
#' A constant is an object the value of which cannot be altered once assigned. 
#'
#' @details
#' See reference.
#'
#' @references
#' \href{https://iqis.netlify.app/post/2019/07/22/how-to-make-a-constant-in-r/}{
#'   Siqi Zhang, 2019: Make a Constant in R with Active Binding}.
#'
#' @family utils
#' @seealso [`lockBinding()`][base::lockBinding]
#'
#' @param name a symbol, the name to be assigned to the constant.
#'
#' @param value any valid R object, including a function
#'
#' @return NULL
#'
#' @keywords utilities
#'
#' @export
#' @examples
#' const(bar, "irish")
#' bar
#' try(bar <- "bavarian")
#'

const <- function(name, value){
  assign(deparse(substitute(name)), value, envir = parent.frame())
  lockBinding(substitute(name), env = parent.frame())
}

# ========================================
#' @title
#' Inlinable Marker Function for Function Development
#'
#' @description
#' This inlinable function outputs a user modifiable message identifying its enclosing function using
#' \code{\link[base]{sys.call}} then returns its first argument, if any.
#'
#' @details
#' Mainly useful for the side effect of the message, which can be used to distinguish among versions of the function under
#' development. Can only be used within a function and may be inlined in a `<`[`piped sequence`][base::pipeOp]`>`
#' using `|>`, see examples.
#'
#' @family utils
#' @seealso [`sys.call()`][base::sys.call]
#'
#' @param rval value to be returned; default `NULL`.
#'
#' @param msg ending of the output message. Since this argument follows \var{rval}, which normally would only ever 
#'   be supplied inline, its name cannot be omitted unless either it follows a comma or `marker()` is inlined;
#'   default `"in Mimiland"`.
#'
#' @return The first argument \var{rval} if any, otherwise `NULL`.
#'
#' @keywords internal
#' @export
#' @examples
#' f1 <- function() marker()
#' f1()
#'
#' f2 <- function() marker(msg = "in BitsnBobs")
#' f2()
#'
#' f3 <- function() marker(, "in BitsnBobs")
#' f3()
#'
#' ## Use inline in "piped" sequence
#' f4 <- function(x) x |> marker() |> sqrt()
#' f4(9)
#'
#' f5 <- function(x) x |> marker(msg = "inlined in BitsnBobs") |> exp()
#' f5(1)
#'
#' f6 <- function(x) x |> marker("inlined in BitsnBobs") |> log()
#' f4(1) |> f5() |> f6()
#'
#' rm_objects(f, 1:6)

marker <- function(rval = NULL, msg = "in Mimiland") {
    cat(as.list(sys.call(-1))[[1]], "running", msg, "\n")
    invisible(rval)
}

# ========================================
#' @title
#' Reverse a Matrix by Rows, Columns or Both
#'
#' @description
#' Reverses the order of entire matrix rows, columns or both.
#'
#' @details
#' Depending on the argument `type`, this function reverses the order of entire rows, columns or both of a matrix,
#' including any dimnames if present.
#'
#' @family utils
#' @seealso \code{\link[base]{matrix}}, \code{\link[base]{t}}
#'
#' @param x a `matrix`.
#'
#' @param type a character string specifying whether to reverse matrix rows, columns or both; must be one of
#'   `"byrow"` (default), `"bycolumn"` or `"byboth"`. You can specify just the initial letter.
#'
#' @return A `matrix` of the same dimensions as \var{x}.
#'
#' @keywords array
#' @export
#' @examples
#'  m <- matrix(1:9, nrow = 3, byrow = TRUE, dimnames = list(paste0("x", 1:3), paste0("y", 1:3)))
#'
#'  m
#'  revmat(m)
#'  revmat(m, "bycol")
#'  revmat(m, "byboth")
#'
#'  rm(m)

revmat <- function(x, type = c("byrow", "bycolumn", "byboth")) {
    stopifnot(is.matrix(x))
    type <- match.arg(type) 
    switch(type,
        byrow = x[rev(seq_len(nrow(x))),],
        bycolumn = x[, rev(seq_len(ncol(x)))],
        byboth = x[rev(seq_len(nrow(x))), rev(seq_len(ncol(x)))]
    )
}


# ========================================
#' @title
#' Vectorised Min and Max Operators
#'
#' @name op-min-max
#'
#' @description
#' Vectorised infix functions implementing pmin() and pmax().
#'
#' @details
#' The vectorised infix functions `%:<%` and `%>:%` may be useful in implementing [`pmin()`][base::pmin] and,
#' [`pmax()`][base::pmax] and was inspired by [`%||%`][rlang::op-null-default] in the \pkg{rlang} package.
#'
#' @family utils
#' @seealso [`%||%`][rlang::op-null-default], [`pmin()`][base::pmin] and [`pmax()`][base::pmax].
#'
#' @param x,y numeric or character arguments (see Note).
#'
#' @inherit base::max note
#'
#' @return As for [`pmin()`][base::pmin] and [`pmax()`][base::pmax], a vector of length the longest of the input vectors,
#'   or length zero if one of the inputs had zero length.
#'
#' @keywords univar arith
#' @export
#' @examples
#' 1:10 %:<% 10:1
#' c(1:10, NA) %:<% c(NA, 10:1)
#'
#' 1:10 %>:% 10:1
#' c(1:10, NA) %>:% c(NA, 10:1)
#'

`%:<%` <-  function (x, y) 
    pmin(x, y)

# ========================================
#' @rdname op-min-max
#' @export

`%>:%` <-  function (x, y) 
    pmax(x, y)

# ========================================
#' @title
#' End String with Full Stop and no Other Punctuation or Spaces.
#'
#' @description
#' `endstop()` removes all punctuation and spaces from the end of a string and optionally terminates the
#' string with a full stop.
#'
#' `endstop_data()` removes all punctuation and spaces from the end of selected strings in \var{.data} and
#' optionally terminates the strings with full stops.
#'
#' @details
#' Uses [`str_detect`][stringr:: str_detect] from package \pkg{\link[stringr]{stringr}} to detect the regular
#' expressions `'[:punct:]'` and `'[:space:]'`, and [`str_sub`][stringr:: str_sub] to modify the string.
#'
#' For `endstop_data()`, character columns in `.data` are selected using \code{\dots} with the
#' <[`tidy-select`][dplyr::dplyr_tidy_select]> syntax of package [dplyr][dplyr], including use of
#' \strong{selection helpers} and modified by `endstop()`. If no character columns are selected in \code{\dots},
#' all character columns in \var{.data} will be modified  by `endstop()`.
#'
#' @family utils
#' @seealso  [`str_detect()`][stringr:: str_detect], [`str_sub()`][stringr:: str_sub]
#'
#' @param string a character vector of length one.
#'
#' @param .stop `logical`. Whether or not to add a full stop at the end of the string; default `TRUE`.
#'
#' @param .data a data frame, or a data frame extension (e.g. a [`tibble`][tibble::tibble-package]).
#'
#' @param \dots <[`tidy-select`][dplyr::dplyr_tidy_select]> character columns in \var{.data} to `endstop`.
#'
#' @return For `endstop()`, a character vector of length one, optionally terminating in a full stop. For
#'   `endstop_data()`, a [`tibble`][tibble::tibble-package] derived from `.data`, with selected character
#'   columns modified by `endstop()`.
#'
#' @export
#' @examples
#' "Mimiland" |> endstop()
#' "Mimiland." |> endstop(F)
#' "Mimiland," |> endstop()
#' "Mimiland ." |> endstop()
#' "Mimiland. " |> endstop()
#' s <- "Mimiland.!?\\(){}"
#' cat(s)
#' endstop(s)
#' s <- "Mimiland . ! ? \\ ( ) { } "
#' cat(s)
#' endstop(s, F)
#'
#' starwars3 |> endstop_data(name)
#' starwars3 |> endstop_data(starts_with("sk"))
#' starwars3 |> endstop_data()
#' 
#' rm(s)

# endstop <- function(string, .stop = TRUE) {
    # stopifnot(length(string) == 1)
    # while(str_sub(string, -1L) |> (\(x) any(str_detect(x, "[:punct:]"), str_detect(x, "[:space:]")))())
        # string <- str_sub(string, end = -2L)
    # if (.stop)
        # paste0(string, ".")
    # else
        # string
# }

endstop <- function(string, .stop = TRUE) {
    stopifnot(length(string) == 1)
    if (is.na(string) | !nchar(string))
        string
    else {
        while(str_sub(string, -1L) |> (\(x) any(str_detect(x, "[:punct:]"), str_detect(x, "[:space:]")))())
            string <- str_sub(string, end = -2L)
        if (.stop & nchar(string))
            paste0(string, ".")
        else
            string
    }
}

# ========================================
#  End String within Data with Full Stop and no Other Punctuation or Spaces.
#' @rdname endstop
#' @export

endstop_data <- function(.data, ..., .stop = TRUE) {
	pos <- eval_select(expr(c(...) & where(is.character)), .data)
	if (!length(pos)) {
		message("endstop_data(): no character variables selected in ...; processing all character variables in .data.\n")
		pos <- eval_select(expr(where(is.character)), .data)
    }
	.data |>
        mutate(across(all_of(pos), \(x) map_chr(x, \(y) if (nchar(y) & !is.na(y)) endstop(y, .stop) else y)))
}
