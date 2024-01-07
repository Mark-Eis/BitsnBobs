# BitsnBobs R Package
# Mark Eisler Dec 2023
# For general bits and bobs of code
#
# Requires R version 4.2.0 (2022-04-22) -- "Vigorous Calisthenics" or later
#
# print.R


# ========================================
#' Pipe-Friendly Line Feeds and Printing
#'
#' @description
#' \code{lf()} outputs one or more line feeds during a piped sequence.
#'
#' @details
#' \code{print_lf()} prints an object in a piped sequence then outputs one or more line feeds.
#'
#' An object passed as argument in a piped sequence is printed and/or one or more line feeds are
#' output during a piped sequence using \code{\link[base]{cat}()}. This can be useful to separate
#' lines of printed output, see examples. 
#'
#' @seealso \code{\link[base]{cat}}.
#' @family print
#'
#' @param x Object to be piped.
#'
#' @param n Number of line feeds; default \code{1}.
#'
#' @return Invisibly returns its first argument.
#'
#' @keywords print
#' @export
#' @examples
#' obj <- "Lorem ipsum dolor sit amet"
#' obj |> lf()               # line feed, object returned invisibly
#' obj |> lf(3)              # three line feeds, object returned invisibly
#' (obj |> lf(3))            # three line feeds, returned object rendered visible
#' obj |> lf(3) |> paste("consectetur adipiscing elit", sep = ", ")
#'
#' obj |> print() |> lf(3)   # line feeds are unexpectedly before printed output; use print_lf() instead.
#'
#' obj |> print_lf()         # object printed with line feed and returned invisibly
#' obj |> print_lf(3)        # object printed with three line feeds and returned invisibly
#' (obj |> print_lf(3))      # Ditto, then rendered visible
#' obj |> print_lf(3) |> paste("consectetur adipiscing elit", sep = ", ")
#'
#' rm(obj)

lf <- function(x, n = 1) {
    linefeed(n)
    invisible(x)
}

# ========================================
#  Pipe-Friendly Print and Line Feeds
#' @rdname lf
#'
#' @export

print_lf <- function(x, n = 1) {
    print(x)
    lf(x, n)
}

# ========================================
#  Add Linefeeds to Printing
#  Not exported

linefeed <- function(n)
    if(!is.null(n)) {
        n <- as.integer(n[[1]])
        if (n < 0)
            n <- 0L
        cat(rep("\n", n))
    }

# ========================================
#  Constructor for a Catapult Print Object
#  new_catapult()
#
#  Not exported

new_catapult <- function(object = vector(), lead = "Catapult: -\n", revert = FALSE, ...) {
    stopifnot(!is.null(object))
    structure(object, class = c("catapult", class(object)), lead = lead, revert = revert, ...)
}

# ========================================
#  Validator for a Catapult Print Object
#  validate_catapult()
#
# # Not exported, but probably should be…
# #' @export

validate_catapult <- function(x) {
    lead <- x %@% lead

    if(!is.character(lead)) {
	    stop(
	        "\"lead\" attribute must be of type character",
		     call. = FALSE
	    )
    }

    if(!length(lead) || !nchar(lead)) {
	    stop(
	        "\"lead\" attribute cannot be empty",
		     call. = FALSE
	    )
    }

    if(!is.logical(x %@% revert)) {
	    stop(
	        "\"revert\" attribute must be of type logical",
		     call. = FALSE
	    )
    }

    x
}


# ========================================
#' Catapult Class for Consistent Printing
#'
#' @description
#' Creates an object of class `"catapult"` with a built-in title string used for printing.
#'
#' @details
#' \code{catapult()} converts an object to class `"catapult"`, inheriting from its existing class(es).
#'
#' @note Deprecated, use instead \pkg{ParaAnita} [`announce()`][ParaAnita::announce].
#'
#' @seealso [`cat()`][base::cat], [`print()`][base::print].
#' @family print
#'
#' @param object Object to be converted to `"catapult"` class.
#'
#' @param lead a `character` string giving the title to be printed.
#'
#' @param revert `logical` indicating whether the `"revert"` attribute should be set to `TRUE` or `FALSE`. By default,
#' when `FALSE`, the corresponding print method `print.catapult()` will invoke further print methods for the underlying
#' inherited `object`; if `TRUE`, `print.catapult()` will return to the calling function after printing `lead`.
#'
#' @param \dots additional named arguments to be forwarded to print methods of classes inherited from `object`, for
#'   example as required for formatting and printing a [`data frame`][base::print.data.frame].
#'
#' @return An object of class `"catapult"` inheriting class(es) from `object`.
#'
#' @keywords print
#' @export
#' @examples
#' catapult()
#' (cpt <- catapult("x", lead = "Lorem ipsum dolor sit amet"))
#' .class2(cpt)
#' attr(cpt, "revert")
#'
#' (cpt <- catapult("x", lead = "Lorem ipsum dolor sit amet", TRUE))
#' .class2(cpt)
#' attr(cpt, "revert")
#'
#' rm(cpt)

# ========================================
#  "Helper" Function for Constructing a Catapult Print Object

catapult <- function(object = vector(), lead = "Catapult", revert = FALSE, ...) {
    lead <- as.character(lead)
    revert <- as.logical(revert)
	validate_catapult(new_catapult(object, lead, revert))
}

# ========================================
#  Print a Catapult Print Object
#  S3method print.contingency_table()
#
#' @rdname catapult
#' @export

print.catapult <- function(x, ...) {
	validate_catapult(x)
    .lead <- x %@% lead
    cat(paste0(rep(c("_", "\n", .lead, ": -\n\n"), c(nchar(.lead) + 3, 1, 1, 1)), collapse = ""))

    if (!x %@% revert) {
        x %@% lead <- x %@% revert <- NULL
        class(x) <- class(x)[!class(x) %in% "catapult"]
        NextMethod()
	    class(x) <- .Class
	    x %@% lead <- .lead
	    x %@% revert <- FALSE
    }
    invisible(x)
}
