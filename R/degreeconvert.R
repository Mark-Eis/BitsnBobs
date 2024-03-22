# BitsnBobs R Package
# Mark Eisler Mar 2024
# For general bits and bobs of code
#
# Requires R version 4.2.0 (2022-04-22) -- "Vigorous Calisthenics" or later
#
# degreeconvert.R


# ========================================
#' Convert Degrees, Minutes and Seconds to Decimal Degrees
#'
#' @description
#' Convert degrees, minutes and seconds to decimal degrees. 
#'
#' @details
#' See reference.
#'
#' @family degreeconvert
#' @seealso 
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
#' degminsec(49.32464)
#' degminsec(49.32464, pointafter = "deg")
#' degminsec(4932.464, pointafter = "min")
#' degminsec(493246.4, pointafter = "sec")

degminsec <- function(x, ...) {
    UseMethod("degminsec")
}

# ========================================
#  Convert Degrees, Minutes and Seconds to Decimal Degrees
#  S3method degminsec.default()
#'
#' @rdname degminsec
#' @export

degminsec.default <- function(dms, ..., pointafter = c("deg", "min", "sec")) {
    new_degminsec(dms, pointafter) |> validate_degminsec()
}

# ========================================
#  Constructor
#  new_degminsec()
#'
#' @rdname degminsec
#  not exported

new_degminsec <- function(dms, pointafter = c("deg", "min", "sec")) {
    pointafter <- match.arg(pointafter)
	pax <- switch(pointafter,
		    deg = 1e0L,
		    min = 1e2L,
		    sec = 1e4L
	    )
    structure(
        list(
            dd = as.integer(dms %/% pax),
            mm = as.integer((dms %% pax) * 100 / pax),
            ss = ((dms %% pax) * 10000 / pax) %% 100
        ),
        class = "degminsec"
    )
}


# ========================================
#  Validator
#  validate_degminsec()
#'
#' @rdname degminsec
#  not exported

validate_degminsec <- function(dms) {

    if (!inherits(dms, "degminsec"))
      stop(
        "`dms` must be of class \"degminsec\"",
        call. = FALSE
      )

    if (abs(dms$dd) > 180)
      stop(
        "`dms$dd` must not be greater than 180",
        call. = FALSE
      )

    if (!abs(dms$mm) < 60)
      stop(
        "`dms$mm` must be less than 60",
        call. = FALSE
      )

    if (!abs(dms$s) < 60)
      stop(
        "`dms$ss` must be less than 60",
        call. = FALSE
      )
    
    dms
}
