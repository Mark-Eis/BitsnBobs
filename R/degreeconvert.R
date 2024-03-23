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
#' `dms_to_decdeg()` is a vectorised function that works with individual coordinates, latitude and longitude
#' values paired in a named list (see examples) or with a longer list of coordinates.
#'
#' @family degreeconvert
#'
#' @param dms an object of class [`"degminsec"`][degminsec], representing a coordinate of latitude or longitude in
#'   degrees, minutes and seconds or a list of "degminsec" objects.
#'
#' @return numeric, a coordinate of latitude or longitude in decimal degrees
#'
#' @keywords utilities
#'
#' @export
#' @examples
#' degminsec(49.32464) |> dms_to_decdeg()
#'
#' (coords <- list(lat = degminsec(49.32464), long = degminsec(18.2354822)))
#' dms_to_decdeg(coords)
#'
#' rm(coords)

dms_to_decdeg <- function(x, ...) {
    UseMethod("dms_to_decdeg")
}

# ========================================
#  Convert Degrees, Minutes and Seconds to Decimal Degrees
#  S3method degminsec.default()
#'
#' @rdname dms_to_decdeg
#' @export

dms_to_decdeg.default <- function(dms) {
    convdms <- function(x) with(x, dd + mm / 60 + ss / 3600)
    if (inherits(dms, "degminsec"))
        convdms(dms)
    else {
        stopifnot(all(purrr::map_lgl(dms, \(x) (inherits(x, "degminsec")))))
        map_dbl(dms, convdms)
    }
}

# ========================================
#' Create Degrees, Minutes and Seconds Object
#'
#' @description
#' The function `degminsec()` is used to create (latitude or longitude) coordinate objects represented in degrees,
#' minutes and seconds. 
#'
#' @details
#' `degminsec()` is a generic S3 function. The default method works with a numeric (`double`) representing a
#' coordinate of latitude or longitude in degrees, minutes and seconds. The argument `pointafter` indicates the
#' position of the decimal point, which may be placed after the degrees, the minutes or the (whole) seconds, and by
#' default assumed to be placed after the degrees.
#'
#' @family degreeconvert
#'
#' @param degminsec numeric, representing a coordinate of latitude or longitude in degrees, minutes and seconds.
#'
#' @param pointafter a character string specifying the position of the decimal point in `degminsec`; must be one of
#'   "deg" (default), "min", or "sec". You can specify just the initial letter.
#'
#' @return An object of class `"degminsec"`, representing a coordinate of latitude or longitude in degrees, minutes
#'   and seconds as a named list with components: -
#'
#' \item{dd}{degrees represented by an integer with maximum absolute value of 180.}
#'
#' \item{mm}{minutes represented by a positive integer with value less than 60.}
#'
#' \item{ss}{seconds represented by a positiveinteger with value less than 60.}
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
#  Create Degrees, Minutes and Seconds Object
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
#
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
#
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

# ========================================
# Print degminsec Object
#  S3method print.degminsec()
#'
#' @rdname degminsec
#' @export

print.degminsec <- function(x, ...) {
    with(x, cat(paste("\n\t", dd, "degrees,", mm, "minutes,", zapsmall(ss), "seconds\n")))
    invisible(x)
} 
