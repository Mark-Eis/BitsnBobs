# BitsnBobs R Package
# Mark Eisler Mar 2024
# For general bits and bobs of code
#
# Requires R version 4.2.0 (2022-04-22) -- "Vigorous Calisthenics" or later
#
# degreeconvert.R


# ========================================
#' Create Decimal Degrees Object
#'
#' @description
#' The function `decdeg()` is used to create (latitude or longitude) coordinate objects represented in decimal degrees. 
#'
#' @details
#' `decdeg()` is a generic S3 function. The default method works with a numeric (`double`) representing a
#' coordinate of latitude or longitude in decimal degrees.
#'
#' @family degreeconvert
#'
#' @param object numeric, representing a coordinate of latitude or longitude in decimal degrees.
#'
#' @param \dots further arguments passed to or from other methods.
#'
#' @param x object to be printed.
#'
#' @return An object of class `"decdeg"`, representing a coordinate of latitude or longitude in decimal degrees
#'   represented by a numeric of type `double` with maximum absolute value of 180.
#'
#' @keywords utilities
#'
#' @export
#' @examples
#' decdeg(49.54622)

decdeg <- function(object, ...) {
    UseMethod("decdeg")
}

# ========================================
#  Create Decimal Degrees Object
#  S3method decdeg.default()
#'
#' @rdname decdeg
#' @export

decdeg.default <- function(object, ...) {
    check_dots_empty()
    new_decdeg(object) |> validate_decdeg()
}

# ========================================
#  Constructor
#  new_decdeg()
#
#  not exported

new_decdeg <- function(d)
    structure(d, class = "decdeg")

# ========================================
#  Validator
#  validate_decdeg()
#
#  not exported

validate_decdeg <- function(dec_deg) {

    if (!inherits(dec_deg, "decdeg"))
      stop(
        "`dec_deg` must be of class \"decdeg\"",
        call. = FALSE
      )

    if (abs(dec_deg) > 180)
      stop(
        "`dec_deg` must not be greater than 180",
        call. = FALSE
      )
    
    dec_deg
}

# ========================================
# Print decdeg Object
#  S3method print.decdeg()
#'
#' @rdname decdeg
#' @export

print.decdeg <- function(x, ...) {
    cat(paste("\n\t", zapsmall(x), "decimal degrees\n"))
    invisible(x)
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
#' coordinate of latitude or longitude in degrees, minutes and seconds. The argument `.after` indicates the
#' position of the decimal point, which may be placed after the degrees, the minutes or the (whole) seconds, and by
#' default assumed to be placed after the degrees.
#'
#' @family degreeconvert
#'
#' @param object numeric, representing a coordinate of latitude or longitude in degrees, minutes and seconds.
#'
#' @param .after a character string indicating the position of the decimal point in `object`; must be one of
#'   "deg" (default), "min", or "sec". You can specify just the initial letter.
#'
#' @inheritParams decdeg
#'
#' @return An object of class `"degminsec"`, representing a coordinate of latitude or longitude in degrees, minutes
#'   and seconds as a named list with components: -
#'
#' \item{deg}{degrees represented by an integer with maximum absolute value of 180.}
#'
#' \item{min}{minutes represented by a positive integer with value less than 60.}
#'
#' \item{sec}{seconds represented by a positive numeric with value less than 60.}
#'
#' @keywords utilities
#'
#' @export
#' @examples
#' degminsec(49.32464)
#' degminsec(49.32464, .after = "deg")
#' degminsec(4932.464, .after = "min")
#' degminsec(493246.4, .after = "sec")

degminsec <- function(object, ...) {
    UseMethod("degminsec")
}

# ========================================
#  Create Degrees, Minutes and Seconds Object
#  S3method degminsec.default()
#'
#' @rdname degminsec
#' @export

degminsec.default <- function(object, ..., .after = c("deg", "min", "sec")) {
    check_dots_empty()
    new_degminsec(object, .after) |> validate_degminsec()
}

# ========================================
#  Constructor
#  new_degminsec()
#
#  not exported

new_degminsec <- function(dms, .after = c("deg", "min", "sec")) {
    .after <- match.arg(.after)
    pax <- switch(.after,
            deg = 1e0L,
            min = 1e2L,
            sec = 1e4L
        )
    structure(
        list(
            deg = as.integer(dms %/% pax),
            min = as.integer((dms %% pax) * 100 / pax),
            sec = ((dms %% pax) * 10000 / pax) %% 100
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

    if (abs(dms$deg) > 180)
      stop(
        "`dms$deg` must not be greater than 180",
        call. = FALSE
      )

    if (!abs(dms$min) < 60)
      stop(
        "`dms$min` must be less than 60",
        call. = FALSE
      )

    if (!abs(dms$sec) < 60)
      stop(
        "`dms$sec` must be less than 60",
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
    with(x, cat(paste("\n\t", deg, "degrees,", min, "minutes,", zapsmall(sec), "seconds\n")))
    invisible(x)
} 

# ========================================
#' Convert Degrees, Minutes and Seconds to Decimal Degrees
#'
#' @description
#' Convert degrees, minutes and seconds to decimal degrees. 
#'
#' @details
#' `dms_to_decdeg()` is an S3 function that works with individual coordinates, latitude and longitude
#' values paired in a named list (see examples) or with a longer list of coordinates.
#'
#' @family degreeconvert
#'
#' @param object an object of class [`"degminsec"`][degminsec], representing a coordinate of latitude or longitude in
#'   degrees, minutes and seconds or a list of "degminsec" objects.
#'
#' @inheritParams degminsec
#'
#' @return An object of class [`"decdeg"`][decdeg], representing a coordinate of latitude or longitude in decimal
#'   degrees represented by a numeric of type `double` with maximum absolute value of 180, or a list of such objects.
#'
#' @keywords utilities
#'
#' @export
#' @examples
#'
#' dms_to_decdeg(49.32464)
#' dms_to_decdeg(49.32464, .after = "deg")
#' dms_to_decdeg(4932.464, .after = "min")
#' dms_to_decdeg(493246.4, .after = "sec")
#'
#' (coord <- degminsec(49.32464))
#' dms_to_decdeg(coord)
#'
#' (coords <- list(lat = degminsec(49.32464), long = degminsec(18.2354822)))
#' dms_to_decdeg(coords)
#'
#' rm(coord, coords)

dms_to_decdeg <- function(object, ...) {
    UseMethod("dms_to_decdeg")
}

# ========================================
#  Convert Degrees, Minutes and Seconds to Decimal Degrees
#  S3method dms_to_decdeg.default()
#'
#' @rdname dms_to_decdeg
#' @export

dms_to_decdeg.default <- function(object, ..., .after = c("deg", "min", "sec")) {
    check_dots_empty()
    stopifnot(is.numeric(object))
    degminsec(object, .after = .after) |>
    dms_to_decdeg()
}

# ========================================
#  Convert Degrees, Minutes and Seconds in a "degminsec" object to Decimal Degrees
#  S3method dms_to_decdeg.degminsec()
#'
#' @rdname dms_to_decdeg
#' @export

dms_to_decdeg.degminsec <- function(object, ...) {
    check_dots_empty()
    validate_degminsec(object)
    with(object, deg + min / 60 + sec / 3600) |>
    decdeg()
}

# ========================================
#  Convert Degrees, Minutes and Seconds in a list to Decimal Degrees
#  S3method dms_to_decdeg.list()
#'
#' @rdname dms_to_decdeg
#' @export

dms_to_decdeg.list <- function(object, ...) {
    check_dots_empty()
    stopifnot(all(purrr::map_lgl(object, \(x) (inherits(x, "degminsec")))))
    lapply(object, dms_to_decdeg)
}
