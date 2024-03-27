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
#' `decdeg()` is a generic S3 function. The default method works with a numeric (`double`) vector representing one
#' or more coordinates of latitude or longitude in decimal degrees.
#'
#' @family degreeconvert
#'
#' @param object numeric, representing one or more coordinates of latitude or longitude in decimal degrees.
#'
#' @param \dots further arguments passed to or from other methods.
#'
#' @param x object to be printed.
#'
#' @return An object of class `"decdeg"`, or if `length(object) > 1`, a `list` of such objects, instantiating a
#'   coordinate of latitude or longitude in decimal degrees represented by a numeric of type `double` with maximum
#'   absolute value of 180 degrees.
#'
#' @keywords utilities
#'
#' @export
#' @examples
#' decdeg(49.54621)
#' decdeg(c(lat = 49.54621, lon = 18.2354822))
#' decdeg(c(lat = -37.11174, lon = -12.28863))

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
    ndvd <- \(x) new_decdeg(x) |> validate_decdeg()

    if (length(object) > 1)
        lapply(object, ndvd)
    else
       ndvd(object)  
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
    check_dots_used()
    cat(paste("\t", zapsmall(x), "decimal degrees\n"))
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
#' `degminsec()` is a generic S3 function. The default method works with a numeric (`double`) representing one or
#' more coordinates of latitude or longitude in degrees, minutes and seconds. The argument `.after` indicates the
#' position of the decimal point, which may be placed after the degrees, the minutes or the (whole) seconds, and by
#' default assumed to be placed after the degrees.
#'
#' @family degreeconvert
#'
#' @param object numeric, representing one or more coordinates of latitude or longitude in degrees, minutes and
#'   seconds.
#'
#' @param .after a character string indicating the position of the decimal point in `object`; must be one of
#'   "deg" (default), "min", or "sec". You can specify just the initial letter.
#'
#' @inheritParams decdeg
#'
#' @return An object of class `"degminsec"`, or if `length(object) > 1`, a `list` of such objects, representing a
#'   coordinate of latitude or longitude in degrees, minutes and seconds as a named list with components: -
#'
#' \item{deg}{degrees represented by an integer with maximum absolute value of 180.}
#'
#' \item{min}{minutes represented by a positive integer with value less than 60.}
#'
#' \item{sec}{seconds represented by a positive numeric with value less than 60.}
#'
#' An attribute `"negative"` indicates whether `object` was originally a negative number i.e. if `TRUE`, the
#'   value represents a west or south rather than north or east coordinate.
#'
#' @keywords utilities
#'
#' @export
#' @examples
#' degminsec(49.3246368)
#' degminsec(4932.46368, .after = "min")
#' degminsec(493246.368, .after = "sec")
#'
#' degminsec(c(lat = 49.3246368, lon = 18.2354822))
#' degminsec(c(lat = -370642.264, lon = -121719.068), .after = "sec")

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
    ndvd <- \(x) new_degminsec(x, .after) |> validate_degminsec()

    if (length(object) > 1)
        lapply(object, ndvd)
    else
       ndvd(object)  
}

# ========================================
#  Constructor
#  new_degminsec()
#
#  not exported

new_degminsec <- function(x, .after = c("deg", "min", "sec"), .latorlon = c(NA, "lat", "lon")) {
    .after <- match.arg(.after)
    .latorlon <- match.arg(.latorlon)
    ltz <- x < 0
    x <- switch(.after,
            deg = abs(x),
            min = abs(x) / 1e2L,
            sec = abs(x) / 1e4L
        )
    structure(
        list(
            deg = as.integer(x),
            min = as.integer(.up2(x)),
            sec = .up2(.up2(x))
        ),
        class = "degminsec",
        negative = ltz,
        .latorlon = .latorlon
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

    if (any(dms$deg < 0, dms$min < 0, dms$sec < 0))
      stop(
        "`dms$deg`, `dms$min` and `dms$sec` must all be not less than zero",
        call. = FALSE
      )

    if (dms$deg > 180)
      stop(
        "`dms$deg` must not be greater than 180",
        call. = FALSE
      )

    if (!dms$min < 60)
      stop(
        "`dms$min` must be less than 60",
        call. = FALSE
      )

    if (!dms$sec < 60)
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
    check_dots_used()
    cat(paste0("\t",.dmsstr(x), if (x %@% "negative") "(W/S)" else "(N/E)", "\n"))
    invisible(x)
} 

# ========================================
#  Degrees, Minutes and Seconds as String
#  .dmsstr()
#
#  not exported

.dmsstr <- function(dms) {
    stopifnot(inherits(dms, "degminsec"))
    with(dms, paste0(deg, "\u00B0", min, "\'", zapsmall(sec), "\""))
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
#' @param object a numeric or an object of class [`"degminsec"`][degminsec], representing a coordinate of latitude or
#'   longitude in degrees, minutes and seconds or a list of `"degminsec"` objects.
#'
#' @inheritParams degminsec
#'
#' @inherit decdeg return
#'
#' @keywords utilities
#'
#' @export
#' @examples
#' dms_to_decdeg(49.3246368)
#' dms_to_decdeg(4932.46368, .after = "min")
#' dms_to_decdeg(493246.368, .after = "sec")
#'
#' (coord <- degminsec(49.3246368))
#' dms_to_decdeg(coord)
#'
#' (coords <- degminsec(c(lat = 49.3246368, lon = 18.2354822)))
#' dms_to_decdeg(coords)
#'
#' (coords <- degminsec(c(lat = -37.0642264, lon = -12.1719068)))
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
    dd <- with(object, deg + min / 60 + sec / 3600)
    (if (object %@% "negative") -dd else dd) |>
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

# ========================================
#' Convert Decimal Degrees to Degrees, Minutes and Seconds
#'
#' @description
#' Convert decimal degrees to degrees, minutes and seconds. 
#'
#' @details
#' `decdeg_to_dms()` is an S3 function that works with individual coordinates, latitude and longitude
#' values paired in a named list (see examples) or with a longer list of coordinates.
#'
#' @family degreeconvert
#'
#' @param object a numeric or an object of class [`"decdeg"`][decdeg], representing a coordinate of latitude or
#'   longitude in decimal degrees or a list of `"decdeg"` objects.
#'
#' @inheritParams degminsec
#'
#' @inherit degminsec return
#'
#' @keywords utilities
#'
#' @export
#' @examples
#' decdeg_to_dms(49.54621)
#'
#' (coord <- decdeg(49.54621))
#' decdeg_to_dms(coord)
#'
#' (coords <- decdeg(c(lat = 49.54621, lon = 18.398562)))
#' decdeg_to_dms(coords)
#'
#' decdeg(c(lat = -37.11174, lon = -12.28863)) |>
#'   decdeg_to_dms()
#'
#' rm(coord, coords)

decdeg_to_dms <- function(object, ...) {
    UseMethod("decdeg_to_dms")
}

# ========================================
#  Convert Decimal Degrees to Degrees, Minutes and Seconds
#  S3method decdeg_to_dms.default()
#'
#' @rdname decdeg_to_dms
#' @export

decdeg_to_dms.default <- function(object, ...) {
    check_dots_empty()
    stopifnot(is.numeric(object))
    ltz <- object < 0
    object <- abs(object)
    structure(
        list(
            deg = as.integer(object %/% 1),
            min = as.integer(((object %% 1) * 60) %/% 1),
            sec = (((object %% 1) * 60) %% 1) * 60
        ),
        class = "degminsec",
        negative = ltz
    ) |>
    validate_degminsec()
}

# ========================================
#  Convert Decimal Degrees in a "decdeg" object to Degrees, Minutes and Seconds
#  S3method decdeg_to_dms.degminsec()
#'
#' @rdname decdeg_to_dms
#' @export

decdeg_to_dms.decdeg <- function(object, ...) {
    check_dots_empty()
    unclass(object) |> decdeg_to_dms()
}

# ========================================
#  Convert Decimal Degrees in a list to Degrees, Minutes and Seconds
#  S3method decdeg_to_dms.list()
#'
#' @rdname decdeg_to_dms
#' @export

decdeg_to_dms.list <- function(object, ...) {
    check_dots_empty()
    stopifnot(all(purrr::map_lgl(object, \(x) (inherits(x, "decdeg")))))
    lapply(object, decdeg_to_dms)
}


# ========================================
#  Get decimal multiplied by 100
#  .up2()
#
#  not exported

.up2 <- function(x) x %% 1 * 1e2L
