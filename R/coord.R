# BitsnBobs R Package
# Mark Eisler Oct 2024
# For general bits and bobs of code
#
# Requires R version 4.2.0 (2022-04-22) -- "Vigorous Calisthenics" or later
#
# coord.R

# _____________________
# `"Coordpart"` class
# Comprises a numeric value (either integer or double) with classes `"Coordpart"` and one of
# `"degxdec"`, `"degxint"`, `"minxint"`, `"minxdec"`, `"secxdec"`

coordpart <- function(x, typex = c("degxdec", "degxint", "minxdec", "minxint", "secxdec")) {
    typex <- match.arg(typex)
    new_coordpart(x, typex) |>
    validate_coordpart()
}

new_coordpart <- function(x, typex) {
    structure(x, class = c(typex, "coordpart"))
}

validate_coordpart <- function(object) {

    if (
        !all(
            inherits(object, "coordpart"),
            inherits(object, c("degxdec", "degxint", "minxdec", "minxint", "secxdec"))
        )
    )
        stop(
            "`object` must be of class `\"coordpart\"` and one of `\"degxdec\"`, `\"degxint\"`, `\"minxdec\"`,",
            "`\"minxdec\"`, `\"secxdec\"`",
            call. = FALSE
        )

    if (all(!is.integer(object), inherits(object, c("degxint", "minxint"))))
        stop(
            "Object of class `\"degxint\"` or `\"minxint\"` must be of type `integer`",
            call. = FALSE
        )

    if (object < 0)
        stop(
            "Object of class `\"coordpart\"` must not be negative",
            call. = FALSE
        )
    
    if (all(inherits(object, c("degxdec", "degxint")), unclass(object > 180))) {
        stop(
            "Object of class `\"degxdec\"` or `\"degxint\"` must not be > 180\u00B0",
            call. = FALSE
        )
    }

    if (all(inherits(object, c("minxdec", "minxint", "secxdec")), unclass(object > (60 - 1e-14)))) {
        stop(
            "Object of class `\"minxdec\"`, `\"minxint\"` or `\"secxdec\"` must be < 60 minutes or seconds",
            call. = FALSE
        )
    }

    object
}


# See R Packages (2e) 7.4 Internal state
# https://r-pkgs.org/data.html#sec-data-state
the <- new.env(parent = emptyenv())

# Formatting information for use by format.coordpart()
the$crdprtfmt <- data.frame(
    name = c("degxdec", "degxint", "minxdec", "minxint", "secxdec"),
    digits = c(6, 0, 4, 0, 2),
    width = c(11, 3, 7, 2, 5),
    format = rep("f", 5),
    flag = c(" ", "", rep("0", 3)),
    endchr = rep(c("\u00b0", "\'", "\""), c(2, 2, 1))
)

#' @exportS3Method base::format
format.coordpart <- function(x, ...) {
    fmtlst <- as.list(c(x = x, the$crdprtfmt[vapply(the$crdprtfmt$name, inherits, logical(1), x = x), 2:6]))
    cat(
        do.call(formatC, fmtlst[-6]),
        fmtlst$endchr,
        sep = ""
    )
}

new_decdeg <- function(d) {
    structure(
        list(deg = coordpart(d, "degxdec")),
        class = c("decdeg")
    )
}

new_degmin <- function(d, m) {
    structure(
        list(
             deg = coordpart(d, "degxint"),
             min = coordpart(round(m, 4), "minxdec")
        ),
        class = c("degmin")
    )
}

new_degminsec <- function(d, m, s) {
    structure(
        list(
            deg = coordpart(d, "degxint"),
            min = coordpart(m, "minxint"),
            sec = coordpart(round(s, 2), "secxdec")
        ),
        class = c("degminsec")
    )
}

new_coord <- function(object, latorlon = NA, negative = FALSE) {
    class(object) <- c(class(object), "coord")
    attr(object, "latorlon") <- latorlon
    attr(object, "negative") <- negative
    object
}

# ____________________
#' @title Geographic or GPS Coordinate Class
#'
#' @description
#' `coord()` creates a robust representation of a geographic or GPS cordinate based on the value of
#' `deg`, `min` and `sec`, instatiated as an object of class `"coord"`.
#'
#' `as_coord()` converts the format of geographic or GPS coordinates among decimal degrees, degrees
#' and minutes, and degrees, minutes and seconds. It also creates `"coord"` objects directly from
#' single numeric values in one of these three formats.
#'
#' @details
#' The value provided in argument `deg` should have a decimal point after the number of whole
#' degrees in the case of decimal degrees. Likewise, the value provided in argument `min` should
#' have a decimal point after the number of whole minutes in the case of degrees and minutes, and
#' argument `deg` should be of type `integer`. In the case of degrees, minutes and seconds,
#' arguments `deg` and `min` must be of type `integer` and argument `sec` should have a decimal
#' point after the number of whole seconds.
#'
#' The total value in degrees, minutes and seconds may not be greater than `180˚`, while the
#' minutes and seconds components (if present) must be less than  `60˚`. If latitude is
#' represented (i.e., `latorlon` attribute is `"lat"`), its  maximum absolute value is `90˚`.
#'
#' `as_coord()` has S3 methods for both `"coord"` objects and `numeric` values. Numeric values
#' should have a decimal point after the number of whole degrees in the case of decimal degrees,
#' after the number of whole minutes in the case of degrees and minutes, and after the number of
#' whole seconds in the case of degrees, minutes and seconds.
#'
#' There is also an S3 method for [`as.numeric()`][base::as.numeric] for objects of class
#' `"coord"`, returning numeric values as described above for `as_coord()`.
#'
#' @family coord
#'
#' @param deg `numeric`, representing the number of degrees. Must be of type `integer` if `min` or
#'   `sec` are provided, otherwise type `double`; default `0`.
#'
#' @param min `numeric`, representing the number of minutes. If `sec` provided, must be of type
#'   `integer`, otherwise `double`; default `NULL`.
#'
#' @param sec `double`, representing the number of seconds; default `NULL`.
#'
#' @param .latorlon a `character` string, either `"lat"` or `"lon"` indicating whether the
#'   coordinate represented is of latitude or longitude, or `NA` (the default).
#' 
#' @param object a `"coord"` object or a `numeric` vector to be converted to another format.
#'
#' @param ... further arguments passed to or from other methods.
#'
#' @param .fmt `character` string indicating the desired format; must be one of `"decdeg"`
#'   (default), `"degmin"` or `"degminsec"`.
#'
#' @return
#' An object of class `"coord"` instantiating a coordinate. Objects of `"coord"` class contain a
#' `list` with one, two or three `numeric` values named `"deg"`, `"min"`, `"sec"`, depending on
#' whether the cordinate in question is represented in decimal degrees, in (integer) degrees and
#' (decimal) minutes, or else in (integer) degrees, (integer) minutes, and (decimal) seconds.
#'
#' `"coord"` objects have `character` attribute `latorlon`, which may be `"lat"` for latitude,
#' `"lon"` for longitude or `NA`, and a `logical` attribute `"negative"`, which when `TRUE`
#' signifies a negative coordinate i.e., S or W, rather than N or E.
#'
#'
#' @export
#' @examples
#' ## Decimal degrees
#' coord()
#'
#' (cdd <- coord(51.507765))
#' coord(-51.507765)
#' coord(51.507765,,, "lat")
#' coord(-51.507765,,, "lat")
#' coord(51.507765,,, "lon")
#' coord(-51.507765,,, "lon")
#'
#' ## Degrees and (decimal) minutes
#' coord(51L, 30.4659)
#' coord(-51L, 30.4659)
#' (cdm <- coord(51L, 30.4659,, "lat"))
#' coord(-51L, 30.4659,, "lat")
#' coord(51L, 30.4659,, "lon")
#' coord(-51L, 30.4659,, "lon")
#'
#' ## Degrees, minutes and (decimal) seconds
#' coord(51L, 30L, 27.95)
#' coord(-51L, 30L, 27.95)
#' coord(51L, 30L, 27.95, "lat")
#' coord(-51L, 30L, 27.95, "lat")
#' coord(51L, 30L, 27.95, "lon")
#' (cdms <- coord(-51L, 30L, 27.95, "lon"))
#'
#' ## Convert formats
#'
#' ## To decimal degrees
#' cdd |> as_coord(.fmt = "decdeg")
#' cdm |> as_coord(.fmt = "decdeg")
#' cdms |> as_coord(.fmt = "decdeg")
#'
#' ## To degrees and minutes
#' cdd |> as_coord(.fmt = "degmin")
#' cdm |> as_coord(.fmt = "degmin")
#' cdms |> as_coord(.fmt = "degmin")
#'
#' ## To degrees, minutes and seconds
#' cdd |> as_coord(.fmt = "degminsec")
#' cdm |> as_coord(.fmt = "degminsec")
#' cdms |> as_coord(.fmt = "degminsec")
#'
#' ## Convert to numeric
#' \dontshow{options("digits" = 8)}
#' ## Decimal degrees
#' cdd |> as.numeric()
#' ## Degrees and minutes
#' cdm |> as.numeric()
#' ## Degrees, minutes and seconds
#' cdms |> as.numeric()
#' \dontshow{options("digits" = 7)}
#' rm(cdd, cdm, cdms)


coord <- function(deg = 0, min = NULL, sec = NULL, .latorlon = c(NA, "lat", "lon")) {
    .latorlon <- match.arg(.latorlon)

    negative <- deg < 0
    deg <- abs(deg)
    if(any(min < 0, sec < 0))
        stop("Minutes and seconds may not be negative", call. = FALSE)

    {
        if (is.null(sec)) {
            if(is.null(min)) {
                new_decdeg(deg)
            } else {
                new_degmin(deg, min)
            }
        } else {
            if(is.null(min)) {
                stop("if \"min\" is NULL, \"sec\" must also be NULL", call. = FALSE)
            } else {
                new_degminsec(deg, min, sec)
            }
        }
    } |>
    new_coord(.latorlon, negative) |>
    validate_coord()
}

# ========================================
#  Convert Coordinate or Numeric to Another Coordinate Format
#  S3generic as_coord()
#
#' @rdname coord
#' @export

as_coord <- function(object, ...) {
	UseMethod("as_coord")
}


# ========================================
#  Convert Coordinate to Another Format
#  S3method as_coord.cord()
#'
#' @rdname coord
#' @export

as_coord.coord <- function(object, ..., .fmt = c("decdeg", "degmin", "degminsec")) {
    check_dots_empty()
    .fmt <- match.arg(.fmt)
	
    if (inherits(object, .fmt))
        object
   	else {
   	    robj <- switch(.fmt,
   	       "decdeg" = coord(sum_degminsec(object)),
   	       "degmin" = coord(as.integer(object$deg), sum_minsec(object)),
   	       "degminsec" = coord(
   	           as.integer(object$deg),
   	           as.integer(sum_minsec(object) %/% 1),
   	           sum_sec(object)),
            stop("Invalid `.fmt` value", call. = FALSE)
   	    )
        attr(robj, "latorlon") <- object %@% "latorlon"
        attr(robj, "negative") <- object %@% "negative"
        robj
   	}
}

# ========================================
#  Convert Numeric to Coordinate Format
#  S3method as_coord.numeric()
#'
#' @rdname coord
#' @export

as_coord.numeric <- function(
    object,
    ...,
    .fmt = c("decdeg", "degmin", "degminsec"), 
    .latorlon = c(NA, "lat", "lon")
) {
    check_dots_empty()
    .fmt <- match.arg(.fmt)
    .latorlon <- match.arg(.latorlon)	

	# makes coord from a numeric
	# .fmt show what's wanted
    object
}


validate_coord <- function(object) {

    if (!inherits(object, "coord"))
        stop(
            "`object` must be of class `\"coord\"`",
            call. = FALSE
        )

    if (sum_degminsec(object) > 180)
        stop(
            "`coord` must not be greater than 180\u00B0",
            call. = FALSE
        )

    if (sum_minsec(object) >= 60)
        stop(
            "`coord$min` must be less than 60\'",
            call. = FALSE
        )

    if (sum_sec(object) >= 60)
        stop(
            "`coord$sec` must be less than 60\'",
            call. = FALSE
        )

    if (!object %@% "latorlon" %in% c(NA, "lat", "lon"))
        stop(
            "Attribute `\"latorlon\"` must be one of `NA`, `\"lat\"`, `\"lon\"`",
            call. = FALSE
        )    

    if (all(object %@% "latorlon" %in% "lat", sum_degminsec(object) > 90))
        stop(
            "Latitude must not be greater than 90\u00B0",
            call. = FALSE
        )

    if (!is.logical(object %@% "negative"))
        stop(
            "Attribute `\"negative\"` must be of type `logical`",
            call. = FALSE
        )    

    object
}

# _______________________________________
# S3 format() method for `"coord"` class
#' @exportS3Method base::format
format.coord <- function(x, ...) {
    check_dots_empty()
    if (all(inherits(x, "decdeg"), x %@% "negative"))
        x$deg <- -x$deg
    lapply(x, format)
    if (inherits(x, "decdeg")) {
        if (!is.na(x %@% "latorlon")) cat("", x %@% "latorlon")
    } else
        cat("", .cmppnt(x %@% "latorlon", x %@% "negative"))
}

# _______________________________________
# S3 print() method for `"coord"` class
#' @export

print.coord <- function(x, ...) {
    check_dots_empty()
    format(x)
    invisible(x)
}

# _______________________________________
# S3 format() method for `"latnlon"` class
#' @exportS3Method base::format
format.latnlon <- function(x, ...) {
    check_dots_empty()
    format(x[[1]])
    cat("  ")
    format(x[[2]])
}

# _______________________________________
# S3 print() method for `"latnlon"` class
#' @export

print.latnlon <- function(x, ...) {
    check_dots_empty()
    format(x)
    invisible(x)
}

# __________________________________________________________________
# Total degrees, including minutes and seconds, as decimal degrees

sum_degminsec <- function(object, ...) {
    UseMethod("sum_degminsec")
}

#' @exportS3Method BitsnBobs::sum_degminsec

sum_degminsec.decdeg <- function(object, ...) {
    check_dots_empty()
    with(object, deg) |>
    as.numeric()
}

#' @exportS3Method BitsnBobs::sum_degminsec

sum_degminsec.degmin <- function(object, ...) {
    check_dots_empty()
    with(object, deg + sum_minsec(object) / 60) |>
    as.numeric()
}

#' @exportS3Method BitsnBobs::sum_degminsec

sum_degminsec.degminsec <- function(object, ...) {
    check_dots_empty()
    with(object, deg + sum_minsec(object) / 60) |>
    as.numeric() 
}

# ______________________________________________________
# Total minutes, including seconds, as decimal minutes

sum_minsec <- function(object, ...) {
    UseMethod("sum_minsec")
}

#' @exportS3Method BitsnBobs::sum_minsec

sum_minsec.decdeg <- function(object, ...) {
    check_dots_empty()
    with(object, deg %% 1 * 60) |>
    sum_minsec_polish()
}

#' @exportS3Method BitsnBobs::sum_minsec

sum_minsec.degmin <- function(object, ...) {
    check_dots_empty()
    with(object, min) |>
    sum_minsec_polish()
}

#' @exportS3Method BitsnBobs::sum_minsec

sum_minsec.degminsec <- function(object, ...) {
    check_dots_empty()
    with(object, min + sum_sec(object) / 60) |>
    sum_minsec_polish()
}

sum_minsec_polish <- function(x)
    round(as.numeric(x), 10)

# _____________________________________
# Seconds, if any, as decimal seconds

sum_sec <- function(object, ...) {
    UseMethod("sum_sec")
}

#' @exportS3Method BitsnBobs::sum_sec

sum_sec.decdeg <- function(object, ...) {
    check_dots_empty()
    sum_minsec(object) %% 1 * 60    ## Prevents rounding up error!
}

#' @exportS3Method BitsnBobs::sum_sec

sum_sec.degmin <- function(object, ...) {
    check_dots_empty()
    sum_minsec(object) %% 1 * 60    ## Prevents rounding up error!
}

#' @exportS3Method BitsnBobs::sum_sec

sum_sec.degminsec <- function(object, ...) {
    check_dots_empty()
    with(object, sec) |>
    as.numeric()
}

# _____________________________
# as.numeric() / as.double methods()

#' @exportS3Method base::as.double

as.double.degminsec <- function(x, ...) {
    check_dots_empty()
    x$num <- as.numeric(with(x, (deg * 1e2 + min) * 1e2 + sec))
    NextMethod()
}

#' @exportS3Method base::as.double

as.double.degmin <- function(x, ...) {
    check_dots_empty()
    x$num <- as.numeric(with(x, deg * 1e2 + min))
    NextMethod()
}

#' @exportS3Method base::as.double

as.double.decdeg <- function(x, ...) {
    check_dots_empty()
    x$num <- as.numeric(with(x, deg))
    NextMethod()
}

#' @exportS3Method base::as.double

as.double.coord <- function(x, ...) {
    check_dots_empty()
    
    x$num |>
    swapsign(x %@% "negative")
}

# ========================================
.cmppnt <- function(latorlon, negative) {
    if (is.na(latorlon))
        if (negative) "(W/S)" else "(N/E)"
    else
        matrix(
            c("N", "E", "S", "W"),
            nrow = 2,
            dimnames = list(c("lat", "lon"))
        )[latorlon, as.integer(negative) + 1] |>
        unname()
}
