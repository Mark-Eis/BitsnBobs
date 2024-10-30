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
#' @title Geographic or GPS Coordinate
#'
#' @description
#' Geographic or GPS coordinate class
#'
#' @details
#' `coord()` creates a robust representation of a geographic or GPS cordinate based on the value of
#' `deg`, `min` and `sec` instatiated as an objects of class `"coord"`. Objects of `"coord"` class
#' contain a `list` with one, two or three `numeric` values named `"deg"`, `"min"`, `"sec"`, depending
#' on whether the cordinate in question is represented in decimal degrees, in (integer) degrees and
#' (decimal) minutes, or else in (integer) degrees, (integer) minutes, and (decimal) seconds.
#'
#' The value provided in argument `dec` should have a decimal point after the number of whole
#' degrees in the case of decimal degrees. Likewise the value provided in argument `min` should have
#' a decimal point, after the number of whole minutes in the case of degrees and minutes, and the
#' value provided in argument `sec` after the number of whole seconds in the case of degrees,
#' minutes and seconds. In all other cases, arguments `deg` and `min` must be of type `integer`.
#'
#' `"coord"` objects have `character` attribute `latorlon`, which may be `"lat"` for latitude,
#' `"lon"` for longitude or `NA`, and a `logical` attribute `"negative"`, which when `TRUE`
#' signifies a negative coordinate i.e., S or W rather than N or E.
#'
#' The total value in degrees, minutes and seconds may not be greater than `180˚`, while the
#' minutes and seconds components (if present) must be less than  `60˚`. If latitude is
#' represented, (i.e., `latorlon` attribute is `"lat"`), its  maximum absolute value is `90˚`.
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
#' @return An object of class `"coord"` instantiating a coordinate. See \emph{Details}.
#'
#' @export
#' @examples
#' ## Decimal degrees
#' coord()
#'
#' coord(51.507765)
#' coord(-51.507765)
#' coord(51.507765,,, "lat")
#' coord(-51.507765,,, "lat")
#' coord(51.507765,,, "lon")
#' coord(-51.507765,,, "lon")
#'
#' ## Degrees and (decimal) minutes
#' coord(51L, 30.4659)
#' coord(-51L, 30.4659)
#' coord(51L, 30.4659,, "lat")
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
#' coord(-51L, 30L, 27.95, "lon")

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
#' @title
#' Convert Coordinate or Numeric to Another Coordinate Format
#'
#' @description
#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
#' Convert the format of geographic or GPS coordinates represented by `"coord"` objects or `numeric`
#' values among decimal degrees, degrees and minutes, and degrees, minutes and seconds.
#'
#' @details
#' Converts between coordinates represented in decimal degrees ("decdeg"), integer degrees and
#' decimal minutes ("degmin"), and integer degrees, integer minutes, and decimal seconds
#' ("degminsec"). Works with individual [`"coord"`][coord] objects returned using the
#' [`coord()`][coord] function, or with vectors of simple numeric values.
#' See [`"coord"`][coord] for further details.
#'
#' @family coord
#'
#' @param object a `"coord"` object or a `numeric` vector.
#'
#' @param ... further arguments passed to or from other methods.
#'
#' @param .fmt `character` string indicating the format of `object`; must be one of `"decdeg"`
#'   (default), `"degmin"` or `"degminsec"`.
#'
#' @param .as_numeric logical, signifying whether to return a `"coord"` object or a `numeric` value;
#'   default `FALSE`, the former.
#'
#' @return
#' A [`"coord"`][coord] object or `numeric` value in the desired format.
#'
#' @export
#' @examples
#' ## `"coord"` objects in decimal degrees; in degrees and minutes;
#' ##   and in degrees, minutes, and seconds
#' (coord_dd <- coord(51.507765, "decdeg"))
#' (coord_dm <- coord(5130.4659, "degmin"))
#' (coord_dms <- coord(513027.95, "degminsec"))

as_coord <- function(object, ...) {
	UseMethod("as_coord")
}

# ========================================
#  Convert Coord to Coord of another format
#  S3method as_coord.coord()
#'
#' @rdname as_coord
#' @export

as_coord.coord <- function(object, ..., .fmt = c("decdeg", "degmin", "degminsec")) {
    check_dots_empty()
    .fmt <- match.arg(.fmt)
	
    if (inherits(object, .fmt))
        object
   	else {
   	    switch(.fmt,
   	       "decdeg" = coord(sum_degminsec(object), .latorlon = object %@% latorlon),
   	       "degmin" = coord(as.integer(object$deg), sum_minsec(object), .latorlon = object %@% latorlon),
   	       "degminsec" = coord(
   	           as.integer(object$deg),
   	           as.integer(sum_minsec(object) %/% 1),
   	           sum_minsec(object) %% 1,
   	           .latorlon = object %@% latorlon),
            stop("Invalid `.fmt` value", call. = FALSE)
   	    )
   	}
   	
}

# ========================================
#  Convert Numeric to Coord
#  S3method as_coord.numeric()
#'
#' @rdname as_coord
#' @export

as_coord.numeric <- function(
    object,
    ...,
    .fmt = c("decdeg", "degmin", "degminsec"), 
    .latorlon = c(NA, "lat", "lon"),
    .as_numeric = FALSE
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
    x$tmp <- with(x, (deg * 1e2 + min) * 1e2 + sec)
    NextMethod()
}

#' @exportS3Method base::as.double

as.double.degmin <- function(x, ...) {
    check_dots_empty()
    x$tmp <- with(x, deg * 1e2 + min)
    NextMethod()
}

#' @exportS3Method base::as.double

as.double.decdeg <- function(x, ...) {
    check_dots_empty()
    x$tmp <- with(x, deg)
    NextMethod()
}

#' @exportS3Method base::as.double

as.double.coord <- function(x, ...) {
    check_dots_empty()
    
    x$tmp |>
    as.numeric() |>
    unlist() |>
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

# ========================================
#' @title
#' Convert Coordinate Format
#'
#' @description
#' Convert the format of geographic or GPS coordinates to decimal degrees, to degrees and minutes
#' or to degrees, minutes and seconds.
#'
#' @details
#' Converts between coordinates represented in decimal degrees ("decdeg"), integer degrees and
#' decimal minutes ("degmin"), and integer degrees, integer minutes, and decimal seconds
#' ("degminsec"). Works with individual [`"coord"`][coord] objects returned using the
#' [`coord()`][coord] function, or with vectors of simple numeric values.
#' See [`"coord"`][coord] for further details.
#'
#' @family coord
#'
#' @param object a `"coord"` object or a `numeric` vector.
#'
#' @param ... further arguments passed to or from other methods.
#'
#' @param .fmt `character` string indicating the format of `object`; must be one of `"decdeg"`
#'   (default), `"degmin"` or `"degminsec"`.
#'
#' @param .as_numeric logical, signifying whether to return a `"coord"` object or a `numeric` value;
#'   default `FALSE`, the former.
#'
#' @return
#' A [`"coord"`][coord] object or `numeric` value in the desired format.
#'
#' @export
#' @examples
#' ## `"coord"` objects in decimal degrees; in degrees and minutes;
#' ##   and in degrees, minutes, and seconds
#' (coord_dd <- coord(51.507765, "decdeg"))
#' (coord_dm <- coord(5130.4659, "degmin"))
#' (coord_dms <- coord(513027.95, "degminsec"))
#'
#' ## as_degminsec
#' coord_dd |> as_degminsec()
#' coord_dm |> as_degminsec()
#'
#' ## as_degmin
#' coord_dd |> as_degmin()
#' coord_dms |> as_degmin()
#'
#' ## as_decdeg
#' coord_dm |> as_decdeg()
#' coord_dms |> as_decdeg()
#'
#' ## `"latnlon"` objects in decimal degrees; in degrees and minutes;
#' ##   and in degrees, minutes, and seconds
#' (latnlon_dd <- coord(c(51.507765, -0.127924), "decdeg", "both"))
#' (latnlon_dm <- coord(c(5130.4659, -7.6754), "degmin", "both"))
#' (latnlon_dms <- coord(c(513027.95, -740.53), "degminsec", "both"))
#'
#' ## as_degminsec
#' latnlon_dd |> as_degminsec()
#' latnlon_dm |> as_degminsec()
#'
#' ## as_degmin
#' latnlon_dd |> as_degmin()
#' latnlon_dms |> as_degmin()
#'
#' ## as_decdeg
#' latnlon_dm |> as_decdeg()
#' latnlon_dms |> as_decdeg()
#'
#' ## Numeric vectors in decimal degrees; in degrees and minutes;
#' ##   and in degrees, minutes, and seconds
#' (num_dd <- c(51.507765, 49.546210, 48.107232, 38.889494, 0.000000, -37.111740, -53.104781))
#' (num_dm <- c(5130.4659, 4932.7726, 4806.4339, 3853.3696, 0.0000, -3706.7044, -5306.2869))
#' (num_dms <- c(513027.95, 493246.36, 480626.04, 385322.18, 0.00, -370642.26, -530617.21))
#'
#' ## as_degminsec
#' num_dd |> as_degminsec(.fmt = "decdeg")
#' num_dm |> as_degminsec(.fmt = "degmin")
#'
#' num_dd |> as_degminsec(.fmt = "decdeg", .as_numeric = TRUE)
#' num_dm |> as_degminsec(.fmt = "degmin", .as_numeric = TRUE)
#'
#' ## as_degmin
#' num_dd |> as_degmin(.fmt = "decdeg")
#' num_dms |> as_degmin(.fmt = "degminsec")
#'
#' num_dd |> as_degmin(.fmt = "decdeg", .as_numeric = TRUE)
#' num_dms |> as_degmin(.fmt = "degminsec", .as_numeric = TRUE)
#'
#' ## as_decdeg
#' num_dm |> as_decdeg(.fmt = "degmin")
#' num_dms |> as_decdeg(.fmt = "degminsec")
#'
#' num_dm |> as_decdeg(.fmt = "degmin", .as_numeric = TRUE)
#' num_dms |> as_decdeg(.fmt = "degminsec", .as_numeric = TRUE)
#'
#' rm(coord_dd, coord_dm, coord_dms, num_dd, num_dm, num_dms)

# as_degminsec <- function(object, ...) {
    # UseMethod("as_degminsec")
# }

# #' @export

# as_degminsec.coord <- function(object, ...) {
    # check_dots_empty()

    # crossprod(
        # c(1e4, 1e2, 6e1),
        # c(sum_degminsec(object) %/% 1, (sum_minsec(object) * 60) %/% 1, (sum_minsec(object) * 60) %% 1)
    # ) |>
    # as.numeric() |>
    # swapsign(object %@% "negative") |>
    # coord("degminsec", .latorlon = object %@% "latorlon")
# }

# #' @exportS3Method BitsnBobs::as_degminsec

# as_degminsec.latnlon <- function(object, ...) {
    # lapply(object, as_degminsec) |>
    # structure(class = "latnlon") 
# }

# # ========================================
# #  Convert Coordinate to Degrees, Minutes and Seconds
# #  S3method as_degminsec.numeric()
# #'
# #' @rdname as_degminsec
# #' @export

# as_degminsec.numeric <- function(
    # object,
    # ...,
    # .fmt = c("decdeg", "degmin", "degminsec"),
    # .as_numeric = FALSE
# ) {
    # check_dots_empty()
    # .fmt <- match.arg(.fmt)

    # degconvert_numeric(object, as_degminsec, .fmt, .as_numeric)
# }

# # ========================================
# #  Convert Coordinate to Degrees and Minutes
# #  S3generic as_degmin()
# #'
# #' @rdname as_degminsec
# #' @export

# as_degmin <- function(object, ...) {
    # UseMethod("as_degmin")
# }

# #' @export

# as_degmin.coord <- function(object, ...) {
    # check_dots_empty()

    # # (sum_degminsec(object) %/% 1 * 1e2 + sum_minsec(object) * 60) |>
    # crossprod(
        # c(1e2, 6e1),
        # c(sum_degminsec(object) %/% 1, sum_minsec(object))
    # ) |>
    # as.numeric() |>
    # swapsign(object %@% "negative") |>
    # coord("degmin", .latorlon = object %@% "latorlon")
# }

# #' @exportS3Method BitsnBobs::as_degmin

# as_degmin.latnlon <- function(object, ...) {
    # lapply(object, as_degmin) |>
    # structure(class = "latnlon") 
# }

# # ========================================
# #  Convert Coordinate to Degrees and Minutes
# #  S3method as_degmin.numeric()
# #'
# #' @rdname as_degminsec
# #' @export

# as_degmin.numeric <- function(
    # object,
    # ...,
    # .fmt = c("decdeg", "degmin", "degminsec"),
    # .as_numeric = FALSE
# ) {
    # check_dots_empty()
    # .fmt <- match.arg(.fmt)

    # degconvert_numeric(object, as_degmin, .fmt, .as_numeric)
# }

# # ========================================
# #  Convert Coordinate to Decimal Degrees
# #  S3generic as_decdeg()
# #'
# #' @rdname as_degminsec
# #' @export

# as_decdeg <- function(object, ...) {
    # UseMethod("as_decdeg")
# }

# #' @export

# as_decdeg.coord <- function(object, ...) {
    # check_dots_empty()

    # sum_degminsec(object) |>
    # as.numeric() |>
    # swapsign(object %@% "negative") |>
    # coord("decdeg", .latorlon = object %@% "latorlon")
# }

# #' @exportS3Method BitsnBobs::as_decdeg

# as_decdeg.latnlon <- function(object, ...) {
    # lapply(object, as_decdeg) |>
    # structure(class = "latnlon") 
# }

# # ========================================
# #  Convert Numeric to Decimal Degrees
# #  S3method as_decdeg.numeric()
# #'
# #' @rdname as_degminsec
# #' @export

# as_decdeg.numeric <- function(
    # object,
    # ...,
    # .fmt = c("decdeg", "degmin", "degminsec"),
    # .as_numeric = FALSE
# ) {
    # check_dots_empty()
    # .fmt <- match.arg(.fmt)

    # degconvert_numeric(object, as_decdeg, .fmt, .as_numeric)
# }

# # ________________________________________________________________________________
# # Powers as_degminsec.numeric(), as_degmins.numeric() and as_decdeg.numeric()
# # Not exported

# degconvert_numeric <- function(object, fun, .fmt, .as_numeric) {
    # fun <- match.fun(fun)

    # rv <- coord(object, .fmt)
    # if (length(object) == 1)
        # rv <- list(rv)
    # rv <- lapply(rv, fun)
        
    # if (.as_numeric) {
        # vapply(rv, as.double, numeric(1))
    # } else 
        # if (length(rv) > 1) rv else rv[[1]]
# }
