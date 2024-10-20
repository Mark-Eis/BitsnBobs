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
    structure(x, class = c("coordpart", typex))
}

validate_coordpart <- function(object) {

    if (!inherits(object, "coordpart"))
        stop(
            "`object` must be of class `\"coordpart\"`",
            call. = FALSE
        )

    if (!class(object)[2] %in% c("degxdec", "degxint", "minxdec", "minxint", "secxdec"))
        stop(
            "`class(object)[2]` must be one of `\"degxdec\"`, `\"degxint\"`, `\"minxdec\"`,",
            "`\"minxdec\"`, `\"secxdec\"`",
            call. = FALSE
        )

    if (all(!is.integer(object), class(object)[2] %in% c("degxint", "minxint")))
        stop(
            "Object of class `\"degxint\"` or `\"minxint\"` must be of type `integer`",
            call. = FALSE
        )

    if (object < 0)
        stop(
            "Object of class `\"coordpart\"` must not have value less than zero",
            call. = FALSE
        )
    
    if (unclass(object) > switch(
            class(object)[2],
            "degxdec" =,
            "degxint" = 180, 
            "minxdec" =,
            "minxint" =,
            "secxdec" = 60 - 1e-14
        )) {
        stop(
            "Values must be <= 180\u00B0 and < 60 for minutes and seconds",
            call. = FALSE
        )
    }
    object
}

#' @exportS3Method base::format

format.degxdec <- function(x, ...) {
    check_dots_empty()
    c(formatC(x, digits = 6, width = 11, format = "f", flag = " "), "\u00B0")
}

#' @exportS3Method base::format

format.degxint <- function(x, ...) {
    check_dots_empty()
    c(formatC(x, digits = 0, width = 3, format = "f"), "\u00B0")
}

#' @exportS3Method base::format

format.minxdec <- function(x, ...) {
    check_dots_empty()
    c(formatC(x, digits = 4, width = 7, format = "f", flag = "0"), "\'")
}

#' @exportS3Method base::format

format.minxint <- function(x, ...) {
    check_dots_empty()
    c(formatC(x, digits = 0, width = 2, format = "f", flag = "0"), "\'")
}

#' @exportS3Method base::format

format.secxdec <- function(x, ...) {
    check_dots_empty()
    c(formatC(x, digits = 2, width = 5, format = "f", flag = "0"), "\"")
}

#' @exportS3Method base::format

format.coordpart <- function(x, ...) {
    check_dots_empty()
    cat(NextMethod(), sep = "")
}


# ____________________
#' @title Geographic or GPS Coordinate
#'
#' @description
#' Geographic or GPS coordinate class
#'
#' @details
#' `coord()` creates a robust representation of a geographic or GPS cordinate based on the value of
#' `x` or, if `length(x) > 1`, a number of such coordinates instatiated as objects of class
#' `"coord"`. Objects of `"coord"` class contain a `list` with one, two or three `numeric` values
#' named `"deg"`, `"min"`, `"sec"`, depending on whether the cordinate in question is represented
#' in decimal degrees, in (integer) degrees and (decimal) minutes, or else in (integer) degrees,
#' (integer) minutes, and (decimal) seconds.
#'
#' The value provided in argument `x` should have a decimal point after the integer number
#' of degrees in the case of decimal degrees, after the integer number of minutes in the case of
#' degrees and minutes, and after the integer number of seconds in the case of degrees, minutes and
#' seconds
#'
#' `"coord"` objects have `character` attribute `latorlon`, which may be `"lat"` for latitude,
#' `"lon"` for longitude or `NA`, and a `logical` attribute `"negative"`, which when `TRUE`
#' signifies a negative coordinate i.e., S or W rather than N or E.
#'
#' If `length(x) > 1`, a list of `"coord"` objects is returned, all of which will have the same
#' `latorlon` attribute (i.e., either all `'lat"` or all `"lon"`). The exception is the case in
#' which argument `.latorlon` is `both` and `length(x) = 2`, when a list of two `"coord"` objects
#' is returned, having `latorlon` attributes one each of `"lat"` and `"lon"`; the list itself is an
#' object of class `"latnlon"`.
#'
#' The total value in degrees, minutes and seconds may not be greater than `180˚`, while the
#' minutes and seconds components (if present) must be less than  `60˚`. If latitude is
#' represented, (i.e., `latorlon` attribute is `"lat"`), its  maximum absolute value is `90˚`.
#'
#' @family coord
#'
#' @param x `numeric`, representing one or more coordinates.
#'
#' @param .fmt `character` string indicating the format of argument `x`; must be one of
#'   `"decdeg"` (default), `"degmin"` or `"degminsec"`.
#'
#' @param .latorlon a `character` string, either `"lat"` or `"lon"` indicating whether the
#'   coordinate(s) represented are of latitude or longitude or `"both"` indicating a pair of
#'   of latitude and longitude coordinates; otherwise it must be `NA` (the default).
#'
#' @return An object of class `"coord"` or if `length(x) > 1`, a list of such objects, each
#'   instantiating a coordinate. See \emph{Details}.
#'
#' @export

coord <- function(
    x,
    .fmt = c("decdeg", "degmin", "degminsec"),
    .latorlon = c(NA, "lat", "lon", "both")
) {
    .fmt <- match.arg(.fmt)
    .latorlon <- match.arg(.latorlon)
    if (all(!is.na(.latorlon), .latorlon == "both", length(x) != 2))
        stop("`x` not of length 2 [`.latorlon` = \"both\"]", call. = FALSE)

    rv <- lapply(x, \(y) {
        negative <- y < 0
        y <- abs(y)
        switch(.fmt,
            decdeg = list(deg = coordpart(y, "degxdec")),
            degmin = list(
                deg = coordpart(as.integer(y %/% 1e2), "degxint"),
                min = coordpart(round(y %% 1e2, 4), "minxdec")
            ),
            degminsec = list(
                deg = coordpart(as.integer(y %/% 1e4), "degxint"),
                min = coordpart(as.integer((y %% 1e4) %/% 1e2), "minxint"),
                sec = coordpart(round((y %% 1e4) %% 1e2, 2), "secxdec")
            ),
            stop("Invalid `.fmt` value", call. = FALSE)
        ) |>
        new_coord(.fmt, .latorlon, negative)
    })

    if (all(!is.na(.latorlon), .latorlon == "both", length(x) == 2)) {
        rv[[1]] %@% "latorlon" <- "lat"
        rv[[2]] %@% "latorlon" <- "lon"
        lapply(rv, validate_coord) |>
        structure(class = "latnlon")
    } else {
        rv <- lapply(rv, validate_coord)
        if (length(rv) > 1) rv else rv[[1]]
    }
}

new_coord <- function(x, fmt, latorlon = NA, negative = FALSE) {
    structure(x, class = c("coord", fmt), latorlon = latorlon, negative = negative)
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

    if (with(object,
            all(object %@% "latorlon" %in% "lat",
                switch(class(object)[2],
                    decdeg = deg,
                    degmin = deg + min / 60,
                    degminsec = deg + min / 60 + sec / 3600,
                    stop("Invalid `Coord` subclass: ", class(object)[2], call. = FALSE)
                ) > 90
            )
        ))
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
    if (all(class(x)[2] == "decdeg", x %@% "negative"))
        x$deg <- -x$deg
    lapply(x, format)
    if (class(x)[2] == "decdeg") {
        if (!is.na(x %@% "latorlon")) cat(" ", x %@% "latorlon", sep = "")
    } else
        cat(" ", .cmppnt(x %@% "latorlon", x %@% "negative"), sep = "")
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

# __________________________________________________________
# Total degrees, including minutes and seconds, as decimal
#' @export

sum_degminsec <- function(object, ...) {
    UseMethod("sum_degminsec")
}

#' @export

sum_degminsec.coord <- function(object, ...) {
    check_dots_empty()
    NextMethod() |>
    as.numeric()
}

#' @exportS3Method BitsnBobs::sum_degminsec

sum_degminsec.decdeg <- function(object, ...) {
    check_dots_empty()
    with(object, deg)
}

#' @exportS3Method BitsnBobs::sum_degminsec

sum_degminsec.degmin <- function(object, ...) {
    check_dots_empty()
    with(object, deg + min / 60)
}

#' @exportS3Method BitsnBobs::sum_degminsec

sum_degminsec.degminsec <- function(object, ...) {
    check_dots_empty()
    with(object, deg + min / 60 + sec / 3600)
}

# ______________________________________________
# Total minutes, including seconds, as decimal
#' @export

sum_minsec <- function(object, ...) {
    UseMethod("sum_minsec")
}

#' @export

sum_minsec.coord <- function(object, ...) {
    check_dots_empty()
    NextMethod() |>
    as.numeric()
}

#' @exportS3Method BitsnBobs::sum_minsec

sum_minsec.decdeg <- function(object, ...) {
    check_dots_empty()
    0
}

#' @exportS3Method BitsnBobs::sum_minsec

sum_minsec.degmin <- function(object, ...) {
    check_dots_empty()
    with(object, min)
}

#' @exportS3Method BitsnBobs::sum_minsec

sum_minsec.degminsec <- function(object, ...) {
    check_dots_empty()
    with(object, min + sec / 60)
}

# _____________________________
# Seconds, if any, as decimal
#' @export

sum_sec <- function(object, ...) {
    UseMethod("sum_sec")
}

#' @export

sum_sec.coord <- function(object, ...) {
    check_dots_empty()
    NextMethod() |>
    as.numeric()
}

#' @exportS3Method BitsnBobs::sum_sec

sum_sec.decdeg <- function(object, ...) {
    check_dots_empty()
    0
}

#' @exportS3Method BitsnBobs::sum_sec

sum_sec.degmin <- function(object, ...) {
    check_dots_empty()
    0
}

#' @exportS3Method BitsnBobs::sum_sec

sum_sec.degminsec <- function(object, ...) {
    check_dots_empty()
    with(object, sec)
}

#' @export

as.double.coord <- function(object, ...) {
    check_dots_empty()
    
    NextMethod() |>
    as.numeric() |>
    unlist() |>
    swapsign(object %@% "negative")
}

#' @exportS3Method base::as.double

as.double.degminsec <- function(object, ...) {
    check_dots_empty()
    with(object, deg + min / 100 + sec / 1e4)
}

#' @exportS3Method base::as.double

as.double.degmin <- function(object, ...) {
    check_dots_empty()
    with(object, deg + min / 100)
}

#' @exportS3Method base::as.double

as.double.decdeg <- function(object, ...) {
    check_dots_empty()
    with(object, deg)
}

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

# To avoid conflict with BitsnBobs::as_degminsec()
#' @export

as__degminsec <- function(object, ...) {
    UseMethod("as__degminsec")
}

#' @export

as__degminsec.coord <- function(object, ...) {
    check_dots_empty()

    NextMethod() |>
    as.numeric() |>
    swapsign(object %@% "negative") |>
    coord("degminsec", .latorlon = object %@% "latorlon")
}

#' @exportS3Method BitsnBobs::as__degminsec

as__degminsec.decdeg <- function(object, ...) {
    check_dots_empty()
    with(object, (deg %/% 1 * 1e2 + (deg %% 1 * 60) %/% 1) * 1e2 + (deg %% 1 * 60) %% 1 * 60)
}

#' @exportS3Method BitsnBobs::as__degminsec

as__degminsec.degmin <- function(object, ...) {
    check_dots_empty()
    with(object, (deg * 1e2 + min %/% 1) * 100 + min %% 1 * 60)
}

#' @exportS3Method BitsnBobs::as__degminsec

as__degminsec.degminsec <- function(object, ...) {
    check_dots_empty()
    with(object, (deg * 1e2 + min) * 1e2 + sec)
}

#' @exportS3Method BitsnBobs::as__degminsec

as__degminsec.numeric <- function(
    object,
    ...,
    .fmt = c("decdeg", "degmin", "degminsec"),
    .as_numeric = FALSE
) {
    check_dots_empty()
    .fmt <- match.arg(.fmt)

    degconvert_numeric(object, as__degminsec, .fmt, .as_numeric)
}

# For consistency (no conflict with BitsnBobs)
#' @export

as__degmin <- function(object, ...) {
    UseMethod("as__degmin")
}

#' @export

as__degmin.coord <- function(object, ...) {
    check_dots_empty()

    NextMethod() |>
    as.numeric() |>
    swapsign(object %@% "negative") |>
    coord("degmin", .latorlon = object %@% "latorlon")
}

#' @exportS3Method BitsnBobs::as__degmin

as__degmin.decdeg <- function(object, ...) {
    check_dots_empty()
    with(object, deg %/% 1 * 1e2 + deg %% 1 * 60)
}

#' @exportS3Method BitsnBobs::as__degmin

as__degmin.degmin <- function(object, ...) {
    check_dots_empty()
    with(object, deg * 1e2 + min)
}

#' @exportS3Method BitsnBobs::as__degmin

as__degmin.degminsec <- function(object, ...) {
    check_dots_empty()
    with(object, deg * 1e2 + min + sec / 60)
}

#' @exportS3Method BitsnBobs::as__degmin

as__degmin.numeric <- function(
    object,
    ...,
    .fmt = c("decdeg", "degmin", "degminsec"),
    .as_numeric = FALSE
) {
    check_dots_empty()
    .fmt <- match.arg(.fmt)

    degconvert_numeric(object, as__degmin, .fmt, .as_numeric)
}

# To avoid conflict with BitsnBobs::as_decdeg()
#' @export

as__decdeg <- function(object, ...) {
    UseMethod("as__decdeg")
}

#' @export

as__decdeg.coord <- function(object, ...) {
    check_dots_empty()

    sum_degminsec(object) |>
    as.numeric() |>
    swapsign(object %@% "negative") |>
    coord("decdeg", .latorlon = object %@% "latorlon")
}

#' @exportS3Method BitsnBobs::as__decdeg

as__decdeg.numeric <- function(
    object,
    ...,
    .fmt = c("decdeg", "degmin", "degminsec"),
    .as_numeric = FALSE
) {
    check_dots_empty()
    .fmt <- match.arg(.fmt)

    degconvert_numeric(object, as__decdeg, .fmt, .as_numeric)
}

# ________________________________________________________________________________
# Powers as__degminsec.numeric(), as__degmins.numeric() and as__decdeg.numeric()
# Not exported

degconvert_numeric <- function(object, fun, .fmt, .as_numeric) {
    fun <- match.fun(fun)

    rv <- coord(object, .fmt)
    if (length(object) == 1)
        rv <- list(rv)
    rv <- lapply(rv, fun)
        
    if (.as_numeric) {
        vapply(rv, as.double, numeric(1))
    } else 
        if (length(rv) > 1) rv else rv[[1]]
}
