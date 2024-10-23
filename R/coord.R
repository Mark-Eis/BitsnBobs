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
    fmtlst <- as.list(c(x = x, the$crdprtfmt[the$crdprtfmt$name == class(x)[2], 2:6]))
    cat(
        do.call(formatC, fmtlst[-6]),
        fmtlst$endchr,
        sep = ""
    )
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
#' @param .fmt `character` string indicating the format of `x`; must be one of
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
#' @examples
#' ## Decimal degrees (default)
#' coord(51.507765)
#' coord(-0.127924)
#' coord(51.507765,, "lat")
#' coord(-0.127924,, "lon")
#' coord(c(51.507765, -0.127924),, "both")
#' c(51.507765, 49.546210, 48.107232, 38.889494, 0.000000, -37.111740, -53.104781) |>
#'     coord(, "lat")
#' c(-0.127924, 18.398562, -122.778671, -77.035242, 0.000000, -12.28863, 73.517283) |>
#'     coord(, "lon")
#'
#' ## Degrees and minutes
#' coord(5130.4659, "degmin")
#' coord(-7.6754, "degmin")
#' coord(5130.4659, "degmin", "lat")
#' coord(-7.6754, "degmin", "lon")
#' coord(c(5130.4659, -7.6754), "degmin", "both")
#' c(5130.4659, 4932.7726, 4806.4339, 3853.3696, 0.0000, -3706.7044, -5306.2869) |>
#'     coord("degmin", "lat")
#' c(-7.6754, 1823.9137, -12246.7203, -7702.1145, 0.0000, -1217.3178, 7331.0370) |>
#'     coord("degmin", "lon")
#'
#' ## Degrees, minutes and seconds
#' coord(513027.95, "degminsec")
#' coord(-740.53, "degminsec")
#' coord(513027.95, "degminsec", "lat")
#' coord(-740.53, "degminsec", "lon")
#' coord(c(513027.95, -740.53), "degminsec", "both")
#' c(513027.95, 493246.36, 480626.04, 385322.18, 0.00, -370642.26, -530617.21) |> 
#'     coord("degminsec", "lat")
#' c(-740.53, 182354.82, -1224643.22, -770206.87, 0.00, -121719.07, 733102.22) |> 
#'     coord("degminsec", "lon")

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

sum_degminsec <- function(object, ...) {
    UseMethod("sum_degminsec")
}

#' @exportS3Method BitsnBobs::sum_degminsec

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
    with(object, deg) + sum_minsec(object) / 60 
}

# ______________________________________________
# Total minutes, including seconds, as decimal

sum_minsec <- function(object, ...) {
    UseMethod("sum_minsec")
}

#' @exportS3Method BitsnBobs::sum_minsec

sum_minsec.coord <- function(object, ...) {
    check_dots_empty()
    NextMethod() |>
    as.numeric()
}

#' @exportS3Method BitsnBobs::sum_minsec

sum_minsec.decdeg <- function(object, ...) {
    check_dots_empty()
    sum_degminsec(object) %% 1 * 60
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

sum_sec <- function(object, ...) {
    UseMethod("sum_sec")
}

#' @exportS3Method BitsnBobs::sum_sec

sum_sec.coord <- function(object, ...) {
    check_dots_empty()
    NextMethod() |>
    as.numeric()
}

#' @exportS3Method BitsnBobs::sum_sec

sum_sec.decdeg <- function(object, ...) {
    check_dots_empty()
    sum_minsec(object) %% 1 * 60
}

#' @exportS3Method BitsnBobs::sum_sec

sum_sec.degmin <- function(object, ...) {
    check_dots_empty()
    sum_minsec(object) %% 1 * 60
}

#' @exportS3Method BitsnBobs::sum_sec

sum_sec.degminsec <- function(object, ...) {
    check_dots_empty()
    with(object, sec)
}

# _____________________________
# as.numeric() / as.double methods()

#' @export

as.double.coord <- function(x, ...) {
    check_dots_empty()
    
    NextMethod() |>
    as.numeric() |>
    unlist() |>
    swapsign(x %@% "negative")
}

#' @exportS3Method base::as.double

as.double.degminsec <- function(x, ...) {
    check_dots_empty()
    with(x, (deg * 1e2 + min) * 1e2 + sec)
}

#' @exportS3Method base::as.double

as.double.degmin <- function(x, ...) {
    check_dots_empty()
    with(x, deg * 1e2 + min)
}

#' @exportS3Method base::as.double

as.double.decdeg <- function(x, ...) {
    check_dots_empty()
    with(x, deg)
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

# Named as__degminsec() to avoid conflict with BitsnBobs::as_degminsec()
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
#' ## as__degminsec
#' coord_dd |> as__degminsec()
#' coord_dm |> as__degminsec()
#'
#' ## as__degmin
#' coord_dd |> as__degmin()
#' coord_dms |> as__degmin()
#'
#' ## as__decdeg
#' coord_dm |> as__decdeg()
#' coord_dms |> as__decdeg()
#'
#' ## `"latnlon"` objects in decimal degrees; in degrees and minutes;
#' ##   and in degrees, minutes, and seconds
#' (latnlon_dd <- coord(c(51.507765, -0.127924), "decdeg", "both"))
#' (latnlon_dm <- coord(c(5130.4659, -7.6754), "degmin", "both"))
#' (latnlon_dms <- coord(c(513027.95, -740.53), "degminsec", "both"))
#'
#' ## as__degminsec
#' latnlon_dd |> as__degminsec()
#' latnlon_dm |> as__degminsec()
#'
#' ## as__degmin
#' latnlon_dd |> as__degmin()
#' latnlon_dms |> as__degmin()
#'
#' ## as__decdeg
#' latnlon_dm |> as__decdeg()
#' latnlon_dms |> as__decdeg()
#'
#' ## Numeric vectors in decimal degrees; in degrees and minutes;
#' ##   and in degrees, minutes, and seconds
#' (num_dd <- c(51.507765, 49.546210, 48.107232, 38.889494, 0.000000, -37.111740, -53.104781))
#' (num_dm <- c(5130.4659, 4932.7726, 4806.4339, 3853.3696, 0.0000, -3706.7044, -5306.2869))
#' (num_dms <- c(513027.95, 493246.36, 480626.04, 385322.18, 0.00, -370642.26, -530617.21))
#'
#' ## as__degminsec
#' num_dd |> as__degminsec(.fmt = "decdeg")
#' num_dm |> as__degminsec(.fmt = "degmin")
#'
#' num_dd |> as__degminsec(.fmt = "decdeg", .as_numeric = TRUE)
#' num_dm |> as__degminsec(.fmt = "degmin", .as_numeric = TRUE)
#'
#' ## as__degmin
#' num_dd |> as__degmin(.fmt = "decdeg")
#' num_dms |> as__degmin(.fmt = "degminsec")
#'
#' num_dd |> as__degmin(.fmt = "decdeg", .as_numeric = TRUE)
#' num_dms |> as__degmin(.fmt = "degminsec", .as_numeric = TRUE)
#'
#' ## as__decdeg
#' num_dm |> as__decdeg(.fmt = "degmin")
#' num_dms |> as__decdeg(.fmt = "degminsec")
#'
#' num_dm |> as__decdeg(.fmt = "degmin", .as_numeric = TRUE)
#' num_dms |> as__decdeg(.fmt = "degminsec", .as_numeric = TRUE)
#'
#' rm(coord_dd, coord_dm, coord_dms, num_dd, num_dm, num_dms)

as__degminsec <- function(object, ...) {
    UseMethod("as__degminsec")
}

#' @export

as__degminsec.coord <- function(object, ...) {
    check_dots_empty()

    crossprod(
        c(1e4, 1e2, 6e1),
        c(sum_degminsec(object) %/% 1, + sum_minsec(object) %/% 1, sum_minsec(object) %% 1)
    ) |>
    as.numeric() |>
    swapsign(object %@% "negative") |>
    coord("degminsec", .latorlon = object %@% "latorlon")
}

#' @exportS3Method BitsnBobs::as__degminsec

as__degminsec.latnlon <- function(object, ...) {
    lapply(object, as__degminsec) |>
    structure(class = "latnlon") 
}

# ========================================
#  Convert Coordinate to Degrees, Minutes and Seconds
#  S3method as__degminsec.numeric()
#'
#' @rdname as__degminsec
#' @export

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

# Named as__degmin() for consistency (no conflict with BitsnBobs)
# ========================================
#  Convert Coordinate to Degrees and Minutes
#  S3generic as__degmin()
#'
#' @rdname as__degminsec
#' @export

as__degmin <- function(object, ...) {
    UseMethod("as__degmin")
}

#' @export

as__degmin.coord <- function(object, ...) {
    check_dots_empty()

    (sum_degminsec(object) %/% 1 * 1e2 + sum_minsec(object)) |>
    as.numeric() |>
    swapsign(object %@% "negative") |>
    coord("degmin", .latorlon = object %@% "latorlon")
}

#' @exportS3Method BitsnBobs::as__degmin

as__degmin.latnlon <- function(object, ...) {
    lapply(object, as__degmin) |>
    structure(class = "latnlon") 
}

# ========================================
#  Convert Coordinate to Degrees and Minutes
#  S3method as__degmin.numeric()
#'
#' @rdname as__degminsec
#' @export

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

# Named as__decdeg() to avoid conflict with BitsnBobs::as_decdeg()
# ========================================
#  Convert Coordinate to Decimal Degrees
#  S3generic as__decdeg()
#'
#' @rdname as__degminsec
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

as__decdeg.latnlon <- function(object, ...) {
    lapply(object, as__decdeg) |>
    structure(class = "latnlon") 
}

# ========================================
#  Convert Numeric to Decimal Degrees
#  S3method as__decdeg.numeric()
#'
#' @rdname as__degminsec
#' @export

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
