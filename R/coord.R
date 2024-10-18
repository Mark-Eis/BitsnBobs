# BitsnBobs R Package
# Mark Eisler Oct 2024
# For general bits and bobs of code
#
# Requires R version 4.2.0 (2022-04-22) -- "Vigorous Calisthenics" or later
#
# coord.R

# __________________________________
# Remove once in package BitsnBobs
.up2 <- \(...) BitsnBobs:::.up2(...)

`%@%` <- \(...) rlang:::`%@%`(...)

check_dots_used <- \(...) rlang:::check_dots_used()

check_dots_empty <- \(...) rlang::: check_dots_empty()
# Ends - remove once in package BitsnBobs

# ____________________
# `"Coordpart"` class
# `"Coordpart"` class contains a list with one, two or three objects of
# classes `"degxdec"`, `"degxint"`, `"minxint"`, `"minxdec"`, `"secxdec"`

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
# Coordinate class
# Coord class contains a list with one, two or three values named
# "deg", "min", "sec"
#' @export

coord <- function(
    x,
    .degrtype = c("decdeg", "degmin", "degminsec"),
    .fmt = c("deg", "min", "sec"),
    .latorlon = c(NA, "lat", "lon")
) {
    .degrtype <- match.arg(.degrtype)
    .fmt <- match.arg(.fmt)
    .latorlon <- match.arg(.latorlon)    

    x <- fmtdeg(x, .degrtype, .fmt)

    rv <- lapply(x, \(y) {
        negative <- y < 0
        y <- abs(y)
        switch(.degrtype,
            decdeg = list(deg = coordpart(y, "degxdec")),
            degmin = list(
                deg = coordpart(as.integer(y), "degxint"),
                min = coordpart(round(.up2(y), 4), "minxdec")
            ),
            degminsec = list(
                deg = coordpart(as.integer(y), "degxint"),
                min = coordpart(as.integer(.up2(y)), "minxint"),
                sec = coordpart(round(.up2(.up2(y)), 2), "secxdec")
            ),
            stop("Invalid `.degrtype` value", call. = FALSE)
        ) |>
        new_coord(.degrtype, .latorlon, negative) |>
        validate_coord()
    })
    if (length(x) > 1) rv else rv[[1]]
}

new_coord <- function(x, degrtype, latorlon = NA, negative = FALSE) {
    structure(x, class = c("coord", degrtype), latorlon = latorlon, negative = negative)
}

validate_coord <- function(object) {

    if (!inherits(object, "coord"))
        stop(
            "`object` must be of class `\"coord\"`",
            call. = FALSE
        )

    if (sum_degminsec(object) > 180)
        stop(
            "`object` must not be greater than 180\u00B0",
            call. = FALSE
        )

    if (sum_minsec(object) >= 60)
        stop(
            "`object$min` must be less than 60\'",
            call. = FALSE
        )

    if (sum_sec(object) >= 60)
        stop(
            "`object$sec` must be less than 60\'",
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
            "`object` for latitude must not be greater than 90\u00B0",
            call. = FALSE
        )

    if (!is.logical(object %@% "negative"))
        stop(
            "Attribute `\"negative\"` must be of type `logical`",
            call. = FALSE
        )    

    object
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

#' @exportS3Method BitnBobs::sum_degminsec

sum_degminsec.decdeg <- function(object, ...) {
    check_dots_empty()
    with(object, deg)
}

#' @exportS3Method BitnBobs::sum_degminsec

sum_degminsec.degmin <- function(object, ...) {
    check_dots_empty()
    with(object, deg + min / 60)
}

#' @exportS3Method BitnBobs::sum_degminsec

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

#' @exportS3Method BitnBobs::sum_minsec

sum_minsec.decdeg <- function(object, ...) {
    check_dots_empty()
    0
}

#' @exportS3Method BitnBobs::sum_minsec

sum_minsec.degmin <- function(object, ...) {
    check_dots_empty()
    with(object, min)
}

#' @exportS3Method BitnBobs::sum_minsec

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

#' @exportS3Method BitnBobs::sum_sec

sum_sec.decdeg <- function(object, ...) {
    check_dots_empty()
    0
}

#' @exportS3Method BitnBobs::sum_sec

sum_sec.degmin <- function(object, ...) {
    check_dots_empty()
    0
}

#' @exportS3Method BitnBobs::sum_sec

sum_sec.degminsec <- function(object, ...) {
    check_dots_empty()
    with(object, sec)
}

# _______________________________________
# S3 print() method for `"Coord"` class
#' @export

print.coord <- function(x, ...) {
    check_dots_empty()
    if (all(class(x)[2] == "decdeg", x %@% "negative"))
        x$deg <- -x$deg
    lapply(x, format)
    if (class(x)[2] == "decdeg") {
        if (!is.na(x %@% "latorlon")) cat(" ", x %@% "latorlon", sep = "")
    } else
        cat(" ", .cmppnt(x %@% "latorlon", x %@% "negative"), sep = "")
    invisible(x)
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

as__degminsec.coord <- function(object, ...) {
    check_dots_empty()

    NextMethod() |>
    as.numeric() |>
    swapsign(object %@% "negative") |>
    coord("degminsec", .latorlon = object %@% "latorlon")
}

as__degminsec.decdeg <- function(object, ...) {
    check_dots_empty()
    with(object, deg %/% 1 + (deg %% 1 * 60) %/% 1 / 100 + (deg %% 1 * 60) %% 1 * 3 / 500)
}

as__degminsec.degmin <- function(object, ...) {
    check_dots_empty()
    with(object, deg + min %/% 1 / 100 + min %% 1 * 3 / 500)
}

as__degminsec.degminsec <- function(object, ...) {
    check_dots_empty()
    with(object, deg + min / 100 + sec / 1e4)
}

as__degminsec.numeric <- function(
    object,
    ...,
    .degrtype = c("decdeg", "degmin", "degminsec"),
    .fmt = c("deg", "min", "sec"),
    .as_numeric = FALSE
) {
    check_dots_empty()
    .degrtype <- match.arg(.degrtype)
    .fmt <- match.arg(.fmt)

    degconvert_numeric(object, as__degminsec, .degrtype, .fmt, .as_numeric)
}

# For consistency (no conflict with BitsnBobs)
#' @export

as__degmin <- function(object, ...) {
    UseMethod("as__degmin")
}

as__degmin.coord <- function(object, ...) {
    check_dots_empty()

    NextMethod() |>
    as.numeric() |>
    swapsign(object %@% "negative") |>
    coord("degmin", .latorlon = object %@% "latorlon")
}


as__degmin.decdeg <- function(object, ...) {
    check_dots_empty()
    with(object, deg %/% 1 + deg %% 1 * 3 / 5)
}

as__degmin.degmin <- function(object, ...) {
    check_dots_empty()
    with(object, deg + min / 100)
}

as__degmin.degminsec <- function(object, ...) {
    check_dots_empty()
    with(object, deg + (min + sec / 60) / 100)
}

as__degmin.numeric <- function(
    object,
    ...,
    .degrtype = c("decdeg", "degmin", "degminsec"),
    .fmt = c("deg", "min", "sec"),
    .as_numeric = FALSE
) {
    check_dots_empty()
    .degrtype <- match.arg(.degrtype)
    .fmt <- match.arg(.fmt)

    degconvert_numeric(object, as__degmin, .degrtype, .fmt, .as_numeric)
}

# To avoid conflict with BitsnBobs::as_decdeg()
#' @export

as__decdeg <- function(object, ...) {
    UseMethod("as__decdeg")
}

as__decdeg.coord <- function(object, ...) {
    check_dots_empty()

    sum_degminsec(object) |>
    as.numeric() |>
    swapsign(object %@% "negative") |>
    coord("decdeg", .latorlon = object %@% "latorlon")
}

as__decdeg.numeric <- function(
    object,
    ...,
    .degrtype = c("decdeg", "degmin", "degminsec"),
    .fmt = c("deg", "min", "sec"),
    .as_numeric = FALSE
) {
    check_dots_empty()
    .degrtype <- match.arg(.degrtype)
    .fmt <- match.arg(.fmt)

    degconvert_numeric(object, as__decdeg, .degrtype, .fmt, .as_numeric)
}

# ________________________________________________________________________________
# Powers as__degminsec.numeric(), as__degmins.numeric() and as__decdeg.numeric()
# Not exported
degconvert_numeric <- function(object, fun, .degrtype, .fmt, .as_numeric) {
    fun <- match.fun(fun)

    rv <- coord(object, .degrtype, .fmt)
    if (length(object) == 1)
        rv <- list(rv)
    rv <- lapply(rv, fun)
        
    if (.as_numeric) {
        vapply(rv, as.double, numeric(1))
    } else 
        if (length(rv) > 1) rv else rv[[1]]
}

# ___________________________________________________________________________________________________________
# Convert numeric decimal degrees, degrees and minutes, and degrees minutes and seconds to "canonical form"
# i.e. with decimal point after integer degrees
fmtdeg <- function(x, .degrtype = c("decdeg", "degmin", "degminsec"), .fmt = c("deg", "min", "sec")) {
    .degrtype <- match.arg(.degrtype)
    .fmt <- match.arg(.fmt)

    switch(.fmt,
        deg = x,
        min = switch(.degrtype,
            degmin =,
            degminsec = x / 1e2L,
            stop(".fmt \"min\" only meaningful with .degrtype of \"degmin\" or \"degminsec\"", call. = FALSE)
        ),
        sec = switch(.degrtype,
            "degminsec" = x / 1e4L,
            stop(".fmt \"sec\" only meaningful with .degrtype of \"degminsec\"", call. = FALSE)
        ),
        stop("Invalid `.fmt` value", call. = FALSE) 
    )
}

# _____________________________________________
# Vectorised conditional sign change function
swapsign <- function(x, negate) {
    stopifnot(length(x) == length(negate))
    ifelse(negate, -x, x)
}
