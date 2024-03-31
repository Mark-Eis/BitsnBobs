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
#' @param object `numeric`, representing one or more coordinates of latitude or longitude in decimal degrees.
#'
#' @param \dots further arguments passed to or from other methods.
#'
#' @param .latorlon a `character` string indicating whether the coordinate represented by `object` is a latitude or
#'   longitude; must be one of `NA` (default), `"lat"`, or `"lon"`.
#'
#' @param x object to be printed.
#'
#' @return An object of class `"decdeg"`, or if `length(object) > 1`, a list of such objects, instantiating a
#'   coordinate of latitude or longitude in decimal degrees represented by a numeric of type `double` with maximum
#'   absolute value of \var{180}˚. Attribute `".latorlon"` indicates whether the object is a coordinate of latitude
#'   or longitude.
#'
#' @keywords utilities
#'
#' @export
#' @examples
#' decdeg(49.54621)
#' decdeg(c(lat = 49.54621, lon = 18.398562))
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

decdeg.default <- function(object, ..., .latorlon = c(NA, "lat", "lon")) {
    check_dots_empty()
    .latorlon <- match.arg(.latorlon)    

    rv <- lapply(object,  \(x) new_decdeg(x, .latorlon) |> validate_decdeg())
    if (length(object) > 1) rv else rv[[1]]
}

# ========================================
#  Constructor
#  new_decdeg()
#
#  not exported

new_decdeg <- function(d, .latorlon)
    structure(d, class = "decdeg", .latorlon = .latorlon)

# ========================================
#  Validator
#  validate_decdeg()
#
#  not exported

validate_decdeg <- function(dec_deg) {

    if (!inherits(dec_deg, "decdeg"))
        stop(
            "`dec_deg` must be of class `\"decdeg\"`",
            call. = FALSE
        )

    if (abs(dec_deg) > 180)
        stop(
            "`dec_deg` must not be greater than 180",
            call. = FALSE
        )

    if (!dec_deg %@% ".latorlon" %in% c(NA, "lat", "lon"))
        stop(
            "Attribute `\".latorlon\"` must be one of `NA`, `\"lat\"`, `\"lon\"`",
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
#' @param object `numeric`, representing one or more coordinates of latitude or longitude in degrees, minutes and
#'   seconds.
#'
#' @param .after a `character` string indicating the position of the decimal point in `object`; must be one of
#'   `"deg"` (default), `"min"`, or `"sec"`. You can specify just the initial letter.
#'
#' @inheritParams decdeg
#'
#' @return An object of class `"degminsec"`, or if `length(object) > 1`, a list of such objects, representing a
#'   coordinate of latitude or longitude in degrees, minutes and seconds as a named list with components: -
#'
#' \item{deg}{degrees represented by an integer with maximum absolute value of 180.}
#'
#' \item{min}{minutes represented by a positive integer with value less than 60.}
#'
#' \item{sec}{seconds represented by a positive numeric with value less than 60.}
#'
#' Attribute `"negative"` indicates whether `object` was originally a negative number i.e. if `TRUE`, the
#'   value represents a west or south rather than north or east coordinate. Attribute `".latorlon"` indicates
#'   whether the object is a coordinate of latitude or longitude.
#'
#' @keywords utilities
#'
#' @export
#' @examples
#' degminsec(49.3246368)
#' degminsec(4932.46368, .after = "min")
#' degminsec(493246.368, .after = "sec")
#'
#' degminsec(49.3246368, .latorlon = "lat")
#' degminsec(18.2354822, .latorlon = "lon")
#'
#' degminsec(-37.0642264, .latorlon = "lat")
#' degminsec(-12.1719068, .latorlon = "lon")
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

degminsec.default <- function(object, ..., .after = c("deg", "min", "sec"), .latorlon = c(NA, "lat", "lon")) {
    check_dots_empty()
    .after <- match.arg(.after)
    .latorlon <- match.arg(.latorlon)

    rv <- lapply(object, \(x) {
        negative <- x < 0
        switch(.after,
                deg = abs(x),
                min = abs(x) / 1e2L,
                sec = abs(x) / 1e4L
            ) |>
        new_degminsec(negative, .latorlon) |>
        validate_degminsec()
    })
    if (length(rv) > 1) rv else rv[[1]] 
}

# ========================================
#  Constructor
#  new_degminsec()
#
#  not exported

new_degminsec <- function(x, negative, .latorlon) {
    structure(
        list(
            deg = as.integer(x),
            min = as.integer(.up2(x)),
            sec = .up2(.up2(x))
        ),
        class = "degminsec",
        negative = negative,
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
            "`dms` must be of class `\"degminsec\"`",
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

    if (!dms %@% ".latorlon" %in% c(NA, "lat", "lon"))
        stop(
            "Attribute `\".latorlon\"` must be one of `NA`, `\"lat\"`, `\"lon\"`",
            call. = FALSE
        )    

    if (!is.logical(dms %@% "negative"))
        stop(
            "Attribute `\"negative\"` must be of type `logical`",
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
    if(is.na(x %@% ".latorlon"))
        cat(paste0("\t", .dmsstr(x), if (x %@% "negative") "(W/S)" else "(N/E)", "\n"))
    else
        cat(paste0("\t", .dmsstr(x), .sfmtx(x), "\n"))
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
#' `dms_to_decdeg()` is an S3 function that works with individual coordinates supplied as `numeric values` or as
#' [`"degminsec"`][BitsnBobs::degminsec] objects, or with lists of such coordinates.  It also accepts, latitude and
#' longitude values paired in a [`"latlon"`][BitsnBobs::latlon] object (see examples), or lists of `"latlon"`
#' paired coordinates.
#'
#' @family degreeconvert
#'
#' @param object a `numeric` or an object of class [`"degminsec"`][degminsec], representing a coordinate of latitude
#'   or longitude in degrees, minutes and seconds or a list of `"degminsec"` objects.
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
#' (coords <- latlon(c(49.3246368, 18.2354822)))
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
    dd <- (if (object %@% "negative") -dd else dd)
    decdeg(dd, .latorlon = object %@% ".latorlon")
}

# ========================================
#  Convert Degrees, Minutes and Seconds in a list to Decimal Degrees
#  S3method dms_to_decdeg.list()
#'
#' @rdname dms_to_decdeg
#' @export

dms_to_decdeg.list <- function(object, ...) {
    check_dots_empty()
    stopifnot(any(
        all(purrr::map_lgl(object, \(x) (inherits(x, "degminsec")))),
        all(purrr::map_lgl(object, \(x) (inherits(x, "latlon"))))
    ))
    lapply(object, dms_to_decdeg)
}

# ========================================
#  Convert Degrees, Minutes and Seconds in a latlon to Decimal Degrees
#  S3method dms_to_decdeg.latlon()
#'
#' @rdname dms_to_decdeg
#' @export

dms_to_decdeg.latlon <- function(object, ...) {
    lapply(object, dms_to_decdeg) |>
    new_latlon("dd") |>
    validate_latlon()
}


# ========================================
#' Convert Decimal Degrees to Degrees, Minutes and Seconds
#'
#' @description
#' Convert decimal degrees to degrees, minutes and seconds. 
#'
#' @details
#' `decdeg_to_dms()` is an S3 function that works with individual coordinates supplied as `numeric values` or as
#' [`"decdeg"`][BitsnBobs::decdeg] objects, or with lists of such coordinates.  It also accepts, latitude and
#' longitude values paired in a [`"latlon"`][BitsnBobs::latlon] object (see examples), or lists of `"latlon"`
#' paired coordinates.
#'
#' @family degreeconvert
#'
#' @param object a `numeric` or an object of class [`"decdeg"`][decdeg], representing a coordinate of latitude or
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
#' decdeg_to_dms(49.54621, .latorlon = "lat")
#'
#' (coord <- decdeg(49.54621, .latorlon = "lat"))
#' decdeg_to_dms(coord)
#'
#' (coords <- latlon_dd(c(49.54621, 18.398562)))
#' decdeg_to_dms(coords)
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

decdeg_to_dms.default <- function(object, ..., .latorlon = c(NA, "lat", "lon")) {
    stopifnot(is.numeric(object))
    check_dots_empty()
   .latorlon <- match.arg(.latorlon)    
    decdeg(object, .latorlon = .latorlon) |>
    decdeg_to_dms()
}

# ========================================
#  Convert Decimal Degrees in a "decdeg" object to Degrees, Minutes and Seconds
#  S3method decdeg_to_dms.degminsec()
#'
#' @rdname decdeg_to_dms
#' @export

decdeg_to_dms.decdeg <- function(object, ...) {
    check_dots_empty()
    negative <- object < 0
    object <- abs(object)
    sum(
        as.integer(object %/% 1),
        as.integer(((object %% 1) * 60) %/% 1) / 100,
        as.numeric((((object %% 1) * 60) %% 1) * 3) / 500
    ) |>
    new_degminsec(negative, object %@% ".latorlon") |>
    validate_degminsec()
}


# ========================================
#  Convert Decimal Degrees in a list to Degrees, Minutes and Seconds
#  S3method decdeg_to_dms.list()
#'
#' @rdname decdeg_to_dms
#' @export

decdeg_to_dms.list <- function(object, ...) {
    check_dots_empty()
    stopifnot(any(
        all(purrr::map_lgl(object, \(x) (inherits(x, "decdeg")))),
        all(purrr::map_lgl(object, \(x) (inherits(x, "latlon"))))
    ))
    lapply(object, decdeg_to_dms)
}


# ========================================
#  Convert Decimal Degrees in a latlon to Degrees, Minutes and Seconds
#  S3method decdeg_to_dms.latlon()
#'
#' @rdname decdeg_to_dms
#' @export

decdeg_to_dms.latlon <- function(object, ...) {
    lapply(object, decdeg_to_dms) |>
    new_latlon("dms") |>
    validate_latlon()
}


# ========================================
#  Get decimal multiplied by 100
#  .up2()
#
#  not exported

.up2 <- function(x) x %% 1 * 1e2L


# ========================================
#  Matrix to provide NESW suffix
#  .sfmtx()
#
#  not exported

.sfmtx <- function(dms) {
    stopifnot(inherits(dms, "degminsec"))
    matrix(
        c("N", "E", "S", "W"),
        nrow = 2,
        dimnames = list(c("lat", "lon"))
    )[dms %@% ".latorlon", as.integer(dms %@% "negative") + 1]
}


# ========================================
#' Create Latitude and Longitude Object
#'
#' @description
#' The function `latlon()` is used to create latitude and longitude objects representing paired coordinates in either
#' decimal degrees or degrees minutes and seconds.
#'
#' `latlon_dd()` is a convenience function, such that `latlon_dd(...)` is equivalent to `latlon(..., decimal = TRUE)`.
#'
#' @details
#' `latlon()` is a generic S3 function. The default method works with a numeric vector of length 2 representing a
#' coordinate of paired latitude and longitude values. The method for class `"matrix"` works with a two column
#' numeric matrix each row of which contains paired latitude and longitude values.
#'
#' @family degreeconvert
#'
#' @param object a `numeric vector`, representing a coordinate of latitude and longitude in decimal degrees or in
#'   degrees, minutes and seconds; or a two-column numeric `matrix` representing a number of such coordinates.
#'
#' @param \dots further arguments passed to or from other methods.
#'
#' @param decimal `logical`, if `TRUE` indicating whether the coordinate represented by `object` is in decimal degrees
#'   or otherwise in degrees, minutes and seconds; default `FALSE`.
#'
#' @param x object to be printed.
#'
#' @inheritParams degminsec
#'
#' @return An object of class `"latlon"` instantiating a coordinate of latitude and longitude in decimal degrees or
#'   degrees, minutes and seconds, comprising a list of either two `"decdeg"` or two `"degminsec"` objects, with
#'   attribute `"degrtype"` indicating which of these two types the object is; or if `length(object) > 1`, a list
#'   of `"latlon"` objects.
#'
#' @keywords utilities
#'
#' @export
#' @examples
#' ## Decimal degrees
#' latlon(c(49.54621, 18.398562), decimal = TRUE)
#'
#' ## Degrees minutes and seconds
#' latlon(c(49.3246368, 18.2354822))
#' latlon(c(493246.368, 182354.822), .after = "sec")
#'
#' ## Decimal degrees—convenience function
#' latlon_dd(c(49.54621, 18.398562))
#'
#' ## Two-column numeric `matrix` in decimal degrees
#' (ll_mtx <- matrix(
#'        c(51.507765, 49.54621, 48.107232, 38.889494, 0, -37.11174, -53.104781,
#'          -0.127924, 18.398562, -122.778671, -77.035242, 0, -12.28863, 73.517283),
#'         ncol = 2,
#'         dimnames = list(
#'             c("Nelson's Column", "Ostravice", "Tally Ho", "Washington Monument",
#'               "Null Island", "Tristan da Cunha", "Mawson Peak")
#'         )
#'     ))
#'
#' latlon(ll_mtx, decimal = TRUE)
#'
#' ## Two-column numeric `matrix` in degrees minutes and seconds
#' ## Deg Min Sec
#' ll_mtx[1:14] <- c(
#'     51.3027954, 49.3246368, 48.0626035, 38.5322178, 0, -37.0642264, -53.0617212,
#'     -0.0740526, 18.2354822, -122.464322, -77.0206871, 0, -12.1719068, 73.3102219
#' )
#'
#' ll_mtx
#'
#' latlon(ll_mtx)
#'
#' rm(ll_mtx)

latlon <- function(object, ...) {
    UseMethod("latlon")
}

# ========================================
#  Create Latitude and Longitude Object
#  S3method latlon.default()
#'
#' @rdname latlon
#' @export

latlon.default <- function(object, ..., decimal = FALSE, .after = c("deg", "min", "sec")) {
    setNames(object, c("lat", "lon")) |>
    imap(\(x, idx)
        if (decimal)
            decdeg(x, .latorlon = idx)
        else
            degminsec(x, .after = .after, .latorlon = idx)
    ) |>
    new_latlon(if (decimal) "dd" else "dms") |>
    validate_latlon()
}

# ========================================
#  Create Latitude and Longitude Object from Matrix
#  latlon.matrix()
#'
#' @rdname latlon
#' @export

latlon.matrix <- function(object, ..., decimal = FALSE, .after = c("deg", "min", "sec")) {
    if (dim(object)[2] != 2)
        stop(
            "`object` must be a matrix of two columns",
            call. = FALSE
        )
    lapply(seq_len(dim(object)[1]), \(x) latlon(object[x, ], decimal = decimal, .after = .after)) |>
    setNames(dimnames(object)[[1]])
}

# ========================================
#  Create Latitude and Longitude Object with Decimal Degrees
#  S3method latlon_dd()
#'
#' @rdname latlon
#' @export

latlon_dd <- function(object)
    latlon(object, decimal = TRUE)

# ========================================
#  Constructor
#  new_latlon()
#
#  not exported

new_latlon <- function(ll, degrtype) {
    structure(ll, class = "latlon", degrtype = degrtype)
}


# ========================================
#  Validator
#  validate_latlon()
#
#  not exported

validate_latlon <- function(ll) {
    if (!inherits(ll, "latlon"))
        stop(
            "`ll` must be of class `\"latlon\"`",
            call. = FALSE
        )

    if (length(ll) != 2)
        stop(
            "Length of `ll` must be 2",
            call. = FALSE
        )

    if (!identical(class(ll[[1]]), class(ll[[2]])))
        stop(
            "`ll[[1]]` and `ll[[2]]` must be of the same class",
            call. = FALSE
        )

    if (inherits(ll, "decdeg")) {
        lapply(ll, validate_decdeg)
    }

    if (inherits(ll, "degminsec")) {
        lapply(ll, validate_degminsec)
    }

    if (!ll %@% "degrtype" %in% c("dd", "dms"))
        stop(
            "Attribute `\".latorlon\"` must be either `\"dd\"` or `\"dms\"`",
            call. = FALSE
        ) 
   
    ll
}


# ========================================
# Print latlon Object
#  S3method print.latlon()
#'
#' @rdname latlon
#' @export

print.latlon <- function(x, ...) {
    switch(x %@% "degrtype",
        "dd" = cat(paste0("\t", x$lat, ", ", x$lon, " decimal degrees\n")),
        "dms" = cat(paste0("\t", .dmsstr(x$lat), .sfmtx(x$lat), ", ", .dmsstr(x$lon), .sfmtx(x$lon), "\n")),
        stop("Invalid `\"degrtype\"`")
    )
    invisible(x)
}


