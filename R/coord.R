# BitsnBobs R Package
# Mark Eisler Apr 2024
# For general bits and bobs of code
#
# Requires R version 4.2.0 (2022-04-22) -- "Vigorous Calisthenics" or later
#
# coord.R

# __________________________________
# Remove once in package BitsnBobs
.up2 <- \(...) BitsnBobs:::.up2(...)

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

format.degxdec <- function(x, ...) {
	check_dots_empty()
	c(formatC(x, digits = 6, width = 10, format = "f", flag = " "), "\u00B0")
}

format.degxint <- function(x, ...) {
	check_dots_empty()
	c(formatC(x, digits = 0, width = 3, format = "f", flag = " "), "\u00B0")
}

format.minxdec <- function(x, ...) {
	check_dots_empty()
	c(formatC(x, digits = 3, width = 6, format = "f", flag = "0"), "\'")
}

format.minxint <- function(x, ...) {
	check_dots_empty()
	c(formatC(x, digits = 0, width = 2, format = "f", flag = "0"), "\'")
}

format.secxdec <- function(x, ...) {
	check_dots_empty()
    c(formatC(x, digits = 3, width = 6, format = "f", flag = "0"), "\"")
}

format.coordpart <- function(x, ...) {
	check_dots_empty()
	cat(NextMethod(), sep = "")
}


# ____________________
# Coordinate class
# Coord class contains a list with one, two or three values named
# "deg", "min", "sec"

coord <- function(
	x,
	.degrtype = c("decdeg", "degmin", "degminsec"),
	.fmt = c("deg", "min", "sec"),
	.latorlon = c(NA, "lat", "lon")
) {
    .degrtype <- match.arg(.degrtype)
    .fmt <- match.arg(.fmt)
    .latorlon <- match.arg(.latorlon)    

	# x <- switch(.fmt,
		# deg = x,
		# min = 	switch(.degrtype,
					# degmin =,
					# degminsec = x / 1e2L,
					# stop(".fmt \"min\" only meaningful with .degrtype of \"degmin\" or \"degminsec\"", call. = FALSE)
				# ),
		# sec = switch(.degrtype,
					# "degminsec" = x / 1e4L,
					# stop(".fmt \"sec\" only meaningful with .degrtype of \"degminsec\"", call. = FALSE)
				# ),
		# stop("Invalid `.fmt` value", call. = FALSE) 
	# )

	x <- fmtdeg(x, .degrtype, .fmt)

    rv <- lapply(x, \(y) {
        negative <- y < 0
        y <- abs(y)
		switch(.degrtype,
                decdeg = list(deg = coordpart(y, "degxdec")),
                degmin = list(
							deg = coordpart(as.integer(y), "degxint"),
							min = coordpart(.up2(y), "minxdec")
						),
                degminsec = list(
							deg = coordpart(as.integer(y), "degxint"),
							min = coordpart(as.integer(.up2(y)), "minxint"),
							sec = coordpart(.up2(.up2(y)), "secxdec")
						),
                default = stop("Invalid `.degrtype` value", call. = FALSE)
            ) |>
        new_coord(.degrtype, .latorlon, negative) |>
	    validate_coord()
    })
    if (length(x) > 1) rv else rv[[1]]
}

new_coord <- function(x, degrtype, latorlon = NA, negative = FALSE) {
	structure(x, class = "coord", degrtype = degrtype, latorlon = latorlon, negative = negative)
}

validate_coord <- function(object) {

    if (!inherits(object, "coord"))
        stop(
            "`object` must be of class `\"coord\"`",
            call. = FALSE
        )

	if (with(object,
			switch(object %@% "degrtype",
				decdeg = deg,
				degmin = deg + min / 60,
				degminsec = deg + min / 60 + sec / 3600,
				stop("Invalid `degrtype` attribute", call. = FALSE)
			) > 180
		))
        stop(
            "`object` must not be greater than 180\u00B0",
            call. = FALSE
        )

	if (with(object,
			switch(object %@% "degrtype",
				decdeg = 0,
				degmin = min,
				degminsec = min + sec / 60,
				stop("Invalid `degrtype` attribute", call. = FALSE)
			) >= 60
		))
        stop(
            "`object$min` must be less than 60\'",
            call. = FALSE
        )

    if (!object %@% "latorlon" %in% c(NA, "lat", "lon"))
        stop(
            "Attribute `\"latorlon\"` must be one of `NA`, `\"lat\"`, `\"lon\"`",
            call. = FALSE
        )    

	if (with(object,
			all(object %@% "latorlon" %in% "lat",
				switch(object %@% "degrtype",
					decdeg = deg,
					degmin = deg + min / 60,
					degminsec = deg + min / 60 + sec / 3600,
					stop("Invalid `degrtype` attribute", call. = FALSE)
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

print.coord <- function(x, ...) {
	check_dots_empty()
	if (all(x %@% "degrtype" == "decdeg", x %@% "negative"))
		cat("-")
 	lapply(x, format)
	if (x %@% "degrtype" == "decdeg") {
		if (!is.na(x %@% "latorlon")) cat(" ", x %@% "latorlon", sep = "")
	} else
		cat(" ", .cmppnt(x %@% "latorlon", x %@% "negative"), sep = "")
	invisible(x)
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


as_degminsec.numeric <- function(
	object,
	...,
	.degrtype = c("decdeg", "degmin", "degminsec"),
	.fmt = c("deg", "min", "sec")
) {
	check_dots_empty()
    .degrtype <- match.arg(.degrtype)
    .fmt <- match.arg(.fmt)

	object <- fmtdeg(object, .degrtype, .fmt)

	switch(.degrtype,
		decdeg = sum(
	       object %/% 1,
	       (object %% 1 * 60) %/% 1 / 100,
	       (object %% 1 * 60) %% 1 * 3 / 500
	    ),
		degmin = sum(deg, as.integer(min) / 100, min %% 1 * 3 / 500),
		degminsec = object,
		stop("Invalid `.degrtype`", call. = FALSE)
	)
}


fmtdeg <- function(x, .degrtype = c("decdeg", "degmin", "degminsec"), .fmt = c("deg", "min", "sec")) {
    .degrtype <- match.arg(.degrtype)
    .fmt <- match.arg(.fmt)

	switch(.fmt,
		deg = x,
		min = 	switch(.degrtype,
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
